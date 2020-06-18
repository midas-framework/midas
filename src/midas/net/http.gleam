import gleam/atom.{Atom}
import gleam/dynamic.{Dynamic}
import gleam/int
import gleam/io
import gleam/iodata
import gleam/list
import gleam/option.{Option, Some, None}
import gleam/result
import gleam/string
import gleam/http

// This is a listen socket of assumed type httpbin
pub external type ListenSocket

pub external type Socket

external type Reserved

type HttpPacket {
  // method is a string because erl returns upcase atoms
  HttpRequest(Dynamic, HttpURI, tuple(Int, Int))
  HttpHeader(Int, Dynamic, Reserved, String)
  HttpEoh
  HttpError(String)
}

pub external fn listen(Int) -> Result(ListenSocket, Nil) =
  "net_http_native" "listen"

pub external fn port(ListenSocket) -> Result(Int, Dynamic) =
  "inet" "port"

pub external fn accept(ListenSocket) -> Result(Socket, Atom) =
  "gen_tcp" "accept"

external fn erl_close(Socket) -> Atom =
  "gen_tcp" "close"

pub fn close(socket: Socket) -> Nil {
  erl_close(socket)
  Nil
}

external fn erl_recv(
  socket: Socket,
  length: Int,
  timeout: Int,
) -> Result(HttpPacket, Atom) =
  "gen_tcp" "recv"

// pub type RecvError{
//     Timeout
//     Closed
//     InetError(Atom)
// }
// It's ok that recv uses this because it's private
pub type NetError {
  InvalidStartLine(String)
  InvalidHeaderLine(String)
  MissingHostHeader
  InvalidHostHeader
  ContentLengthTooLarge
  Closed
  Timeout
  InetError(Atom)
}

// https://erlang.org/doc/man/gen_tcp.html#recv-3
// Argument Length is only meaningful when the socket is in raw mode and denotes the number of bytes to read.
// If Length is 0, all available bytes are returned.
// If Length > 0, exactly Length bytes are returned, or an error;
// possibly discarding less than Length bytes of data when the socket is closed from the other side.
fn recv(socket: Socket, timeout: Int) {
  let timeout_atom = atom.create_from_string("timeout")
  let closed_atom = atom.create_from_string("closed")
  case erl_recv(socket, 0, timeout) {
    Ok(value) -> Ok(value)
    Error(reason) if reason == timeout_atom -> Error(Timeout)
    Error(reason) if reason == closed_atom -> Error(Closed)
    Error(reason) -> Error(InetError(reason))
  }
}

pub external fn send(Socket, String) -> Result(Nil, NetError) =
  "net_http_native" "send"

type HttpURI {
  AbsPath(String)
}

external fn monotonic_time(Atom) -> Int =
  "erlang" "monotonic_time"

fn now() {
  monotonic_time(atom.create_from_string("millisecond"))
}

fn check_deadline(deadline) {
  case now() <= deadline {
    True -> Ok(Nil)
    False -> Error(Timeout)
  }
}

// Decode packet turns some methods/header fields into atoms
fn do_read_headers(socket, line_timeout, head_by, headers) {
  try _ = check_deadline(head_by)
  case recv(socket, line_timeout) {
    Ok(HttpEoh) -> Ok(list.reverse(headers))
    Ok(HttpHeader(_, name, _, value)) -> {
      let name = case dynamic.atom(name) {
        Ok(name) -> atom.to_string(name)
        Error(_) -> {
          assert Ok(name) = dynamic.string(name)
          name
        }
      }
      // NOTE erlang decode packet will uppercase even if sent lowercase
      let header = tuple(string.lowercase(name), value)
      let headers = [header, ..headers]
      do_read_headers(socket, line_timeout, head_by, headers)
    }
    Ok(HttpError(line)) -> Error(InvalidHeaderLine(line))
    Error(x) -> Error(x)
  }
}

pub type ReadOptions {
  IdleTimeout(Int)
  HeadTimeout(Int)
  BodyTimeout(Int)
  MaximumBodyLength(Int)
}

fn do_read_request_head(socket, line_timeout, head_by) {
  let options_atom = atom.create_from_string("OPTIONS")
  let get_atom = atom.create_from_string("GET")
  let head_atom = atom.create_from_string("HEAD")
  let post_atom = atom.create_from_string("POST")
  let put_atom = atom.create_from_string("PUT")
  let delete_atom = atom.create_from_string("DELETE")
  let trace_atom = atom.create_from_string("TRACE")
  try _ = check_deadline(head_by)
  case recv(socket, line_timeout) {
    Ok(HttpRequest(method, http_uri, tuple(1, 1))) -> {
      let method = case dynamic.atom(method) {
        Ok(m) if m == options_atom -> http.Options
        Ok(m) if m == get_atom -> http.Get
        Ok(m) if m == head_atom -> http.Head
        Ok(m) if m == post_atom -> http.Post
        Ok(m) if m == put_atom -> http.Put
        Ok(m) if m == delete_atom -> http.Delete
        Ok(m) if m == trace_atom -> http.Trace
        Error(_) -> case dynamic.string(method) {
          Ok("PATCH") -> http.Patch
          Ok(other) -> http.Other(other)
        }
      }
      case do_read_headers(socket, line_timeout, head_by, []) {
        Ok(headers) -> Ok(tuple(method, http_uri, headers))
        Error(x) -> Error(x)
      }
    }
    Ok(HttpError(line)) -> case string.trim(line) == "" {
      True -> do_read_request_head(socket, line_timeout, head_by)
      False -> Error(InvalidStartLine(line))
    }
    Error(x) -> Error(x)
  }
}

pub external fn binary_to_list(String) -> List(Int) =
  "erlang" "binary_to_list"

fn charachter_unreserved(char) {
  case char {
    // a-z
    c if 97 <= c && c <= 122 -> True
    // A-Z
    c if 65 <= c && c <= 90 -> True
    // 0-9
    c if 48 <= c && c <= 57 -> True
    // -
    c if c == 45 -> True
    // .
    c if c == 46 -> True
    // _
    c if c == 95 -> True
    // ~
    c if c == 126 -> True
    _ -> False
  }
}

// FIXME add support for IP6 addresses
// https://github.com/ninenines/cowlib/blob/master/src/cow_http_hd.erl#L2097
pub fn parse_host(raw: String) -> Result(tuple(String, Option(Int)), Nil) {
  try tuple(host, port) = case string.split_once(raw, ":") {
    Ok(tuple(host, port)) -> case int.parse(port) {
      Ok(port) -> Ok(tuple(host, Some(port)))
      Error(Nil) -> Error(Nil)
    }
    Error(Nil) -> Ok(tuple(raw, None))
  }
  case list.all(binary_to_list(host), charachter_unreserved) {
    True -> Ok(tuple(string.lowercase(host), port))
    False -> Error(Nil)
  }
}

pub fn read_request_head(socket, options) {
  let line_timeout = list.find_map(
      options,
      fn(option) {
        case option {
          IdleTimeout(line_timeout) -> Ok(line_timeout)
          _ -> Error(Nil)
        }
      },
    )
    |> result.unwrap(5000)
  let head_timeout = list.find_map(
      options,
      fn(option) {
        case option {
          HeadTimeout(head_timeout) -> Ok(head_timeout)
          _ -> Error(Nil)
        }
      },
    )
    |> result.unwrap(30000)

  let head_by = now() + head_timeout

  try raw = do_read_request_head(socket, line_timeout, head_by)

  // parse host
  case raw {
    tuple(method, AbsPath(path), raw_headers) -> case raw_headers {
      [tuple("host", host), ..headers] -> {
        try tuple(
          host,
          port,
        ) = parse_host(host)
          |> result.map_error(fn(_) { InvalidHostHeader })
        let tuple(path, query) = case string.split_once(path, "?") {
          Ok(tuple(path, query)) -> tuple(path, Some(query))
          Error(Nil) -> tuple(path, None)
        }
        let request_head = http.RequestHead(
          method: method,
          host: host,
          port: port,
          path: path,
          query: query,
        )
        Ok(tuple(request_head, headers))
      }
      _ -> Error(MissingHostHeader)
    }
  }
}

external fn erl_read_body(
  socket,
  content_length,
  timeout,
) -> Result(String, Atom) =
  "net_http_native" "read_body"

pub fn read_body(socket, content_length, timeout) {
  let timeout_atom = atom.create_from_string("timeout")
  let closed_atom = atom.create_from_string("closed")
  case erl_read_body(socket, content_length, timeout) {
    Ok(value) -> Ok(value)
    Error(reason) if reason == timeout_atom -> Error(Timeout)
    Error(reason) if reason == closed_atom -> Error(Closed)
    Error(reason) -> Error(InetError(reason))
  }
}

// Note this does not handle transfer encoding chunked.
pub fn read_request(socket, options) {
  try tuple(request_head, headers) = read_request_head(socket, options)
  let content_length = result.unwrap(
    list.key_find(headers, "content-length"),
    or: "0",
  )
  let Ok(content_length) = int.parse(content_length)
  let maximum_body_length = list.find_map(
      options,
      fn(option) {
        case option {
          MaximumBodyLength(maximum_body_length) -> Ok(maximum_body_length)
          _ -> Error(Nil)
        }
      },
    )
    |> result.unwrap(30000)
  try body = case content_length {
    0 -> Ok("")
    length if length <= maximum_body_length -> {
      let body_timeout = list.find_map(
          options,
          fn(option) {
            case option {
              BodyTimeout(body_timeout) -> Ok(body_timeout)
              _ -> Error(Nil)
            }
          },
        )
        |> result.unwrap(30000)
      read_body(socket, content_length, body_timeout)
    }
    _ -> Error(ContentLengthTooLarge)
  }
  Ok(http.Message(request_head, headers, iodata.from_strings([body])))
}
