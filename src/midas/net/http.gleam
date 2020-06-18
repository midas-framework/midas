import gleam/atom.{Atom}
import gleam/dynamic.{Dynamic}
import gleam/io
import gleam/list
import gleam/result
import gleam/string

// This is a listen socket of assumed type httpbin
pub external type ListenSocket

pub external type Socket

external type Reserved

type HttpPacket {
  // method is a string because erl returns upcase atoms
  HttpRequest(AtomOrBinary, HttpURI, tuple(Int, Int))
  HttpHeader(Int, AtomOrBinary, Reserved, String)
  HttpEoh
  HttpError(String)
}

pub external fn listen(Int) -> Result(ListenSocket, Nil) =
  "net_http_native" "listen"

pub external fn port(ListenSocket) -> Result(Int, Dynamic) =
  "inet" "port"

pub external fn accept(ListenSocket) -> Result(Socket, Nil) =
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
pub type ReadError {
  InvalidStartLine(String)
  InvalidHeaderLine(String)
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

pub external fn send(Socket, String) -> Result(Nil, Nil) =
  "net_http_native" "send"

pub type HttpURI {
  AbsPath(String)
}

external type AtomOrBinary

external fn atom_or_binary_to_string(AtomOrBinary) -> String =
  "net_http_native" "atom_or_binary_to_string"

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
fn do_read_headers(socket, line_timeout, complete_by, headers) {
  try _ = check_deadline(complete_by)
  case recv(socket, line_timeout) {
    Ok(HttpEoh) -> Ok(list.reverse(headers))
    Ok(HttpHeader(_, name, _, value)) -> {
      let header = tuple(
          string.lowercase(atom_or_binary_to_string(name)),
          value,
        )
      let headers = [header, ..headers]
      do_read_headers(socket, line_timeout, complete_by, headers)
    }
    Ok(HttpError(line)) -> Error(InvalidHeaderLine(line))
    Error(x) -> Error(x)
  }
}

pub type ReadOptions {
  LineTimeout(Int)
  CompletionTimeout(Int)
}

fn do_read_request_head(socket, line_timeout, complete_by) {
  try _ = check_deadline(complete_by)
  case recv(socket, line_timeout) {
    Ok(HttpRequest(method, http_uri, tuple(1, 1))) -> {
      let method = atom_or_binary_to_string(method)
      case do_read_headers(socket, line_timeout, complete_by, []) {
        Ok(headers) -> Ok(tuple(method, http_uri, headers))
        Error(x) -> Error(x)
      }
    }
    Ok(HttpError(line)) -> case string.trim(line) == "" {
      True -> do_read_request_head(socket, line_timeout, complete_by)
      False -> Error(InvalidStartLine(line))
    }
    Error(x) -> Error(x)
  }
}

pub fn read_request_head(socket, options) {
  let line_timeout = list.find_map(
      options,
      fn(option) {
        case option {
          LineTimeout(line_timeout) -> Ok(line_timeout)
          _ -> Error(Nil)
        }
      },
    )
    |> result.unwrap(5000)
  let completion_timeout = list.find_map(
      options,
      fn(option) {
        case option {
          CompletionTimeout(completion_timeout) -> Ok(completion_timeout)
          _ -> Error(Nil)
        }
      },
    )
    |> result.unwrap(30000)
  let complete_by = now() + completion_timeout

  do_read_request_head(socket, line_timeout, complete_by)
}

pub external fn read_body(
  socket,
  content_length,
  timeout,
) -> Result(String, Nil) =
  "net_http_native" "read_body"
