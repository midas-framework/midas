import gleam/list
import gleam/string
import midas_utils

pub external type ListenSocket

pub external type Socket

pub external fn listen(Int) -> Result(ListenSocket, Nil) =
  "net_http_native" "listen"

pub external fn accept(ListenSocket) -> Result(Socket, Nil) =
  "gen_tcp" "accept"

// TODO real Error
external fn recv(Socket, length: Int, timeout: Int) -> Result(p, Nil) =
  "gen_tcp" "recv"

pub external fn send(Socket, String) -> Result(Nil, Nil) =
  "net_http_native" "send"

pub type HttpURI {
  AbsPath(String)
}

external type AtomOrBinary

external fn atom_or_binary_to_string(AtomOrBinary) -> String =
  "net_http_native" "atom_or_binary_to_string"

external type Reserved

type HttpPacket {
  // method is a string because erl returns upcase atoms
  HttpRequest(AtomOrBinary, HttpURI, tuple(Int, Int))
  HttpHeader(Int, AtomOrBinary, Reserved, String)
  HttpEoh
}

fn read_packet(socket, timeout) {
  recv(socket, 0, timeout)
}

// Decode packet turns some methods/header fields into atoms
fn do_read_headers(socket, timeout, headers) {
  case read_packet(socket, timeout) {
    Ok(HttpEoh) -> Ok(list.reverse(headers))
    Ok(HttpHeader(_, name, _, value)) -> {
      let header = tuple(
          string.lowercase(atom_or_binary_to_string(name)),
          value,
        )
      let headers = [header, ..headers]
      do_read_headers(socket, timeout, headers)
    }
    _ -> Error(Nil)
  }
}

pub fn read_request_head(socket, timeout) {
  case read_packet(socket, timeout) {
    Ok(HttpRequest(method, http_uri, tuple(1, 1))) -> {
      let method = atom_or_binary_to_string(method)
      case do_read_headers(socket, timeout, []) {
        Ok(headers) -> Ok(tuple(method, http_uri, headers))
        _ -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

pub external fn read_body(
  socket,
  content_length,
  timeout,
) -> Result(String, Nil) =
  "net_http_native" "read_body"
