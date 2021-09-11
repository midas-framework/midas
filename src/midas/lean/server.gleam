import gleam/atom
import gleam/list
import gleam/int
import gleam/io
import gleam/string_builder
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import gleam/uri.{Uri}
import gleam/http
import process/process
import process/process.{
  BarePid, ExitReason, From, Infinity, Milliseconds, Pid, TrapExit
}
import midas/net/http as wire

// TODO remove
// message body = the encoded gzip etc version of the payload body
pub type Body {
    Empty
    Prepared(content_type: String, payload: BitBuilder)
    Streams
}

fn response_to_string(response) {
  // let http.Message(
  //   head: http.ResponseHead(status),
  //   headers: headers,
  //   body: body,
  // ) = response
  let status = http.status(response)
  let headers = http.get_headers(response)
  let body = http.get_body(response)
  let headers = [tuple("connection", "close"), ..headers]
  let status_line = string.concat(["HTTP/1.1 ", int.to_string(status), " \r\n"])
  let response_head = list.fold(
    headers,
    status_line,
    fn(header, buffer) {
      let tuple(name, value) = header
      string.concat([buffer, name, ": ", value, "\r\n"])
    },
  )
  let response = string.concat(
    [response_head, "\r\n", body],
  )
  response
}

fn run(_receive, handler, listen_socket) {
  let closed_atom = atom.create_from_string("closed")
  case wire.accept(listen_socket) {
    Ok(socket) -> {
      io.debug("LEAN: accepted connection")
      case wire.read_request(socket, []) {
        Ok(request) -> {
          io.debug("LEAN: read request")
          let response = handler(request)
          case wire.send(socket, response_to_string(response)) {
            Ok(Nil) -> Nil
            Error(reason) -> {
              io.debug(reason)
              Nil
            }
          }
        }
        Error(reason) -> {
          io.debug(reason)
          Nil
        }
      }
    }
    Error(reason) if reason == closed_atom -> {
      io.debug(reason)
      Nil
    }
  }
}

pub fn spawn_link(handler, listen_socket) {
  process.spawn_link(run(_, handler, listen_socket))
}
