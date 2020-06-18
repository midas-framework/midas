import gleam/atom
import gleam/list
import gleam/int
import gleam/io
import gleam/iodata
import gleam/option.{Some, None}
import gleam/result
import gleam/string
import gleam/uri.{Uri}
import gleam/http
import process/process
import process/process.{From, Pid, BarePid, ExitReason, Infinity, Milliseconds, TrapExit}
import midas/net/http as wire

fn response_to_string(response) {
  let http.Message(
    head: http.ResponseHead(status),
    headers: headers,
    body: body,
  ) = response
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
  let response = string.concat([response_head, "\r\n", iodata.to_string(body)])
  response
}

pub type Accept {
  Accept(From(Bool))
}

fn run(receive, handler, listen_socket) {
  let closed_atom = atom.create_from_string("closed")
  assert Some(Accept(from)) = receive(Infinity)
  case wire.accept(listen_socket) {
    Ok(socket) -> {
      let Nil = process.reply(from, True)
      case wire.read_request(socket, []) {
        Ok(request) -> {
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
      let Nil = process.reply(from, False)
      Nil
    }
  }
}

pub fn spawn_link(handler, listen_socket) {
  process.spawn_link(run(_, handler, listen_socket))
}
