import gleam/list
import gleam/int
import gleam/iodata
import gleam/option.{Some, None}
import gleam/result
import gleam/string
import gleam/uri.{Uri}
import gleam/http
import process/process
import process/process.{From, Pid, BarePid, ExitReason, Infinity, Milliseconds, TrapExit}
import midas/net/http as wire

// TODO reanme supervisor -> endpoint
fn response_to_string(response) {
  let http.Message(
    head: http.ResponseHead(status),
    headers: headers,
    body: body,
  ) = response
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
  Accept(From(Nil))
}

fn run(receive, handler, listen_socket) {
  let Some(Accept(from)) = receive(Infinity)
  let Ok(socket) = wire.accept(listen_socket)
  process.reply(from, Nil)
  let Ok(request) = wire.read_request(socket, [])
  let response = handler(request)
  let Ok(Nil) = wire.send(socket, response_to_string(response))
  Nil
}

pub fn spawn_link(handler, listen_socket) {
  let pid = process.spawn_link(
    fn(receive) { run(receive, handler, listen_socket) },
  )
  pid
}

pub fn accept(pid) {
  process.call(pid, Accept(_), Infinity)
}
