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
  Accept(From(Nil))
}

fn run(receive, handler, listen_socket) {
  assert Some(Accept(from)) = receive(Infinity)
  assert Ok(socket) = wire.accept(listen_socket)
  process.reply(from, Nil)
  let request = wire.read_request(socket, [])
  try request = request
  let response = handler(request)
  wire.send(socket, response_to_string(response))
}

pub fn spawn_link(handler, listen_socket) {
  process.spawn_link(run(_, handler, listen_socket))
}

pub fn accept(pid) {
  process.call(pid, Accept(_), Infinity)
}
