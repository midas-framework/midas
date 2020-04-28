import gleam/iodata
import gleam/list
import gleam/int
import gleam/result
import process/process
import process/process.{From, Pid, BarePid, ExitReason, Normal, Kill, Infinity, Milliseconds, TrapExit}
import net/tcp
import midas_utils
import midas/http
import midas/headers as h_utils
import midas/request.{Request}
// import midas/response
import midas/response.{Response, to_string}

fn read_headers(socket, headers) {
  let Ok(line) = tcp.read_line(socket, 5000)
  case http.parse_header_line(line) {
    Ok(http.Header(header)) -> read_headers(socket, [header, ..headers])
    Ok(http.EndOfHeaders) -> Ok(list.reverse(headers))
  }
}

fn read_request(socket) {
    // https://erlang.org/doc/man/timer.html#exit_after-2
  let Ok(line) = tcp.read_line(socket, 5000)
  let Ok(tuple(method, path)) = http.parse_request_line(line)
  let Ok(headers) = read_headers(socket, [])
  let Ok(authority) = h_utils.find(headers, "host")
  let content_length = result.unwrap(
    h_utils.find(headers, "content-length"),
    or: "0",
  )
  let Ok(content_length) = int.parse(content_length)
  let body = case content_length {
    0 -> ""
    _ -> {
      let Ok(body) = tcp.read_blob(socket, content_length, 5000)
      body
    }
  }
  Ok(
    Request(
      method: method,
      authority: authority,
      headers: headers,
      path: path,
      body: body,
    ),
  )
}

pub type Accept {
  Accept(From(Nil))
}

fn run(receive, handler, listen_socket) {
  let Ok(Accept(from)) = receive(Infinity)
  let Ok(socket) = tcp.accept(listen_socket)
  process.reply(from, Nil)
  let Ok(request) = read_request(socket)
  let response = handler(request)
  let Ok(Nil) = tcp.send(socket, to_string(response))
  Normal
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
