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

fn parse_method(method_string) -> Result(http.Method, Nil) {
  case method_string {
    "CONNECT" -> Ok(http.Connect)
    "DELETE" -> Ok(http.Delete)
    "GET" -> Ok(http.Get)
    "HEAD" -> Ok(http.Head)
    "OPTIONS" -> Ok(http.Options)
    "PATCH" -> Ok(http.Patch)
    "POST" -> Ok(http.Post)
    "PUT" -> Ok(http.Put)
    "TRACE" -> Ok(http.Trace)
    _ -> Error(Nil)
  }
}

// TODO reanme supervisor -> endpoint
fn read_request(socket) {
  // TODO https://erlang.org/doc/man/timer.html#exit_after-2
  let Ok(
    tuple(method_string, wire.AbsPath(raw_path), raw_headers),
  ) = wire.read_request_head(socket, [])
  let Ok(method) = parse_method(method_string)

  let Ok(
    Uri(
      scheme: None,
      userinfo: None,
      host: None,
      port: None,
      path: path,
      query: query,
      fragment: None,
    ),
  ) = uri.parse(raw_path)

  let [tuple("host", authority), ..headers] = raw_headers
  let Ok(
    Uri(
      scheme: None,
      userinfo: None,
      host: Some(host),
      port: port,
      path: "",
      query: None,
      fragment: None,
    ),
  ) = uri.parse(string.append("//", authority))

  let content_length = result.unwrap(
    list.key_find(headers, "content-length"),
    or: "0",
  )
  let Ok(content_length) = int.parse(content_length)
  let body = case content_length {
    0 -> ""
    _ -> {
      let Ok(body) = wire.read_body(socket, content_length, 5000)
      body
    }
  }
  Ok(
    http.Message(
      http.RequestHead(
        method: method,
        host: host,
        port: port,
        path: path,
        query: query,
      ),
      headers,
      iodata.from_strings([body]),
    ),
  )
}

// completely ignore reson phrases
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

// TODO needs to add content length Or does it!, use the set_body function
pub type Accept {
  Accept(From(Nil))
}

fn run(receive, handler, listen_socket) {
  let Some(Accept(from)) = receive(Infinity)
  let Ok(socket) = wire.accept(listen_socket)
  process.reply(from, Nil)
  let Ok(request) = read_request(socket)
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
