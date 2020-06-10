import gleam/option.{Some, None}
import gleam/uri
import midas
import gleam/http
import gleam/http.{Request, Response, Get}
import midas/net/tcp
import gleam/should

fn handle_request(request) {
  let http.Message(body: body, ..) = request
  case http.path_segments(request) {
    ["echo"] -> {
      let Some(content_type) = http.get_header(request, "content-type")
      http.Message(
        http.ResponseHead(200),
        [tuple("content-type", content_type)],
        body,
      )
    }
  }
}

pub fn echo_body_test() {
  midas.spawn_link(handle_request, 10001)

  let Ok(socket) = tcp.connect("localhost", 10001)
  let Ok(
    _,
  ) = tcp.send(
    socket,
    "GET /echo HTTP/1.1\r\nhost: midas.test\r\ncontent-length: 14\r\ncontent-type: text/unusual\r\n\r\nHello, Server!",
  )
  let Ok(response) = tcp.read_blob(socket, 0, 100)
  should.equal(
    response,
    "HTTP/1.1 200 \r\ncontent-type: text/unusual\r\n\r\nHello, Server!",
  )

  let Ok(socket) = tcp.connect("localhost", 10001)
  let Ok(
    _,
  ) = tcp.send(
    socket,
    "GET /echo HTTP/1.1\r\nhost: midas.test\r\ncontent-type: text/unusual\r\n\r\n",
  )
  let Ok(response) = tcp.read_blob(socket, 0, 100)
  should.equal(response, "HTTP/1.1 200 \r\ncontent-type: text/unusual\r\n\r\n")
}
