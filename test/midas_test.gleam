import gleam/io
import gleam/uri
import midas
import process/process
import gleam/http.{Request, Response, Get}
import midas/net/tcp
import midas/net/http as net_http
import gleam/should

fn handle_request(request) {
  let http.Message(body: body, ..) = request
  case http.path_segments(request) {
    ["echo"] -> {
      let Ok(content_type) = http.get_header(request, "content-type")
      http.Message(
        http.ResponseHead(200),
        [tuple("content-type", content_type)],
        body,
      )
    }
  }
}

pub fn echo_body_test() {
  assert Ok(listen_socket) = net_http.listen(0)
  let Ok(port) = net_http.port(listen_socket)
  let Ok(socket) = tcp.connect("localhost", port)
  let endpoint_pid = midas.spawn_link(handle_request, listen_socket)

  let Ok(socket) = tcp.connect("localhost", port)
  let message = "GET /echo HTTP/1.1\r\nhost: midas.test\r\ncontent-length: 14\r\ncontent-type: text/unusual\r\n\r\nHello, Server!"
  let Ok(_) = tcp.send(socket, message)
  let Ok(response) = tcp.read_blob(socket, 0, 100)
  should.equal(
    response,
    "HTTP/1.1 200 \r\nconnection: close\r\ncontent-type: text/unusual\r\n\r\nHello, Server!",
  )

  let Ok(socket) = tcp.connect("localhost", port)
  let message = "GET /echo HTTP/1.1\r\nhost: midas.test\r\nconnection: close\r\nhost: midas.test\r\ncontent-type: text/unusual\r\n\r\n"
  let Ok(_) = tcp.send(socket, message)
  let Ok(port) = net_http.port(listen_socket)
  let Ok(response) = tcp.read_blob(socket, 0, 100)
  should.equal(
    response,
    "HTTP/1.1 200 \r\nconnection: close\r\ncontent-type: text/unusual\r\n\r\n",
  )
}
