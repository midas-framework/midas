import net/tcp
import net/http
import gleam/should
import midas_utils

//
// pub external fn read_http(Socket, Int) -> Result(String, Nil) =
//   "net_http_native" "read_http"
pub fn read_http_request_known_method_test() {
  let Ok(listen_socket) = http.listen(9000)

  let Ok(socket) = tcp.connect("localhost", 9000)
  let Ok(_) = tcp.send(socket, "GET / HTTP/1.1\r\n\r\n")

  let Ok(server_socket) = http.accept(listen_socket)
  let Ok(
    tuple(method, path, headers),
  ) = http.read_request_head(server_socket, 100)
  should.equal(method, "GET")
  should.equal(path, http.AbsPath("/"))
  should.equal(headers, [])
}

// erlang decode packet doesn't handle patch
pub fn read_http_request_unknown_method_test() {
  let Ok(listen_socket) = http.listen(9001)

  let Ok(socket) = tcp.connect("localhost", 9001)
  let Ok(_) = tcp.send(socket, "PATCH / HTTP/1.1\r\n\r\n")

  let Ok(server_socket) = http.accept(listen_socket)
  let Ok(
    tuple(method, _path, _headers),
  ) = http.read_request_head(server_socket, 100)
  should.equal(method, "PATCH")
}

pub fn read_http_request_headers_test() {
  let Ok(listen_socket) = http.listen(9002)

  let Ok(socket) = tcp.connect("localhost", 9002)
  let Ok(_) = tcp.send(socket, "GET / HTTP/1.1\r\naccept: text/plain\r\nx-foo: bar\r\n\r\n")

  let Ok(server_socket) = http.accept(listen_socket)
  let Ok(
    tuple(_method, _path, headers),
  ) = http.read_request_head(server_socket, 100)
  should.equal(headers, [tuple("accept", "text/plain"), tuple("x-foo", "bar")])
}
// uri_string:parse("foo://hrllo:8080/?400").
