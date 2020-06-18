import gleam/atom.{Atom}
import gleam/io
import gleam/list
import gleam/string
import process/process
import gleam/http as gleam_http
import midas/net/tcp
import midas/net/http
import gleam/should

pub fn read_request_from_single_packet_test() {
  assert Ok(listen_socket) = http.listen(0)
  assert Ok(port) = http.port(listen_socket)

  assert Ok(socket) = tcp.connect("localhost", port)
  let message = "GET / HTTP/1.1\r\naccept: text/plain\r\nx-foo: bar\r\n\r\n"
  assert Ok(Nil) = tcp.send(socket, message)

  assert Ok(server_socket) = http.accept(listen_socket)
  let Ok(
    tuple(method, path, headers),
  ) = http.read_request_head(server_socket, [])
  // TODO make this atom
  should.equal(method, gleam_http.Get)
  should.equal(path, http.AbsPath("/"))
  should.equal(headers, [tuple("accept", "text/plain"), tuple("x-foo", "bar")])
}

// TODO duplicate uppercase headers handled after net
// should.equal(headers, [tuple("accept", "text/plain"), tuple("x-UPPERCASE", "BAR"),tuple("x-duplicate", "1"), tuple("x-duplicte", "2")])
pub fn read_request_from_multiple_packets_test() {
  assert Ok(listen_socket) = http.listen(0)
  assert Ok(port) = http.port(listen_socket)

  process.spawn_link(
    fn(_receive) {
      assert Ok(socket) = tcp.connect("localhost", port)

      let parts = [
          "GET / HT",
          "TP/1.1\r",
          "\n",
          "accept: text/plain",
          "\r\nx-fo",
          "o: bar\r\n\r\n",
        ]
      list.map(
        parts,
        fn(part) {
          assert Ok(Nil) = tcp.send(socket, part)
          process.sleep(100)
        },
      )
    },
  )

  assert Ok(server_socket) = http.accept(listen_socket)
  let Ok(
    tuple(method, path, headers),
  ) = http.read_request_head(server_socket, [])
  // TODO make this atom
  should.equal(method, gleam_http.Get)
  should.equal(path, http.AbsPath("/"))
  should.equal(headers, [tuple("accept", "text/plain"), tuple("x-foo", "bar")])
}

pub fn read_request_starting_with_empty_lines_test() {
  assert Ok(listen_socket) = http.listen(0)
  assert Ok(port) = http.port(listen_socket)

  assert Ok(socket) = tcp.connect("localhost", port)
  let message = "\r\n\r\nGET / HTTP/1.1\r\naccept: text/plain\r\nx-foo: bar\r\n\r\n"
  assert Ok(Nil) = tcp.send(socket, message)

  assert Ok(server_socket) = http.accept(listen_socket)
  let Ok(
    tuple(method, path, headers),
  ) = http.read_request_head(server_socket, [])
  should.equal(method, gleam_http.Get)
  should.equal(path, http.AbsPath("/"))
  should.equal(headers, [tuple("accept", "text/plain"), tuple("x-foo", "bar")])
}

// erlang decode packet doesn't handle patch
pub fn read_http_request_unknown_method_test() {
  assert Ok(listen_socket) = http.listen(0)
  assert Ok(port) = http.port(listen_socket)

  let Ok(socket) = tcp.connect("localhost", port)
  let Ok(_) = tcp.send(socket, "PATCH / HTTP/1.1\r\n\r\n")

  let Ok(server_socket) = http.accept(listen_socket)
  let Ok(
    tuple(method, _path, _headers),
  ) = http.read_request_head(server_socket, [])
  should.equal(method, gleam_http.Patch)
}

pub fn invalid_start_line_test() {
  assert Ok(listen_socket) = http.listen(0)
  assert Ok(port) = http.port(listen_socket)

  let Ok(socket) = tcp.connect("localhost", port)
  let Ok(_) = tcp.send(socket, "NOT-HTTP\r\nrest\r\n")

  let Ok(server_socket) = http.accept(listen_socket)
  http.read_request_head(server_socket, [])
  |> should.equal(Error(http.InvalidStartLine("NOT-HTTP\r\n")))
}

pub fn double_start_line_test() {
  assert Ok(listen_socket) = http.listen(0)
  assert Ok(port) = http.port(listen_socket)

  let Ok(socket) = tcp.connect("localhost", port)
  let Ok(
    _,
  ) = tcp.send(socket, "GET / HTTP/1.1\r\nGET / HTTP/1.1\r\nfoo: bar\r\n")

  let Ok(server_socket) = http.accept(listen_socket)
  http.read_request_head(server_socket, [])
  |> should.equal(Error(http.InvalidHeaderLine("GET / HTTP/1.1\r\n")))
}

pub fn start_line_to_long_test() {
  assert Ok(listen_socket) = http.listen(0)
  assert Ok(port) = http.port(listen_socket)

  let Ok(socket) = tcp.connect("localhost", port)
  let Ok(_) = tcp.send(socket, string.append("GET /", string.repeat("a", 3000)))

  let Ok(server_socket) = http.accept(listen_socket)
  http.read_request_head(server_socket, [])
  |> should.equal(Error(http.InetError(atom.create_from_string("emsgsize"))))
}

pub fn invalid_header_line_test() {
  assert Ok(listen_socket) = http.listen(0)
  assert Ok(port) = http.port(listen_socket)

  let Ok(socket) = tcp.connect("localhost", port)
  let Ok(_) = tcp.send(socket, "GET / HTTP/1.1\r\na \r\n::\r\n    ")

  let Ok(server_socket) = http.accept(listen_socket)
  http.read_request_head(server_socket, [])
  |> should.equal(Error(http.InvalidHeaderLine("a \r\n")))
}

pub fn header_line_to_long_test() {
  assert Ok(listen_socket) = http.listen(0)
  assert Ok(port) = http.port(listen_socket)

  let Ok(socket) = tcp.connect("localhost", port)
  let Ok(
    _,
  ) = tcp.send(
    socket,
    string.append("GET / HTTP/1.1\r\nfoo: ", string.repeat("a", 3000)),
  )

  let Ok(server_socket) = http.accept(listen_socket)
  http.read_request_head(server_socket, [])
  |> should.equal(Error(http.InetError(atom.create_from_string("emsgsize"))))
}

pub fn read_from_already_closed_socket_test() {
  assert Ok(listen_socket) = http.listen(0)
  assert Ok(port) = http.port(listen_socket)

  let Ok(socket) = tcp.connect("localhost", port)
  let Nil = tcp.close(socket)

  let Ok(server_socket) = http.accept(listen_socket)
  http.read_request_head(server_socket, [])
  |> should.equal(Error(http.Closed))
}

pub fn read_from_a_socket_that_closes_test() {
  assert Ok(listen_socket) = http.listen(0)
  assert Ok(port) = http.port(listen_socket)

  process.spawn_link(
    fn(_receive) {
      let Ok(socket) = tcp.connect("localhost", port)
      let Ok(_) = tcp.send(socket, "GET / HTTP/1.1\r\nfoo")
      process.sleep(200)
      tcp.close(socket)
    },
  )

  let Ok(server_socket) = http.accept(listen_socket)
  http.read_request_head(server_socket, [])
  |> should.equal(Error(http.Closed))
}

pub fn timeout_from_slow_start_line_test() {
  assert Ok(listen_socket) = http.listen(0)
  assert Ok(port) = http.port(listen_socket)

  process.spawn_link(
    fn(_receive) {
      let Ok(socket) = tcp.connect("localhost", port)
      let Ok(_) = tcp.send(socket, "\r\n")
      process.sleep(100)
      let Ok(_) = tcp.send(socket, "\r\n")
      process.sleep(100)
      let Ok(_) = tcp.send(socket, "\r\n")
      process.sleep(100)
    },
  )

  let Ok(server_socket) = http.accept(listen_socket)
  http.read_request_head(server_socket, [http.CompletionTimeout(200)])
  |> should.equal(Error(http.Timeout))
}

pub fn timeout_from_slow_request_test() {
  assert Ok(listen_socket) = http.listen(0)
  assert Ok(port) = http.port(listen_socket)

  process.spawn_link(
    fn(_receive) {
      let Ok(socket) = tcp.connect("localhost", port)
      let Ok(_) = tcp.send(socket, "GET / HTTP/1.1\r\n")
      process.sleep(100)
      let Ok(_) = tcp.send(socket, "foo: bar\r\n")
      process.sleep(100)
      let Ok(_) = tcp.send(socket, "foo: bar\r\n")
      process.sleep(100)
      let Ok(_) = tcp.send(socket, "foo: bar\r\n")
      process.sleep(100)
    },
  )

  let Ok(server_socket) = http.accept(listen_socket)
  http.read_request_head(server_socket, [http.CompletionTimeout(300)])
  |> should.equal(Error(http.Timeout))
}
// TODO always send connection close, not a test here
