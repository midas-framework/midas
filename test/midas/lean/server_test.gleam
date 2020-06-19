import gleam/atom
import gleam/dynamic
import gleam/io
import gleam/result
import gleam/http
import process/process
import midas/lean/server
import midas/net/http as wire
import midas/net/tcp
import gleam/should

pub external fn unsafe_receive(process.Wait) -> Result(m, Nil) =
  "process_native" "do_receive"

pub fn empty_response_test() {
  assert Ok(listen_socket) = wire.listen(0)
  assert Ok(port) = wire.port(listen_socket)

  let pid = server.spawn_link(
    fn(_) {
      http.response(200)
      |> http.set_body("")
    },
    listen_socket,
  )

  assert Ok(socket) = tcp.connect("localhost", port)
  let message = "GET / HTTP/1.1\r\nhost: example.test\r\n\r\n"
  assert Ok(Nil) = tcp.send(socket, message)
  assert Ok(body) = tcp.read_blob(socket, 0, 5000)
  should.equal(
    body,
    "HTTP/1.1 200 \r\nconnection: close\r\ncontent-length: 0\r\n\r\n",
  )
}

pub fn response_with_body_test() {
  assert Ok(listen_socket) = wire.listen(0)
  assert Ok(port) = wire.port(listen_socket)

  let pid = server.spawn_link(
    fn(_) {
      http.response(200)
      |> http.set_body("Hello, You!")
    },
    listen_socket,
  )

  assert Ok(socket) = tcp.connect("localhost", port)
  let message = "GET / HTTP/1.1\r\nhost: example.test\r\n\r\n"
  assert Ok(Nil) = tcp.send(socket, message)
  assert Ok(body) = tcp.read_blob(socket, 0, 5000)
  should.equal(
    body,
    "HTTP/1.1 200 \r\nconnection: close\r\ncontent-length: 11\r\n\r\nHello, You!",
  )
}

pub fn client_closes_connection_test() {
  assert Ok(listen_socket) = wire.listen(0)
  assert Ok(port) = wire.port(listen_socket)

  let pid = server.spawn_link(
    fn(_) {
      http.response(200)
      |> http.set_body("")
    },
    listen_socket,
  )
  let reference = process.monitor(pid)

  assert Ok(socket) = tcp.connect("localhost", port)
  let message = "GET / HTTP/1.1\r\nhos"
  assert Ok(Nil) = tcp.send(socket, message)
  let Nil = tcp.close(socket)
  let Ok(message) = unsafe_receive(process.Infinity)

  message
  |> dynamic.element(0)
  |> result.then(dynamic.atom)
  |> should.equal(Ok(atom.create_from_string("down")))

  message
  |> dynamic.element(1)
  |> should.equal(Ok(dynamic.from(reference)))

  message
  |> dynamic.element(4)
  |> result.then(dynamic.atom)
  |> should.equal(Ok(atom.create_from_string("normal")))
}

pub fn client_doesnt_wait_for_response_test() {
  assert Ok(listen_socket) = wire.listen(0)
  assert Ok(port) = wire.port(listen_socket)

  let pid = server.spawn_link(
    fn(_) {
      process.sleep(200)
      http.response(200)
      |> http.set_body("")
    },
    listen_socket,
  )
  let reference = process.monitor(pid)

  assert Ok(socket) = tcp.connect("localhost", port)
  let message = "GET / HTTP/1.1\r\nhost: example.test\r\n\r\n"
  assert Ok(Nil) = tcp.send(socket, message)
  let Nil = tcp.close(socket)
  let Ok(message) = unsafe_receive(process.Infinity)

  message
  |> dynamic.element(0)
  |> result.then(dynamic.atom)
  |> should.equal(Ok(atom.create_from_string("down")))

  message
  |> dynamic.element(1)
  |> should.equal(Ok(dynamic.from(reference)))

  message
  |> dynamic.element(4)
  |> result.then(dynamic.atom)
  |> should.equal(Ok(atom.create_from_string("normal")))
}
