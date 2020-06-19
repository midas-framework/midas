import gleam/iodata.{Iodata}
import gleam/http.{Request, Response}
import midas/lean/server_supervisor

pub fn spawn_link(
  handler: fn(Request(Iodata)) -> Response(Iodata),
  listen_socket,
  count: Int,
) {
  server_supervisor.spawn_link(handler, listen_socket, count)
}

pub fn start_link(
  handler: fn(Request(Iodata)) -> Response(Iodata),
  port: Int,
  count: Int,
) {
  server_supervisor.start_link(handler, port, count)
}
