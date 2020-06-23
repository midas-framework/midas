import gleam/iodata.{Iodata}
import gleam/list
import gleam/result
import gleam/http.{Request, Response}
import midas/lean/server_supervisor

pub type Option {
  MaxConcurrency(Int)
}

pub fn spawn_link(
  handler: fn(Request(Iodata)) -> Response(Iodata),
  listen_socket,
  options: List(Option),
) {
  let MaxConcurrency(
    count,
  ) = list.head(options)
    |> result.unwrap(MaxConcurrency(10000))
  server_supervisor.spawn_link(handler, listen_socket, count)
}

pub fn start_link(
  handler: fn(Request(Iodata)) -> Response(Iodata),
  port: Int,
  options: List(Option),
) {
  let MaxConcurrency(
    count,
  ) = list.head(options)
    |> result.unwrap(MaxConcurrency(10000))
  server_supervisor.start_link(handler, port, count)
}
