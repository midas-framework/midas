import gleam/iodata.{Iodata}
import gleam/result
import gleam/http.{Request, Response}
import process/process
import process/process.{From, Pid, BarePid, ExitReason, Infinity, Milliseconds, TrapExit}
import midas/lean/endpoint

pub fn spawn_link(handler: fn(Request(Iodata)) -> Response(Iodata), listen_socket) {
  endpoint.spawn_link(handler, listen_socket)
}

pub fn start_link(handler: fn(Request(Iodata)) -> Response(Iodata), port: Int) {
  endpoint.start_link(handler, port)
}
// No optional arguments or multy arity functions
// pub fn start_link(handler: fn(Request) -> Response) -> Result(Nil, Nil) {
//     start_link(handler, 8080)
// }
// NOTE actually a pid in result
