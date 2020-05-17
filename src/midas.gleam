import gleam/result
import process/process
import process/process.{From, Pid, BarePid, ExitReason, Kill, Infinity, Milliseconds, TrapExit}
import magpie/supervisor
import midas/http.{Request, Response}

pub fn spawn_link(handler: fn(Request) -> Response, port: Int) {
  supervisor.spawn_link(handler, port)
}

pub fn start_link(handler: fn(Request) -> Response, port: Int) {
  let pid = spawn_link(handler, port)
  Ok(pid)
}
// No optional arguments or multy arity functions
// pub fn start_link(handler: fn(Request) -> Response) -> Result(Nil, Nil) {
//     start_link(handler, 8080)
// }
// NOTE actually a pid in result
