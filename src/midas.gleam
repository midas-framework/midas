import gleam/result
import core/process
import midas/supervisor
import midas/request.{Request}
import midas/response.{Response}

pub fn start_link(handler: fn(Request) -> Response, port: Int) -> Result(process.Pid(Nil), Nil) {
    let pid = supervisor.spawn_link(handler, port)
    Ok(pid)
}

// No optional arguments or multy arity functions
// pub fn start_link(handler: fn(Request) -> Response) -> Result(Nil, Nil) {
//     start_link(handler, 8080)
// }
// NOTE actually a pid in result
