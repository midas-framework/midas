import gleam/result
import midas/request.{Request}
import midas/response.{Response}

pub external fn start_link(fn(Request) -> Response, Int) -> Result(Nil, Nil)
    = "midas_supervisor" "start_link"

// No optional arguments or multy arity functions
// pub fn start_link(handler: fn(Request) -> Response) -> Result(Nil, Nil) {
//     start_link(handler, 8080)
// }
// NOTE actually a pid in result
