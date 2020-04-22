import gleam/result
import midas/request.{Request}
import midas/response.{Response}

pub external fn start_link(fn(Request) -> Response, Int) -> Result(Nil, Nil)
    = "midas_supervisor" "start_link"

// NOTE actually a pid in result
