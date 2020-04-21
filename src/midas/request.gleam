import midas_utils
import gleam/result
import gleam/list

external fn display(a) -> Nil = "erlang" "display"
pub type Request {
  Request(path: String)
}

fn do_split_segments(segments_string, accumulator) {
  let tuple(segment, tail) =  midas_utils.split_on(segments_string, "/")
  let accumulator = case segment {
    "" -> accumulator
    segment -> [segment | accumulator]
  }

  case tail {
    Ok(remaining) -> do_split_segments(remaining, accumulator)
    Error(Nil) -> list.reverse(accumulator)
  }
}

fn split_segments(path) {
  let tuple("", tail) = midas_utils.split_on(path, "/")
  case tail {
    Ok("") -> []
    Ok(segments_string) -> do_split_segments(segments_string, [])
  }
}


pub fn segments(request: Request) -> List(String) {
  let Request(path: raw_path) = request
  let tuple(path, _query) = midas_utils.split_on(raw_path, "?")
  split_segments(path)
}
