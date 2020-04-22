import gleam/string
import midas_utils
import midas/headers as h_utils
// All these functions assume socket is reading one line at a time.

// HTTP standard method as defined by RFC 2616, and PATCH which is defined by
// RFC 5789.
//
pub type Method {
  Get
  Post
  Head
  Put
  Delete
  Trace
  Connect
  Options
  Patch
}

pub type HeaderLine {
    Header(tuple(String, String))
    EndOfHeaders
}

fn parse_method(method_string) -> Result(Method, Nil) {
  case method_string {
    "CONNECT" -> Ok(Connect)
    "DELETE" -> Ok(Delete)
    "GET" -> Ok(Get)
    "HEAD" -> Ok(Head)
    "OPTIONS" -> Ok(Options)
    "PATCH" -> Ok(Patch)
    "POST" -> Ok(Post)
    "PUT" -> Ok(Put)
    "TRACE" -> Ok(Trace)
    _ -> Error(Nil)
  }
}

pub fn parse_request_line(line: String) {
    let tuple(method_string, Ok(rest)) = midas_utils.split_on(line, " ")
    let Ok(method) = parse_method(method_string)

    let tuple(path, Ok("HTTP/1.1\r\n")) = midas_utils.split_on(rest, " ")
    // TODO check starts with "/"
    Ok(tuple(method, path))
}

pub fn parse_header_line(line: String) -> Result(HeaderLine, Nil) {
    let tuple(line, Ok("")) = midas_utils.split_on(line, "\r\n")
    case line {
        "" -> Ok(EndOfHeaders)
        _ -> {
            let tuple(key, Ok(value)) = midas_utils.split_on(line, ": ")
            Ok(Header(tuple(string.lowercase(key), value)))
        }
    }
}
