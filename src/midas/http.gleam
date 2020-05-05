import gleam/list
import gleam/result.{Option}

import uri/uri
import midas_utils

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

// scheme is part of the connection level
// Body is String, Not Option(String), there is no useful distinction between a body of "" and Nil,
// if needed a get_body funcation can handle the difference, more sensible this way because Nil vs "" decided by method and headers
pub type Request {
  Request(
    method: Method,
    path: String,
    query: Option(String),
    authority: String,
    headers: List(tuple(String, String)),
    body: String,
  )
}

pub type Response {
  Response(status: Int, headers: List(tuple(String, String)), body: String)
}

// Headers
pub fn get_header(headers, key) {
  list.key_find(headers, key)
}

// Request
// I hardly use these
// pub fn host(request: Request) -> String {
//   let Request(authority: authority, ..) = request
//   let tuple(host, _port) = midas_utils.split_on(authority, ":")
//   host
// }
//
// pub fn port(request: Request) -> Int {
//   let Request(authority: authority, ..) = request
//   let tuple(_host, port_string) = midas_utils.split_on(authority, ":")
//
//   case port_string {
//     Error(Nil) -> 80
//     Ok(string) -> {
//       let Ok(port) = int.parse(string)
//       port
//     }
//   }
// }

// Could move to Uri module, same for host vs port, or match how erlang uri string works and replace authority with host port/ question of setting default port or having it optional.
fn do_split_segments(segments_string, accumulator) {
  let tuple(segment, tail) = midas_utils.split_on(segments_string, "/")
  let accumulator = case segment {
    "" -> accumulator
    segment -> [segment, ..accumulator]
  }

  case tail {
    Ok(remaining) -> do_split_segments(remaining, accumulator)
    Error(Nil) -> list.reverse(accumulator)
  }
}

pub fn split_segments(path) {
  let tuple("", tail) = midas_utils.split_on(path, "/")
  case tail {
    Ok("") -> []
    Ok(segments_string) -> do_split_segments(segments_string, [])
  }
}

// pub import uri/uri.{parse_query}
pub fn parse_query(string) {
    uri.parse_query(string)
}
// Response

// Could happily call these things in a web module in template/example project
