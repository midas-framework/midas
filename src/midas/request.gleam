import midas_utils
import midas/headers as h_utils
import midas/http
import gleam/result
import gleam/result.{Option}
import gleam/list
import gleam/int
import gleam/string

pub type Request {
  // scheme is part of the connection level
  Request(
    method: http.Method,
    path: String,
    authority: String,
    headers: h_utils.Headers,
    body: String,
  )
}

// Not Option(String), there is no useful distinction between a body of "" and Nil,
// if needed a get_body funcation can handle the difference, more sensible this way because Nil vs "" decided by method and headers
pub fn host(request: Request) -> String {
  let Request(authority: authority, ..) = request
  let tuple(host, _port) = midas_utils.split_on(authority, ":")
  host
}

pub fn port(request: Request) -> Int {
  let Request(authority: authority, ..) = request
  let tuple(_host, port_string) = midas_utils.split_on(authority, ":")

  case port_string {
    Error(Nil) -> 80
    Ok(string) -> {
      let Ok(port) = int.parse(string)
      port
    }
  }
}

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

fn split_segments(path) {
  let tuple("", tail) = midas_utils.split_on(path, "/")
  case tail {
    Ok("") -> []
    Ok(segments_string) -> do_split_segments(segments_string, [])
  }
}

pub fn segments(request: Request) -> List(String) {
  let Request(path: raw_path, ..) = request
  let tuple(path, _query) = midas_utils.split_on(raw_path, "?")
  split_segments(path)
}

fn do_split_query_string(query_string, accumulator) {
  let tuple(part, tail) = midas_utils.split_on(query_string, "&")
  let tuple(key, maybe_value) = midas_utils.split_on(part, "=")
  let pair = tuple(key, result.unwrap(maybe_value, or: ""))

  // TODO escaping of query string and form parameters
  let accumulator = case key {
    "" -> accumulator
    _ -> [pair, ..accumulator]
  }
  case tail {
    Error(Nil) -> list.reverse(accumulator)
    Ok(rest) -> do_split_query_string(rest, accumulator)
  }
}

pub fn query(request: Request) -> List(tuple(String, String)) {
  let Request(path: raw_path, ..) = request
  let tuple(_path, query_string) = midas_utils.split_on(raw_path, "?")
  case query_string {
    Ok(query_string) -> do_split_query_string(query_string, [])
    Error(Nil) -> []
  }
}

pub fn set_header(request: Request, key: String, value: String) -> Request {
  let Request(
    method: method,
    authority: authority,
    headers: headers,
    path: path,
    body: body,
  ) = request
  let headers = h_utils.append(headers, key, value)
  Request(
    method: method,
    authority: authority,
    headers: headers,
    path: path,
    body: body,
  )
}

pub fn get_header(request: Request, key: String) -> Option(String) {
  let Request(headers: headers, ..) = request
  h_utils.find(headers, key)
}
