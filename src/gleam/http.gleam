import gleam/int
import gleam/iodata.{Iodata}
import gleam/list
import gleam/option.{Option, Some, None}
import gleam/string
import gleam/uri

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

pub type Header =
  tuple(String, String)

pub type Message(head, body) {
  Message(head: head, headers: List(Header), body: body)
}

pub type RequestHead {
  RequestHead(
    method: Method,
    host: String,
    port: Option(Int),
    path: String,
    query: Option(String),
  )
}

pub type ResponseHead {
  ResponseHead(status: Int)
}

pub type Request(body) =
  Message(RequestHead, body)

pub type Response(body) =
  Message(ResponseHead, body)

pub fn request(method: Method, uri_string: String) -> Request(Nil) {
  let Ok(
    uri.Uri(host: Some(host), port: port, path: path, query: query, ..),
  ) = uri.parse(uri_string)
  Message(
    head: RequestHead(method, host, port, path, query),
    headers: [],
    body: Nil,
  )
}

pub fn response(status: Int) -> Response(Nil) {
  Message(head: ResponseHead(status), headers: [], body: Nil)
}

pub fn path_segments(message: Message(RequestHead, body)) -> List(String) {
  let Message(RequestHead(path: path, ..), ..) = message
  uri.path_segments(path)
}

pub fn method(message: Message(RequestHead, body)) -> Method {
  let Message(RequestHead(method: method, ..), ..) = message
  method
}

pub fn get_query(
  message: Message(RequestHead, body),
) -> Result(List(tuple(String, String)), Nil) {
  let Message(RequestHead(query: query_string, ..), ..) = message
  case query_string {
    Some(query_string) -> uri.parse_query(query_string)
    None -> Ok([])
  }
}

pub fn get_header(message: Message(head, body), key: String) -> Option(String) {
  let Message(headers: headers, ..) = message
  list.key_find(headers, string.lowercase(key))
  |> option.from_result()
}

pub fn set_header(
  message: Message(head, body),
  key: String,
  value: String,
) -> Message(head, body) {
  let Message(head: head, headers: headers, body: body) = message
  let headers = list.append(headers, [tuple(string.lowercase(key), value)])
  Message(head: head, headers: headers, body: body)
}

// uses String as simplest but iodata internally, thats what server can deal with
pub fn get_body(message: Message(head, Iodata)) -> String {
  let Message(body: body, ..) = message
  iodata.to_string(body)
}

pub fn set_body(
  message: Message(head, Nil),
  body: String,
) -> Message(head, Iodata) {
  let Message(head: head, headers: headers, body: _nil) = message
  let body = iodata.from_strings([body])
  let content_length = iodata.byte_size(body)
  let headers = list.append(
    headers,
    [tuple("content-length", int.to_string(content_length))],
  )
  Message(head: head, headers: headers, body: body)
}

pub fn get_form(
  message: Message(head, Iodata),
) -> Result(List(tuple(String, String)), Nil) {
  let Message(body: body, ..) = message
  uri.parse_query(iodata.to_string(body))
}

pub fn set_form(message, form) {
  message
  |> set_header("content-type", "application/x-www-form-urlencoded")
  |> set_body(uri.query_to_string(form))
}

pub fn redirect(location: String) -> Response(Iodata) {
  response(303)
  |> set_header("location", location)
  |> set_body("")
}
