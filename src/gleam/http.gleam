import gleam/iodata.{Iodata}
import gleam/list
import gleam/result.{Option}

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

pub type Header = tuple(String, String)

pub type Message(head, body) {
    Message(head: head, headers: List(Header), body: body)
}

pub type RequestHead{
    RequestHead
}
pub type ResponseHead{
    ResponseHead(status: Int)
}

pub type Request(body) = Message(RequestHead, body)

pub type Response(body) = Message(ResponseHead, body)


pub fn request() -> Request(Nil) {
    todo
}

pub fn response(status: Int) -> Response(Nil) {
    todo
}

// pub fn path_segments(message)

// get_query -> returns empty list if no query string.

pub fn get_header(message: Message(h, b), key: String) -> Option(String) {
    let Message(headers: headers, ..) = message
    list.key_find(headers, key)
}

pub fn set_header(message: Message(h, b), key: String, value: String) -> Message(h, b) {
    todo
}

pub fn get_body(message:  Message(h, Iodata)) -> String {
    todo
}
pub fn set_body(message: Message(h, Nil), body: String) -> Message(h, Iodata) {
    // .. operator
    // let message = Message(body: [], ..message)
    todo
    // single setter is this lenses
}


pub fn get_form() {
    todo
}

pub fn set_form() {
    todo
}
