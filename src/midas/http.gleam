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

pub type Message(t, b) {
  Message(t, List(tuple(String, String)), b)
}

pub fn response(status: Int) {
  Message(status, [], Nil)
}

pub fn set_body(message: Message(t, Nil), body) {
  let Message(start_line, headers, Nil) = message
  // TODO add content-length
  Message(start_line, headers, body)
}
pub fn stuff() {
  response(200)
  |> set_body("help", iolist.byte_size)
  |> set_body("help", string.byte_size)
}

// Startup without Error,
// What do you do in the start link function.
// No names.

// gleam/uri make case for standard lib
// gleam/http less likely standard lib but will work on client + server
// Write Readme for both, requires extra pages in docs to be supported.
// uri.split_segments split on slash and then remove empty strings
// http.get_form works on body of type iolist
// http.set_form sets iolist
// http.get_query returns empty list if no query string but Error(ParseError) is still possible
// do_sleep returns external type OK but sleep returns nil
// ExitReason is Normal/Crash/Kill
// There is no Exit function, only a Kill function
// supervisor should accept run function
// I like the return type on Run being Never
type Child(m) {
  Permanent(Run(m, Never))
  Transient(Run(m, Shutdown))
  Temporary(Run(m, Nil))
}
// What does one_for_rest even mean if process stops.
// returning which_children Live(Pid) | Done(Reason (Never is good cz this should never happen.))
// process always has to handle case it might be too slow and be killed, or node dies. let's go strait to kill

fn do_loop(receive, loop, state) {
  do_loop(receive, loop, loop(receive(Infinity, state)))
}

fn gen_server(init, loop) {
  fn (receive) {
    let state = init()
    do_loop(receive, loop, state)
  }
}
// Return state loop


fn loop(message) {

  tuple(loop_two(_, "Foo"), [(Address, message), (from, reply)])
}

// Don't think the list of messages is doable, could be a send function, and there might be ways to intercept, but we don't have single state value to look at

// If ets is started reference can be returned by a call to the pid in an init step
// Should write up why No names

// Call should be build on channels.

// Stop asking for permission seems logically consistent see how far it gets.
// magpie is a good name for client, get's shiny things

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
