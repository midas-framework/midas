import midas_http
import midas/request.{Request}

import gleam/expect

external fn display(a) -> Nil = "erlang" "display"

pub fn parse_segments_test() {
    let Ok(request) = midas_http.parse("GET / HTTP/1.1\r\nhost: example.com\r\n\r\n")
    expect.equal(request, Request(authority: "example.com", path: "/", headers: []))
}
