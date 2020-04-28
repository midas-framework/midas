import midas/http
import gleam/expect

pub fn parse_request_line_test() {
  let Ok(tuple(method, path)) = http.parse_request_line("GET / HTTP/1.1\r\n")
  expect.equal(method, http.Get)
  expect.equal(path, "/")
}
