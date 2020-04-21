import midas/request.{Request, segments, host, port, query}
import gleam/expect

pub fn parse_segments_test() {
  let request = Request(authority: "e.test", path: "/")
  expect.equal(segments(request), [])

  let request = Request(authority: "e.test", path: "/foo/bar")
  expect.equal(segments(request), ["foo", "bar"])

  let request = Request(authority: "e.test", path: "////")
  expect.equal(segments(request), [])

  let request = Request(authority: "e.test", path: "/foo//bar")
  expect.equal(segments(request), ["foo", "bar"])

  let request = Request(authority: "e.test", path: "/foo//bar?baz=5")
  expect.equal(segments(request), ["foo", "bar"])

  let request = Request(authority: "e.test", path: "/?baz=5")
  expect.equal(segments(request), [])
}

pub fn parse_host_test() {
  let request = Request(authority: "e.test", path: "/")
  expect.equal(host(request), "e.test")

  let request = Request(authority: "e.test:8080", path: "/")
  expect.equal(host(request), "e.test")
}

pub fn parse_port_test() {
  let request = Request(authority: "e.test", path: "/")
  expect.equal(port(request), 80)

  let request = Request(authority: "e.test:8080", path: "/")
  expect.equal(port(request), 8080)
}

pub fn parse_query_test() {
  let request = Request(authority: "e.test", path: "/")
  expect.equal(query(request), [])

  let request = Request(authority: "e.test", path: "/?")
  expect.equal(query(request), [])

  let request = Request(authority: "e.test", path: "/?foo=bar")
  expect.equal(query(request), [tuple("foo", "bar")])

}
