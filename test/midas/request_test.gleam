import midas/request.{Request, segments, host, port, query, get_header, set_header}
import midas/http
import gleam/expect

pub fn parse_segments_test() {
  let request = Request(
    method: http.Get,
    authority: "e.test",
    headers: [],
    path: "/",
    body: "",
  )
  expect.equal(segments(request), [])

  let request = Request(
    method: http.Get,
    authority: "e.test",
    headers: [],
    path: "/foo/bar",
    body: "",
  )
  expect.equal(segments(request), ["foo", "bar"])

  let request = Request(
    method: http.Get,
    authority: "e.test",
    headers: [],
    path: "////",
    body: "",
  )
  expect.equal(segments(request), [])

  let request = Request(
    method: http.Get,
    authority: "e.test",
    headers: [],
    path: "/foo//bar",
    body: "",
  )
  expect.equal(segments(request), ["foo", "bar"])

  let request = Request(
    method: http.Get,
    authority: "e.test",
    headers: [],
    path: "/foo//bar?baz=5",
    body: "",
  )
  expect.equal(segments(request), ["foo", "bar"])

  let request = Request(
    method: http.Get,
    authority: "e.test",
    headers: [],
    path: "/?baz=5",
    body: "",
  )
  expect.equal(segments(request), [])
}

pub fn parse_host_test() {
  let request = Request(
    method: http.Get,
    authority: "e.test",
    headers: [],
    path: "/",
    body: "",
  )
  expect.equal(host(request), "e.test")

  let request = Request(
    method: http.Get,
    authority: "e.test:8080",
    headers: [],
    path: "/",
    body: "",
  )
  expect.equal(host(request), "e.test")
}

pub fn parse_port_test() {
  let request = Request(
    method: http.Get,
    authority: "e.test",
    headers: [],
    path: "/",
    body: "",
  )
  expect.equal(port(request), 80)

  let request = Request(
    method: http.Get,
    authority: "e.test:8080",
    headers: [],
    path: "/",
    body: "",
  )
  expect.equal(port(request), 8080)
}

pub fn parse_query_test() {
  let request = Request(
    method: http.Get,
    authority: "e.test",
    headers: [],
    path: "/",
    body: "",
  )
  expect.equal(query(request), [])

  let request = Request(
    method: http.Get,
    authority: "e.test",
    headers: [],
    path: "/?",
    body: "",
  )
  expect.equal(query(request), [])

  let request = Request(
    method: http.Get,
    authority: "e.test",
    headers: [],
    path: "/?foo=bar",
    body: "",
  )
  expect.equal(query(request), [tuple("foo", "bar")])
}

pub fn header_test() {
  let request = Request(
    method: http.Get,
    authority: "e.test",
    headers: [],
    path: "/",
    body: "",
  )
  expect.equal(get_header(request, "foo"), Error(Nil))

  let request = set_header(request, "foo", "bar")
  expect.equal(get_header(request, "foo"), Ok("bar"))
}
