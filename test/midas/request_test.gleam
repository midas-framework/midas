import midas/request.{Request, segments}
import gleam/expect

pub fn parse_segments_test() {
  let request = Request(path: "/")
  expect.equal(segments(request), [])

  let request = Request(path: "/foo/bar")
  expect.equal(segments(request), ["foo", "bar"])

  let request = Request(path: "////")
  expect.equal(segments(request), [])

  let request = Request(path: "/foo//bar")
  expect.equal(segments(request), ["foo", "bar"])

  let request = Request(path: "/foo//bar?baz=5")
  expect.equal(segments(request), ["foo", "bar"])
  
  let request = Request(path: "/?baz=5")
  expect.equal(segments(request), [])
}
