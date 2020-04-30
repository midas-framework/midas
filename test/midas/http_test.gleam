import midas/http
import gleam/expect

pub fn parse_segments_test() {
  expect.equal(http.split_segments("/"), [])
  expect.equal(http.split_segments( "/foo/bar" ), ["foo", "bar"])
expect.equal(http.split_segments( "////" ), [])
expect.equal(http.split_segments( "/foo//bar" ), ["foo", "bar"])
}
//
// pub fn parse_host_test() {
//   let [tuple("foo", "bar")] = Request(
//     method: http.Get,
//     authority: "e.test",
//     headers: [],
//     path: "/",
//     body: "",
//   )
//   expect.equal(host(request), "e.test")
//
//   let request = Request(
//     method: http.Get,
//     authority: "e.test:8080",
//     headers: [],
//     path: "/",
//     body: "",
//   )
//   expect.equal(host(request), "e.test")
// }s
//
// pub fn parse_port_test() {
//   let request = Request(
//     method: http.Get,
//     authority: "e.test",
//     headers: [],
//     path: "/",
//     body: "",
//   )
//   expect.equal(port(request), 80)
//
//   let request = Request(
//     method: http.Get,
//     authority: "e.test:8080",
//     headers: [],
//     path: "/",
//     body: "",
//   )
//   expect.equal(port(request), 8080)
// }

pub fn parse_query_test() {
  expect.equal(http.parse_query(""), [])
  expect.equal(http.parse_query("foo=bar"), [tuple("foo", "bar")])
}

pub fn header_test() {
  expect.equal(http.get_header([], "foo"), Error(Nil))
  expect.equal(http.get_header([tuple("foo", "bar")], "foo"), Ok("bar"))
}
