import midas/http
import gleam/should

pub fn parse_segments_test() {
  should.equal(http.split_segments("/"), [])
  should.equal(http.split_segments("/foo/bar"), ["foo", "bar"])
  should.equal(http.split_segments("////"), [])
  should.equal(http.split_segments("/foo//bar"), ["foo", "bar"])
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
//   should.equal(host(request), "e.test")
//
//   let request = Request(
//     method: http.Get,
//     authority: "e.test:8080",
//     headers: [],
//     path: "/",
//     body: "",
//   )
//   should.equal(host(request), "e.test")
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
//   should.equal(port(request), 80)
//
//   let request = Request(
//     method: http.Get,
//     authority: "e.test:8080",
//     headers: [],
//     path: "/",
//     body: "",
//   )
//   should.equal(port(request), 8080)
// }
pub fn parse_query_test() {
  should.equal(http.parse_query(""), [])
  should.equal(http.parse_query("foo=bar"), [tuple("foo", "bar")])
}

pub fn header_test() {
  should.equal(http.get_header([], "foo"), Error(Nil))
  should.equal(http.get_header([tuple("foo", "bar")], "foo"), Ok("bar"))
}
