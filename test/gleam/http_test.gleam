import gleam/iodata
import gleam/http
import gleam/http.{Message, Get}
import gleam/should

pub fn redirect_test() {
  let response = http.redirect("/other")

  should.equal(303, response.head.status)
  should.equal(Ok("/other"), http.get_header(response, "location"))
}

pub fn path_segments_test() {
  let request = http.request(Get, "//example.com/foo/bar")
  should.equal(["foo", "bar"], http.path_segments(request))
}

pub fn get_query_test() {
  let request = http.request(Get, "//example.com/?foo=x%20y")
  should.equal(Ok([tuple("foo", "x y")]), http.get_query(request))

  let request = http.request(Get, "//example.com/")
  should.equal(Ok([]), http.get_query(request))

  let request = http.request(Get, "//example.com/?foo=%!2")
  should.equal(Error(Nil), http.get_query(request))
}

pub fn get_header_test() {
  let message = Message(Nil, [tuple("x-foo", "x")], Nil)

  should.equal(Ok("x"), http.get_header(message, "x-foo"))
  should.equal(Ok("x"), http.get_header(message, "X-Foo"))
  should.equal(Error(Nil), http.get_header(message, "x-bar"))
}

pub fn set_header_test() {
  let message = Message(Nil, [], Nil)

  should.equal(
    [tuple("x-foo", "x")],
    http.set_header(message, "x-foo", "x").headers,
  )
  should.equal(
    [tuple("x-foo", "x")],
    http.set_header(message, "X-Foo", "x").headers,
  )
}

pub fn set_body_test() {
  let message = Message(Nil, [], Nil)
    |> http.set_body("Hello, World!")

  should.equal(Ok("13"), http.get_header(message, "content-length"))
}

pub fn get_body_test() {
  let message = Message(Nil, [], iodata.from_strings(["Hello, ", "World!"]))

  should.equal("Hello, World!", http.get_body(message))
}

pub fn set_form_test() {
  let message = Message(Nil, [], Nil)
    |> http.set_form([tuple("foo", "x y"), tuple("bar", "%&")])

  should.equal("foo=x+y&bar=%25%26", iodata.to_string(message.body))
}

pub fn get_form_test() {
  let message = Message(Nil, [], iodata.from_strings(["foo=x+y&bar=%25%26"]))

  should.equal(
    Ok([tuple("foo", "x y"), tuple("bar", "%&")]),
    http.get_form(message),
  )
}
