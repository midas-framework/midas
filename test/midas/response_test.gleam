import midas/response.{Response, to_string}
import gleam/expect

pub fn serialize_response_test() {
    let response = Response(status: 200, headers: [tuple("content-type", "text/html")], body: "Hello, World!")
    expect.equal(to_string(response), "HTTP/1.1 200 OK\r\n\r\nHello, World!")

    let response = Response(status: 404, headers: [tuple("content-type", "text/html")], body: "")
    expect.equal(to_string(response), "HTTP/1.1 404 Not Found\r\n\r\n")
}
