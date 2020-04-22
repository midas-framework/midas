import gleam/iodata
import gleam/list
import process
import midas_tcp
import midas/http
import midas/request.{Request}
// import midas/response
import midas/response.{Response, to_string}

external fn display(a) -> Nil = "erlang" "display"
external fn format(String) -> Nil = "io" "format"

fn read_headers(socket, headers) {
    let Ok(line) = midas_tcp.pull(socket, 5000)
    format(line)
    case http.parse_header_line(line) {
        Ok(http.Header(header)) -> read_headers(socket, [header | headers])
        Ok(http.EndOfHeaders) -> Ok(list.reverse(headers))
    }
}

fn read_request(socket) {
    let Ok(line) = midas_tcp.pull(socket, 5000)
    display(line)
    format(line)
    let Ok(tuple(method, path)) = http.parse_request_line(line)
    display(method)
    let Ok([tuple("host", authority) | headers]) = read_headers(socket, [])
    // change mode to read the body
    // pop host
    // read content length
    Ok(Request(authority: authority, headers: headers, path: path))
}


fn run(listen_socket, handler) {
    let Ok(socket) = midas_tcp.accept(listen_socket)
    display("accepted")
    let Ok(request) = read_request(socket)
    display("request")
    display(request)
    let response = handler(request)
    let Ok(Nil) = midas_tcp.send(socket, to_string(response))
    Nil
}

pub fn start_link() {
    let Ok(listen_socket) = midas_tcp.listen(8080)
    display("Listening")
    process.start(fn(_receive) {
        run(listen_socket, fn(request) {
            let Request(authority: _, path: path, headers: _) = request
            let body =
            ["Midas: Could not find ", path]
                |> iodata.from_strings
                |> iodata.to_string
            Response(status: 404, headers: [], body: body)
        })
    })
}
