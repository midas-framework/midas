import gleam/iodata
import gleam/list
import process
import midas_tcp
import midas_utils
import midas/http
import midas/request.{Request}
// import midas/response
import midas/response.{Response, to_string}

fn read_headers(socket, headers) {
    let Ok(line) = midas_tcp.pull(socket, 5000)
    case http.parse_header_line(line) {
        Ok(http.Header(header)) -> read_headers(socket, [header | headers])
        Ok(http.EndOfHeaders) -> Ok(list.reverse(headers))
    }
}

fn read_request(socket) {
    let Ok(line) = midas_tcp.pull(socket, 5000)
    let Ok(tuple(method, path)) = http.parse_request_line(line)
    let Ok([tuple("host", authority) | headers]) = read_headers(socket, [])
    // change mode to read the body
    // pop host
    // read content length
    Ok(Request(method: method, authority: authority, headers: headers, path: path))
}


fn run(listen_socket, handler) {
    let Ok(socket) = midas_tcp.accept(listen_socket)
    let Ok(request) = read_request(socket)
    let response = handler(request)
    let Ok(Nil) = midas_tcp.send(socket, to_string(response))
    Nil
}

pub fn start_link(listen_socket, handler) {
    let pid = process.start_link(fn(_receive) {
        run(listen_socket, handler)
    })
    Ok(pid)
}

// Have a start child type
