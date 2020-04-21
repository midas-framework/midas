import gleam/iodata
import midas_tcp
import midas_http
import midas/request.{Request}
// import midas/response
import midas/response.{Response, to_string}

external fn display(a) -> Nil = "erlang" "display"

pub fn start_link() {
    display("data")
    let Ok(listen_socket) = midas_tcp.listen(8080)
    let Ok(socket) = midas_tcp.accept(listen_socket)
    let Ok(data) = midas_tcp.pull(socket, 5000)
    let Ok(request) = midas_http.parse(data)
    let Request(authority: _, path: path, headers: _) = request
    display(path)
    let body = ["Midas: Could not find ", path]
    |> iodata.from_strings
    |> iodata.to_string
    let response = Response(status: 404, headers: [], body: body)
    let response_string = to_string(response)
    display(response_string)
    let Ok(Nil) = midas_tcp.send(socket, response_string)
    0
}
