import gleam/iodata
import midas/request.{Request}
import midas/response.{Response}

pub fn handle_request(request) {
    let Request(authority: _, path: path, headers: _) = request
    let body =
        ["Midas: Could not find ", path]
        |> iodata.from_strings
        |> iodata.to_string
    Response(status: 404, headers: [], body: body)
}
