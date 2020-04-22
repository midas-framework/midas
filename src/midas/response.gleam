import gleam/iodata
import gleam/result.{Option}
import midas/headers as h_utils

pub type Response {
    // scheme is part of the connection level
    // TODO method Enum
    Response(
        status: Int,
        headers: h_utils.Headers,
        body: String
    )
}

pub fn set_header(response: Response, key: String, value: String) -> Response {
    let Response(status: status, headers: headers, body: body) = response
    let headers = h_utils.append(headers, key, value)
    Response(status: status, headers: headers, body: body)
}


pub fn get_header(response: Response, key: String) -> Option(String) {
    let Response(status: _, headers: headers, body: _) = response
    h_utils.find(headers, key)
}

pub fn concat(strings: List(String)) -> String {
    strings
    |> iodata.from_strings
    |> iodata.to_string
}

pub fn to_string(response: Response) -> String {
    let Response(status: _status, headers: _headers, body: body) = response
    // TODO status response
    concat(["HTTP/1.1 ", "200", " Unknown\r\n\r\n", body])
}
