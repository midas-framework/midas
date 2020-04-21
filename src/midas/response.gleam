import gleam/result.{Option}
import midas/headers as h_utils

pub type Response {
    // scheme is part of the connection level
    // TODO method Enum
    Response(
        status: Int,
        headers: h_utils.Headers,
    )
}

pub fn set_header(response: Response, key: String, value: String) -> Response {
    let Response(status: status, headers: headers) = response
    let headers = h_utils.append(headers, key, value)
    Response(status: status, headers: headers)
}


pub fn get_header(response: Response, key: String) -> Option(h_utils.Header) {
    let Response(status: _, headers: headers) = response
    h_utils.find(headers, key)
}
