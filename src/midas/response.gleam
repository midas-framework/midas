import gleam/iodata
import gleam/int
import gleam/result.{Option}
import midas/headers as h_utils

pub type Response {
  Response(status: Int, headers: h_utils.Headers, body: String)
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
  let Response(status: status, headers: _headers, body: body) = response
  let reason_phrase = case status {
    200 -> "OK"
    404 -> "Not Found"
    _ -> "Unknown"
  }

  // TODO needs to add content length Or does it!, use the set_body function
  concat(
    ["HTTP/1.1 ", int.to_string(status), " ", reason_phrase, "\r\n\r\n", body],
  )
}
