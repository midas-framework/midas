import gleam/bit_array
import gleam/dynamic
import gleam/http
import gleam/http/request
import gleam/http/response
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result.{try}
import gleam/string
import gleam/uri.{Uri}
import midas/task as t
import snag

const api_host = "gmail.googleapis.com"

fn base_request(token) {
  request.new()
  |> request.set_host(api_host)
  |> request.prepend_header("Authorization", string.append("Bearer ", token))
}

// fn get(token, path) {
//   base_request(token)
//   |> request.set_path(path)
//   |> request.set_body(<<>>)
// }

fn post(token, path, mime, content) {
  base_request(token)
  |> request.set_method(http.Post)
  |> request.set_path(path)
  |> request.prepend_header("content-type", mime)
  |> request.set_body(content)
}

pub fn send(token, from, to, message) {
  let request = send_request(token, from, to, message)
  use response <- t.do(t.fetch(request))
  use response <- t.try(send_response(response))
  t.Done(response)
}

pub fn send_request(token, from, to, message) {
  let mime = "application/json; charset=UTF-8"
  let email =
    "From: "
    <> from
    <> " To: "
    <> to
    <> " Content-Type: text/html; charset=utf-8 "
    <> message
  let raw = bit_array.base64_encode(<<email:utf8>>, True)
  let body = json.object([#("raw", json.string(raw))]) |> json.to_string
  request.new()
  // non-standard host
  |> request.set_host("content-gmail.googleapis.com")
  |> request.set_query([#("alt", "json")])
  |> request.prepend_header("Authorization", string.append("Bearer ", token))
  |> request.set_path("/gmail/v1/users/" <> from <> "/messages/send")
  |> request.prepend_header("content-type", mime)
  |> request.set_body(<<body:utf8>>)
}

pub fn send_response(response: response.Response(BitArray)) {
  use json <- try(
    bit_array.to_string(response.body)
    |> result.replace_error(snag.new("not utf8 encoded")),
  )
  //   let decoder = dynamic.list(site_decoder)
  //   use videos <- try(
  //     json.decode_bits(response.body, decoder)
  //     |> result.map_error(fn(reason) {
  //       snag.new(string.inspect(reason))
  //       |> snag.layer("failed to decode sites")
  //     }),
  //   )
  Ok(json)
}
// fn site_decoder(raw) {
//   dynamic.decode4(
//     Site,
//     dynamic.field("id", dynamic.string),
//     dynamic.field("state", dynamic.string),
//     dynamic.field("name", dynamic.string),
//     dynamic.field("url", dynamic.string),
//   )(raw)
// }
