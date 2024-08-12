import gleam/bit_array
import gleam/dynamic
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

pub type App {
  App(client_id: String, redirect_uri: String)
}

const auth_host = "accounts.google.com"

const auth_path = "/o/oauth2/auth"

pub fn authenticate(app, scopes) {
  let App(client_id, redirect_uri) = app
  let state = int.to_string(int.random(1_000_000_000))
  let url = auth_url(client_id, redirect_uri, scopes, state)
  use redirect <- t.do(t.follow(url))
  use #(access_token, token_type, returned_state) <- t.try(
    auth_redirect(redirect) |> result.map_error(snag.new),
  )
  use Nil <- t.try(case returned_state == state {
    True -> Ok(Nil)
    False -> Error(snag.new("returned state was not equal to sent state"))
  })

  use Nil <- t.try(case token_type == "Bearer" {
    True -> Ok(Nil)
    False -> Error(snag.new("returned token_type was not 'Bearer'"))
  })
  t.Done(access_token)
}

fn auth_url(client_id, redirect_uri, scopes, state) {
  let query = [
    #("client_id", client_id),
    #("response_type", "token"),
    #("redirect_uri", redirect_uri),
    #("state", state),
    #("scope", string.join(scopes, " ")),
  ]
  let query = Some(uri.query_to_string(query))
  Uri(Some("https"), None, Some(auth_host), None, auth_path, query, None)
  |> uri.to_string
}

pub fn auth_redirect(redirect) {
  let Uri(fragment: fragment, ..) = redirect
  use hash <- try(case fragment {
    Some(hash) -> Ok(hash)
    None -> Error("uri did not have a fragment")
  })
  use parts <- try(
    uri.parse_query(hash)
    |> result.replace_error("Failed to parse query: " <> hash),
  )
  use access_token <- try(key_find(parts, "access_token"))
  use token_type <- try(key_find(parts, "token_type"))
  use returned_state <- try(key_find(parts, "state"))
  // other parts expires_in and scope
  Ok(#(access_token, token_type, returned_state))
}

fn key_find(items, key) {
  list.key_find(items, key)
  |> result.replace_error("Did not find key: " <> key)
}

const openid_host = "openidconnect.googleapis.com"

const userinfo_path = "/v1/userinfo"

pub fn userinfo(token) {
  let request = userinfo_request(token)
  use response <- t.do(t.fetch(request))
  use response <- t.try(userinfo_response(response))
  t.Done(response)
}

pub fn userinfo_request(token) {
  request.new()
  |> request.set_host(openid_host)
  |> request.prepend_header("Authorization", string.append("Bearer ", token))
  |> request.set_path(userinfo_path)
  |> request.set_body(<<>>)
}

pub fn userinfo_response(response: response.Response(BitArray)) {
  use json <- try(
    bit_array.to_string(response.body)
    |> result.replace_error(snag.new("not utf8 encoded")),
  )
  use message <- try(
    json.decode_bits(response.body, dynamic.field("email", dynamic.string))
    |> result.map_error(fn(reason) {
      snag.new(string.inspect(reason))
      |> snag.layer("failed to decode message")
    }),
  )
  Ok(message)
}
