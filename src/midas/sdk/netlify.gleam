import gleam/bit_array
import gleam/dynamic
import gleam/http
import gleam/http/request
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

const auth_host = "app.netlify.com"

const auth_path = "/authorize"

pub fn authenticate(app) {
  let App(client_id, redirect_uri) = app
  let state = int.to_string(int.random(1_000_000_000))
  let url = auth_url(client_id, redirect_uri, state)

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

fn auth_url(client_id, redirect_uri, state) {
  let query = [
    #("client_id", client_id),
    #("response_type", "token"),
    #("redirect_uri", redirect_uri),
    #("state", state),
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
  Ok(#(access_token, token_type, returned_state))
}

fn key_find(items, key) {
  list.key_find(items, key)
  |> result.replace_error("Did not find key: " <> key)
}

const api_host = "api.netlify.com"

fn base_request(token) {
  request.new()
  |> request.set_host(api_host)
  |> request.prepend_header("Authorization", string.append("Bearer ", token))
}

fn post(token, path, mime, content) {
  base_request(token)
  |> request.set_method(http.Post)
  |> request.set_path(path)
  |> request.prepend_header("content-type", mime)
  |> request.set_body(content)
}

pub fn deploy_site(token, site_id, files) {
  use zipped <- t.do(t.zip(files))
  let path = string.concat(["/api/v1/sites/", site_id, "/deploys"])
  let r = post(token, path, "application/zip", zipped)

  use response <- t.do(t.fetch(r))
  use body <- t.do(case response.status {
    200 -> t.Done(response.body)
    other ->
      t.Abort(snag.new("Status was not OK got: " <> int.to_string(other)))
  })
  let assert Ok(body) = bit_array.to_string(body)
  let assert Ok(data) =
    json.decode(body, dynamic.field("state", dynamic.string))
  t.Done(data)
}
