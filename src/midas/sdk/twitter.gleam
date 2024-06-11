import gleam/bit_array

import gleam/dynamic

// import gleam/fetch
import gleam/http
import gleam/http/request

import gleam/http/response.{Response}
import gleam/int
import gleam/io

// import gleam/javascript/promise
import gleam/json
import gleam/list
import gleam/option.{None, Some}

import gleam/result.{try}
import gleam/string
import gleam/uri.{Uri}
import midas/task as t
import snag

const auth_host = "twitter.com"

const auth_path = "/i/oauth2/authorize"

// https://developer.twitter.com/en/docs/authentication/oauth-2-0/user-access-token
pub fn authenticate(client_id, redirect_uri, scopes) {
  let state = int.to_string(int.random(1_000_000_000))
  let challenge = int.to_string(int.random(1_000_000_000))
  let url = auth_url(client_id, redirect_uri, scopes, state, challenge)

  use redirect <- t.do(t.follow(url))
  use #(code, returned_state) <- t.try(
    auth_redirect(redirect) |> result.map_error(snag.new),
  )
  use Nil <- t.try(case returned_state == state {
    True -> Ok(Nil)
    False -> Error(snag.new("returned state was not equal to sent state"))
  })

  let request = token_request(client_id, redirect_uri, challenge, code)
  use response <- t.do(t.fetch(request))
  use token <- t.try(token_response(response))
  t.Done(token)
}

pub fn auth_url(client_id, redirect_uri, scopes, state, challenge) {
  let query = [
    #("client_id", client_id),
    #("response_type", "code"),
    #("redirect_uri", redirect_uri),
    #("state", state),
    #("code_challenge", challenge),
    #("code_challenge_method", "plain"),
    #("scope", string.join(scopes, " ")),
  ]
  let query = Some(uri.query_to_string(query))
  Uri(Some("https"), None, Some(auth_host), None, auth_path, query, None)
  |> uri.to_string
}

pub fn auth_redirect(redirect) {
  let Uri(query: query, ..) = redirect
  use query <- try(case query {
    Some(query) -> Ok(query)
    None -> Error("uri did not have a query: " <> string.inspect(redirect))
  })
  use parts <- try(
    uri.parse_query(query)
    |> result.replace_error("Failed to parse query: " <> query),
  )
  use code <- try(key_find(parts, "code"))
  use returned_state <- try(key_find(parts, "state"))
  Ok(#(code, returned_state))
}

fn key_find(items, key) {
  list.key_find(items, key)
  |> result.replace_error("Did not find key: " <> key)
}

const api_host = "api.twitter.com"

const token_path = "/2/oauth2/token"

pub fn token_request(client_id, redirect_uri, code_verifier, code) {
  let query = [
    #("grant_type", "authorization_code"),
    #("client_id", client_id),
    #("redirect_uri", redirect_uri),
    #("code", code),
    #("code_verifier", code_verifier),
  ]

  request.new()
  |> request.set_host(api_host)
  |> request.set_method(http.Post)
  |> request.set_path(token_path)
  |> request.prepend_header("content-type", "application/x-www-form-urlencoded")
  |> request.set_body(bit_array.from_string(uri.query_to_string(query)))
}

pub fn token_response(response) {
  let Response(body: body, ..) = response
  let assert Ok(body) = bit_array.to_string(body)
  let decoder = dynamic.field("access_token", dynamic.string)
  let assert Ok(token) = json.decode(body, decoder)
  Ok(token)
}

fn base_request(token) {
  request.new()
  |> request.set_host(api_host)
  |> request.prepend_header("Authorization", string.append("Bearer ", token))
}

fn get(token, path) {
  base_request(token)
  |> request.set_path(path)
  |> request.set_body(<<>>)
}

fn post(token, path, mime, content) {
  base_request(token)
  |> request.set_method(http.Post)
  |> request.set_path(path)
  |> request.prepend_header("content-type", mime)
  |> request.set_body(content)
}

pub fn create_tweet(token, text) {
  let request = create_tweet_request(token, text)

  use response <- t.do(t.fetch(request))
  // use response <- t.try(token_response(response))
  t.Done(response)
}

pub fn create_tweet_request(token, text) {
  let path = "/2/tweets"
  let body = json.to_string(json.object([#("text", json.string(text))]))
  post(token, path, "application/json", bit_array.from_string(body))
}

pub fn user_by_username(token, username) {
  let request = user_by_username_request(token, username)
  use response <- t.do(t.fetch(request))
  // use response <- t.try(token_response(response))
  t.Done(response)
}

pub fn user_by_username_request(token, username) {
  let path = "/2/users/by/username/" <> username
  get(token, path)
}

pub fn user_timeline(token, user_id) {
  let request = user_timeline_request(token, user_id)
  use response <- t.do(t.fetch(request))
  io.debug("==============")
  use response <- t.try(user_timeline_response(response))
  t.Done(response)
}

pub fn user_timeline_request(token, user_id) {
  let path = "/2/users/" <> user_id <> "/tweets"
  get(token, path)
}

pub fn user_timeline_response(response) {
  let Response(body: body, ..) = response
  let assert Ok(body) = bit_array.to_string(body)
  let assert Ok(data) = json.decode(body, Ok)
  io.debug(#(data, "00000000000"))
  Ok(data)
}
