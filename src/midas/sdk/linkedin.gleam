import gleam/bit_array
import gleam/dynamic
import gleam/http
import gleam/http/request
import gleam/http/response.{Response}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result.{try}
import gleam/string
import gleam/uri.{Uri}
import midas/task as t
import snag

// https://www.linkedin.com/developers/apps/
pub type App {
  App(client_id: String, client_secret: String, redirect_uri: String)
}

const auth_host = "www.linkedin.com"

const auth_path = "/oauth/v2/authorization"

// https://learn.microsoft.com/en-us/linkedin/shared/authentication/authorization-code-flow?tabs=HTTPS1
pub fn authenticate(app, scopes) {
  let App(client_id, client_secret, redirect_uri) = app
  let state = int.to_string(int.random(1_000_000_000))
  let url = auth_url(client_id, redirect_uri, scopes, state)

  use redirect <- t.do(t.follow(url))
  use #(code, returned_state) <- t.try(
    auth_redirect(redirect) |> result.map_error(snag.new),
  )
  use Nil <- t.try(case returned_state == state {
    True -> Ok(Nil)
    False -> Error(snag.new("returned state was not equal to sent state"))
  })

  let request = token_request(client_id, redirect_uri, client_secret, code)
  use response <- t.do(t.fetch(request))
  use token <- t.try(token_response(response))
  t.Done(token)
}

fn auth_url(client_id, redirect_uri, scopes, state) {
  let query = [
    #("client_id", client_id),
    #("response_type", "code"),
    #("redirect_uri", redirect_uri),
    #("scope", string.join(scopes, " ")),
    #("state", state),
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

const token_path = "/oauth/v2/accessToken"

pub fn token_request(client_id, redirect_uri, client_secret, code) {
  let query = [
    #("grant_type", "authorization_code"),
    #("client_id", client_id),
    #("redirect_uri", redirect_uri),
    #("code", code),
    #("client_secret", client_secret),
  ]

  request.new()
  |> request.set_host(auth_host)
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

const api_host = "api.linkedin.com"

fn base_request(token) {
  request.new()
  |> request.set_host(api_host)
  |> request.prepend_header("Authorization", string.append("Bearer ", token))
  |> request.set_header("X-Restli-Protocol-Version", "2.0.0")
  |> request.set_header("LinkedIn-Version", "202408")
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

fn post_json(token, path, data) {
  let body = json.to_string(data) |> bit_array.from_string
  post(token, path, "application/json", body)
}

pub fn userinfo(token) {
  let request = userinfo_request(token)
  use response <- t.do(t.fetch(request))
  use response <- t.try(userinfo_response(response))
  t.Done(response)
}

pub fn userinfo_request(token) {
  let path = "/v2/userinfo"
  get(token, path)
}

pub type Userinfo {
  Userinfo(sub: String)
}

fn userinfo_decoder(raw) {
  dynamic.decode1(Userinfo, dynamic.field("sub", dynamic.string))(raw)
}

pub fn userinfo_response(response: response.Response(BitArray)) {
  use json <- try(
    bit_array.to_string(response.body)
    |> result.replace_error(snag.new("not utf8 encoded")),
  )
  // let decoder = dynamic.list(site_decoder)
  use info <- try(
    json.decode_bits(response.body, userinfo_decoder)
    |> result.map_error(fn(reason) {
      snag.new(string.inspect(reason))
      |> snag.layer("failed to decode sites")
    }),
  )
  Ok(info)
}

fn error_decoder(raw) {
  dynamic.field("message", dynamic.string)(raw)
}

pub type Author {
  Person(id: String)
  Organization(id: String)
}

pub fn author_to_urn(author) {
  let type_ = case author {
    Person(_) -> "person"
    Organization(_) -> "organization"
  }
  "urn:li:" <> type_ <> ":" <> author.id
}

pub type Image {
  Image(id: String)
}

pub fn image_to_urn(image) {
  let Image(id) = image
  "urn:li:image:" <> id
}

pub type Content {
  // https://learn.microsoft.com/en-us/linkedin/marketing/integrations/ads/advertising-targeting/version/article-ads-integrations?view=li-lms-2024-08&tabs=http#workflow
  Article(
    description: Option(String),
    source: String,
    thumbnail: Option(Image),
    thumbnail_alt_text: Option(String),
    title: String,
  )
}

pub fn content_to_json(content) {
  case content {
    Article(d, s, t, ta, title) ->
      json.object([
        #(
          "article",
          json.object([
            #("description", json.nullable(d, json.string)),
            #("source", json.string(s)),
            #(
              "thumbnail",
              json.nullable(t, fn(t) { json.string(image_to_urn(t)) }),
            ),
            #("thumbnailAltText", json.nullable(ta, json.string)),
            #("title", json.string(title)),
          ]),
        ),
      ])
  }
}

pub type Visability {
  Connections
  Public
  LoggedIn
  Container
}

pub fn visability_to_string(visability) {
  case visability {
    Connections -> "CONNECTIONS"
    Public -> "PUBLIC"
    LoggedIn -> "LOGGEDIN"
    Container -> "CONTAINER"
  }
}

pub fn create_post(token, user_id, content, commentary) {
  let request = create_post_request(token, user_id, content, commentary)
  use response <- t.do(t.fetch(request))
  use response <- t.try(create_post_response(response))
  t.Done(response)
}

// how to set articles
// https://learn.microsoft.com/en-us/linkedin/marketing/integrations/ads/advertising-targeting/version/article-ads-integrations?view=li-lms-2024-08&tabs=http#workflow
pub fn create_post_request(token, author, content, commentary) {
  let path = "/rest/posts"
  let data =
    json.object({
      let always = [
        #("author", json.string(author_to_urn(author))),
        #("commentary", json.string(commentary)),
        #(
          "distribution",
          json.object([
            #("feedDistribution", json.string("MAIN_FEED")),
            #("targetEntities", json.array([], json.string)),
            #("thirdPartyDistributionChannels", json.array([], json.string)),
          ]),
        ),
        #("isReshareDisabledByAuthor", json.bool(False)),
        #("lifecycleState", json.string("PUBLISHED")),
        #("visibility", json.string(visability_to_string(Public))),
      ]
      case content {
        Some(content) -> [#("content", content_to_json(content)), ..always]
        None -> always
      }
    })
  post(
    token,
    path,
    "application/json",
    json.to_string(data)
      |> bit_array.from_string,
  )
}

pub fn create_post_response(response: response.Response(BitArray)) {
  use id <- try(case response.status {
    201 ->
      case response.get_header(response, "x-restli-id") {
        Ok(id) -> Ok(id)
        _ -> Error(snag.new("no post id"))
      }
    status -> {
      case json.decode_bits(response.body, error_decoder) {
        Ok(message) -> snag.new(message)
        Error(reason) -> snag.new(string.inspect(reason))
      }
      |> snag.layer("post not created status: " <> int.to_string(status))
      |> Error
    }
  })
  Ok(id)
}

pub fn get_posts(token, author) {
  let request = get_posts_request(token, author)
  use response <- t.do(t.fetch(request))
  // use response <- t.try(get_posts_response(response))
  t.Done(response)
}

pub fn get_posts_request(token, author) {
  let path = "/rest/posts"

  get(token, path)
  |> request.set_header("X-RestLi-Method", "FINDER")
  |> request.set_query([
    #("author", author_to_urn(author)),
    #("q", "author"),
    #("count", "10"),
    #("sortBy", "LAST_MODIFIED"),
  ])
}

pub type Share {
  Share(id: String)
}

fn share_to_urn(share) {
  let Share(id) = share
  "urn:li:share:" <> id
}

pub fn get_post(token, share) {
  let request = get_post_request(token, share)
  use response <- t.do(t.fetch(request))
  // use response <- t.try(get_post_response(response))
  t.Done(response)
}

pub fn get_post_request(token, share) {
  let path = "/rest/posts/" <> uri.percent_encode(share_to_urn(share))

  get(token, path)
}

// images

pub fn upload_image(token, owner, bits) {
  use InitializeImageUpload(_exp, url, image) <- t.do(initialize_image_upload(
    token,
    owner,
  ))
  let assert Ok(uri) = uri.parse(url)
  let assert Ok(r) = request.from_uri(uri)
  let upload =
    r
    |> request.set_method(http.Post)
    |> request.prepend_header("Authorization", string.append("Bearer ", token))
    |> request.set_body(bits)
  use response <- t.do(t.fetch(upload))
  case response.status {
    201 -> {
      let assert "urn:li:image:" <> id = image
      t.done(Image(id))
    }
    _ ->
      t.abort(snag.new("failed to upload image")// bit_array.to_string(response.body)
      )
  }
}

pub fn initialize_image_upload(token, owner) {
  let request = initialize_image_upload_request(token, owner)
  use response <- t.do(t.fetch(request))
  use response <- t.try(initialize_image_upload_response(response))
  t.Done(response)
}

pub fn initialize_image_upload_request(token, owner) {
  let path = "/rest/images"
  let data =
    json.object([
      #(
        "initializeUploadRequest",
        json.object([#("owner", json.string(author_to_urn(owner)))]),
      ),
    ])
  post_json(token, path, data)
  |> request.set_query([#("action", "initializeUpload")])
}

pub type InitializeImageUpload {
  InitializeImageUpload(
    upload_url_expires_at: Int,
    upload_url: String,
    image: String,
  )
}

pub fn initialize_image_upload_decoder(raw) {
  dynamic.field(
    "value",
    dynamic.decode3(
      InitializeImageUpload,
      dynamic.field("uploadUrlExpiresAt", dynamic.int),
      dynamic.field("uploadUrl", dynamic.string),
      dynamic.field("image", dynamic.string),
    ),
  )(raw)
}

pub fn initialize_image_upload_response(response: response.Response(BitArray)) {
  use id <- try(case response.status {
    200 ->
      json.decode_bits(response.body, initialize_image_upload_decoder)
      |> result.map_error(fn(reason) {
        snag.new(string.inspect(reason))
        |> snag.layer("failed to decode initialize image upload ")
      })

    status -> {
      case json.decode_bits(response.body, error_decoder) {
        Ok(message) -> snag.new(message)
        Error(reason) -> snag.new(string.inspect(reason))
      }
      |> snag.layer("post not created status: " <> int.to_string(status))
      |> Error
    }
  })
  Ok(id)
}
