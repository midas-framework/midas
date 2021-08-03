# Midas - web development with Gleam

Gleam brings type safety to erlang.
This is a fantastic combination for rapidly building robust web applications.
We've been doing it since Gleam 0.10.0 at [plummail.co](https://plummail.co)

This repo **DOES NOT** contain a framework for you to start building web applications.
We started building a framework but found that it was not necessary (yet) and instead focused on contributing to back to other Gleam libraries.

Instead this repo is a guide to web development in Gleam.

### Gleam & Mix

We use Mix because Elixir has some great libraries that are easy to call from Gleam, and it has been the easiest way to have a project with both languages.

- Start a project using `mix new my_app --sup`
- Add [mix_gleam](https://github.com/gleam-lang/mix_gleam) and follow their instructions.

### A Server

Battle tested web servers are an Erlang speciality, there are Gleam wrappers for [Cowboy](https://hex.pm/packages/gleam_cowboy), [Elli](https://hex.pm/packages/gleam_elli) and [Plug](https://hex.pm/packages/gleam_plug).

Adding Cowboy to your supervision tree so that Mix will start it.

```elixir
  children = [
    # ...
    %{
      id: :cowboy,
      start: {
        :gleam@http@cowboy,
        :start, [&:my_app@web@router.handle(&1, config), 8080]
      }
    }
  ]
```

*Note `:my_app@web@router.handle` is a function on a gleam module, we will cover it in the next section.*

### Routing, Action, Controllers

The [gleam_http]() library defines request and response types for HTTP.
The utilities in this library and the power of pattern matching is everything we use.

```rust
fn route(request, config) {
  case http.path_segments(request) {
    [] -> {
      io.debug("Do something for the homepage")
      http.response(200)
      |> Ok
    }
    ["user", user_id] -> {
      io.debug("Hello user")
      http.response(200)
      |> Ok
    }
    ["api" | rest] -> api_router.route(request, config)
  }
}

pub fn handle(request, config) {
  case route(request, config) {
    Ok(response) -> response
    Error(reason) -> todo("define your error response")
  }
}
```

We found it convenient to allow routes to return errors because it gives you early return when using the (extremely helpful) try syntax.

### Handling input

We don't normally create a controller or action module.
All parsing/rendering is done in the case statement show above and we call out to business logic functions. e.g.

```rust
// in case statement of router
["posts", "create"] -> {
  try json = parse_form(request)
  try params = create_post.params(json)
  try user_id = load_session(request, config.secret)
  try post = create_post.execute(topic, user_id)
  redirect(string.concat["posts", int.to_string(post.id)])
  |> Ok
}
```

**Note all of our functions at this level return the same Error type**.
The Error type is defined by our application, functions like `parse_form` are wrappers around `uri.parse_query` (part of Gleam standard library) that transform errors into our application specific Error.

### JSON

We maintain [gleam_json](https://github.com/midas-framework/gleam_json),
to handle JSON input we define application helpers than transform errors in the same way as `parse_form`

### Views

Gleam does not [(yet)](https://github.com/gleam-lang/gleam/issues/565) have any string interpolation or templating, the easiest way we found to work around this was to use [EExHTML](https://hex.pm/packages/eex_html) and wrap calls as external function calls. **Note this was not very convenient and we are currently not doing this because our service is just a JSON API.**

### Database Access

We use Postgresql and the [pgo library](https://github.com/erleans/pgo), there is a [gleam wrapper](https://github.com/gleam-experiments/pgo)

This does not give us models, we haven't missed them. (I would argue models have less value in a functional world, but different projects might miss them).

All of our SQL is hand written queries and we have helpers to make sure that errors are wrapped in out application specific error type.

```rust
pub fn insert_user(email_address) {
  let sql = "
    INSERT INTO users (email_address)
    VALUES ($1)
    RETURNING id, email_address
  "

  let args = [pgo.text(email_address)]
  try [identifier] = run_sql(sql, args, row_to_user)
}

pub fn row_to_user(row) {
  assert Ok(id) = dynamic.element(row, 0)
  assert Ok(id) = dynamic.int(id)
  assert Ok(email_address) = dynamic.element(row, 1)
  assert Ok(email_address) = dynamic.string(email_address)
  Ok(User(id, email_address))
}
```

### Development process

To start the application run `iex -S mix`.

There is no live reloading set up, we type `recompile()` in the shell.
Between the assurances of the type system and our tests most of the time we start it up it's already working so manually typing recompile works for us.

### Final Note

Scaffolding a project that included sessions/flashes/etc would be great.
We think Midas will become a Gleam web framework it just hasn't yet.
The rest of this repo contains experiments on some of the pieces that framework would need.
