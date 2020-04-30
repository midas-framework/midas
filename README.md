# Midas

**A web framework for Gleam, Midas makes shiny things.**

[![Hex pm](http://img.shields.io/hexpm/v/Midas.svg?style=flat)](https://hex.pm/packages/midas)

[![Build Status](https://github.com/midas-framework/midas/workflows/test/badge.svg?branch=master)](https://github.com/midas-framework/midas/actions?query=workflow%3Atest)

[![IRC: #gleam-lang on chat.freenode.net](https://img.shields.io/badge/freenode%20chat-%23gleam--lang-indigo)](https://webchat.freenode.net/#gleam-lang)

- [Install from hex.pm](https://hex.pm/packages/midas)
- [Documentation available on hexdoc](https://hexdocs.pm/Midas)
- [Discuss on freenode](https://webchat.freenode.net/#gleam-lang)

## Quick start

Add `midas` to your list of dependencies in `rebar.config`

```erlang
{deps, [
    midas
]}.
```

### Define a handler function

```rust
import midas/http
import midas/http.{Request, Response, GET, POST}

pub fn handle_request(request: Request) -> Response {
  let Request(method: method, path: path, ..) = request
  case tuple(method, http.path_segments(path)) {
    tuple(GET, []) -> {
      Response(
        status: 200,
        headers: [tuple("content-type", "text/plain")]
        body: "Hello, World!"
      )
    }

    ["greet", name] -> {
      let params = http.query_params(query)
      Response(
        status: 200,
        headers: [tuple("content-type", "text/plain")]
        body: string.concat(["Hello, ", name, "!"])
      )
    }
    _ -> Response(
      status: 404,
      headers: [tuple("content-type", "text/plain")],
      body: "Nothing here."
    )
  }
}
```

-session submodule

- csrf
- flash
- get_form
- get_header x-forwarded
- set-json payload
- basic auth
- get header auth
- get header user-agent / origin
- set header (lenses)

```rust
// src/my_app/web.gleam

import midas
import midas/request.{Request}
import midas/respose.{Response}

pub fn handle_request(request: Request) -> Response {
  let Request(path: path, ..) = request
  case path {
    "/" -> Response(status: 200, headers: [], body: "Hello, World!")
    _ -> Response(status: 404, headers: [], body: "Nothing here.")
  }
}
```

### Start a service

```rust
midas.start_link(handle_request, 8080)
```

Normally you would want to start midas under your applications supervision tree.
Inside you application file `src/my_app_app.erl` add midas to your supervision tree.

```erlang
Handler = fun (Request) -> my_app@web:handle_request(Request) end,
Port = 8080,

ChildSpecs = [
  #{
    id => midas,
    start => {midas, start_link, [Handler, Port]}}
],
```

## Development

This project is to validate the value of [Gleam](https://github.com/gleam-lang/gleam) as a language for web development. Currently I am investigating the shape of such a project and not filling in the details, top priorities are as follows.

- Process model: how to handle supervision? Should this be in Gleam, or erlang?
- Language features/Libraries: How ergnomic is Gleam for the whole process of making web applications in particular templating, using a database.
