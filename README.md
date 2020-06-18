# Midas

**A web framework for Gleam, Midas makes shiny things.**

[![Hex pm](http://img.shields.io/hexpm/v/Midas.svg?style=flat)](https://hex.pm/packages/midas)
[![Build Status](https://github.com/midas-framework/midas/workflows/test/badge.svg?branch=master)](https://github.com/midas-framework/midas/actions?query=workflow%3Atest)
[![IRC: #gleam-lang on chat.freenode.net](https://img.shields.io/badge/freenode%20chat-%23gleam--lang-indigo)](https://webchat.freenode.net/#gleam-lang)

- [Install from hex.pm](https://hex.pm/packages/midas)
- [Documentation available on hexdoc](https://hexdocs.pm/midas)
- [Discuss on freenode](https://webchat.freenode.net/#gleam-lang)

## Overview

Midas is a framework for building web applications that focuses on simplicity in the long run.

```rust
// src/my_app/web.gleam
import gleam/http
import gleam/http.{Get}
import midas

pub fn handle_request(request) {
  case http.method(request), http.path_segments(request) {
    Get, [] -> {
      http.response(200)
      |> http.set_body("Hello, World!")
    }
    _, _ -> {
      http.response(404)
      |> http.set_body("Nothing here :-(")
    }
  }
}

pub fn start_link() {
  midas.start_link(handle_request, 8080)
}
```

### Start Midas in an erlang application

Normally you would want to start midas under your applications supervision tree.
Inside you application file `src/my_app_app.erl` add midas to your supervision tree.

```erlang
ChildSpecs = [
  #{
    id => web,
    start => {my_app@web, start_link, []}
  }
]
```

Features included in this library.

- Helpers for HTTP Requests and Responses, see `gleam/http`.
- HTTP/1 server, *adapter to HTTP/2 server welcome*.
- Helpers for runtime configuration
- Postgresql client based on [pgo](https://github.com/erleans/pgo)
