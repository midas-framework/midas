# Midas

A Web Framework for Gleam applications

[Hex](https://hex.pm/packages/midas)

**Very early software, you WILL find things missing. See [Development](#development) for details**

## Quick start

Add `midas` to your list of dependencies in `rebar.config`

```erlang
{deps, [
    midas
]}.
```

### Define a handler function

```rust
// src/my_app/web.geam

import midas
import midas/request.{Request}
import midas/respose.{Response}

pub fn handle_request(request: Request) -> Response {
  let Request(authority: _, path: path, headers: _) = request
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
