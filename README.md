# Midas

**A web framework for Gleam, Midas makes shiny things.**

[![Hex pm](http://img.shields.io/hexpm/v/Midas.svg?style=flat)](https://hex.pm/packages/midas)
[![Build Status](https://github.com/midas-framework/midas/workflows/test/badge.svg?branch=master)](https://github.com/midas-framework/midas/actions?query=workflow%3Atest)
[![IRC: #gleam-lang on chat.freenode.net](https://img.shields.io/badge/freenode%20chat-%23gleam--lang-indigo)](https://webchat.freenode.net/#gleam-lang)

- [Install from hex.pm](https://hex.pm/packages/midas)
- [Documentation available on hexdoc](https://hexdocs.pm/Midas)
- [Discuss on freenode](https://webchat.freenode.net/#gleam-lang)

## Overview

Midas is a framework to help you get started started building web applications in Gleam as quickly as possible, while focusing on simplicity in the long run.

The quickest way to get started is to run the [template project](https://github.com/midas-framework/template)

_Gleam is a young language so midas comes with postgresql client library, configuration support and module for implementing procsses and supervisors._

### Web Functionality

```rust
import midas/http
import midas/http.{Request, Response, Get, Post}

pub fn handle_request(request) {
  let Request(method: method, path: path, query: query, body: body ..) = request
  let segments = http.split_segments(path)
  let params = http.parse_query(result.unwrap(query, ""))

  case method, segments {
    Get, [] -> {
      let name = params
        |> list.key_find("name")
        |> result.unwrap("World")

      Response(
        status: 200,
        headers: [tuple("content-type", "text/plain")],
        body: string.concat(["Hello, ", name, "!"])
      )
    }

    Get, ["posts", id] -> {
      // Action to get a post by id
      todo
    }

    Post, ["posts"] -> {
      // Action to create a post
      todo
    }

    _, _ -> Response(
      status: 404,
      headers: [tuple("content-type", "text/plain")],
      body: "Nothing here."
    )
  }
}
```

### Config

**config.get_env**

Returns the system environment variables as a map

**config.required**

Try to fetch and parse a value from a map, return as a result.

**config.optional**

Try to fetch value from a map, returns the fallback value if not in the map.

```rust
let env = get_env()

let port = optional(env, "PORT", int.parse, 8080)
let Ok(secret_key_base) = required(env, "SECRET_KEY_BASE", fn(x) { Ok(x) })
```

### Start Midas

```rust
midas.start_link(handle_request, 8080)
```

### Start Midas in an erlang application

Normally you would want to start midas under your applications supervision tree.
Inside you application file `src/my_app_app.erl` add midas to your supervision tree.

```erlang
Handler = my_app@web:handle_request/1,
Port = 8080,

ChildSpecs = [
  #{
    id => midas,
    start => {midas, start_link, [Handler, Port]}}
],
```

## Tasks

- in a shell there is `r3:compile()` that should recompile source.
- start a shell with `--start-clean` to start no application and run task.
- can run eval "erl code" in release, a way of doing tasks
- task.sh

```
rel start
rel eval "myapp:"
```

## Development

This project is to validate the value of [Gleam](https://github.com/gleam-lang/gleam) as a language for web development. Currently I am investigating the shape of such a project and not filling in the details, top priorities are as follows.

- Process model: how to handle supervision? Should this be in Gleam, or erlang?
- Language features/Libraries: How ergnomic is Gleam for the whole process of making web applications in particular templating, using a database.

### Thoughts on processes

**These notes are not relevant to understanding Midas as a server or framework. The process code should eventually be extracted from this repo.**

Supervisors should not accept start functions that fail.

One thing that can cause failures is naming processes.
This is a big global state problem and probably could always happen,
but at the registered process level a naming failure should be a hard crash.

Potential an Ignore response is ok, for example not starting a server if the PORT is not set.
There is no reason the supervisor can't handle this but it might be a nicer grouping of logic to put this in the particular process.
This is similar to the logic that lead to child_specs

Having the init and init_ack functionality causes alot of delay and the supervisor has to watch for more race conditions.

config typed properly

```rust
let Ok(pid) = postgresql.start_client(database_url: String)

// Replace that unsafe start API with two step process
// 1. outside supervision tree
let Ok(db_config) = postgresql.parse_url(database_url: String)

// 2.
let pid = postgresql.start_client(db_config)
```

There are some good discussion on this around,
need to look for more on less things being required at startup.
users of the pg_client are better of reporting the errors.

##### Request handlers

These are tasks not "actors", they shouldn't have an open inbox but deal in gen:replies promises.

Super general inbox setup

```rust
let tuple(receive, resolve) = promise.new()

process.spawn(fn(){
  resolve(5)
})

let Ok(value) = receive(Infinity)
```

A sendable trait could be really useful here.

### Resources http://joeduffyblog.com/2015/11/19/asynchronous-everything/
