# Midas

**Why**: Midas exists to speed up developers by eliminating complexity,
**How**: ...by extending type guarantees to all stages of development,
**What**: ...with a Gleam library to replace use of build and deploy tools like Bash/Make.

[![Package Version](https://img.shields.io/hexpm/v/midas)](https://hex.pm/packages/midas)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/midas/)

```sh
gleam add midas
```

## Task definition vs running a task

Midas separates defining tasks from running tasks.
The `midas/task` module is for defining tasks, this is normally the module you want to start with.

```gleam
import gleam/task as t

pub fn task() {
  let request = // ...
  use response <- t.do(t.fetch(request))
  use Nil <- t.do(t.log("Fetched"))
  t.done(Nil)
}
```

Running tasks is handled by modules specific to the environment the tasks are running in,
for example `midas/node` or `midas/browser`. 

This separation has several benefits:
- Run tasks in different environments
- Switch out implementations, i.e. choose different HTTP clients
- No need to use mocks for testing just use a test task runner.
- Allows middleware, for example to add spans for tracing

```gleam
import midas/node

pub fn main() {
  node.run(task())
}
```

## A note on SDKs

Libraries can use the task definition to specify calling an API without needing to choose a how the task is run.
i.e. there should be less cases where the library needs changing to run on JS or erlang.

For example using the twitter SDK.

```gleam
pub fn run() {
  use token <- t.do(twitter.authenticate(client_id, redirect_uri, scopes))
  use response <- t.do(twitter.user_by_username(token, "crowdhailer"))
  io.debug(response)
  t.Done(response)
}
```

You can follow this example on [gleamtours.com](todo) where the task runs in the browser.

## A note of effects

The `midas/task` module defines an `Effect` type which represents all the effects that can be defined for any task.
Gleam doesn't have an extensible type so the `Effect` type is essentially a single namespace defining all possible effects.

This is not really a problem because any effect might return an error.
So it is always possible to return an error that indicates that the effect is not implemented by a given runner.


## A note on errors

Using `t.fetch(request)` continues with a `Response` type and not a `Result`.
In case of an error the task aborts with an error based on the [snag library](https://hex.pm/packages/snag).
Having the same error type makes composing tasks much easier.

However if you task might recover in the case of an error you should drop down to the effect directly.
The return type using the effect directly has an informative error type that has not yet be cast to a `Snag`.
