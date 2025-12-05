# Midas

[![Package Version](https://img.shields.io/hexpm/v/midas)](https://hex.pm/packages/midas)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/midas/)

Midas tackles the function colouring problem by separating defining and running effectful code.
It provides a description of effects, as well functions for composing effectful computation.

This separation has several benefits:
- Reuse code across environments
- Switch out implementations, i.e. choose different HTTP clients
- No need to use mocks for testing.
- Allows middleware, for example to add spans for tracing.

```sh
gleam add midas@2
```

## Why Midas

Already sold jump to [Usage](#usage)

### The goal

Let's imagine we have some code that we want to run on JavaScript and the BEAM.
Also suppose that this code needs to make http requests.

Ideally we write something like the following.

```gleam
fn my_func(send){
  let request_a = request.new() |> request.set_path("/a")
  let result_a = send(request_a)
  let assert Ok(a) = result_a

  let request_b = request.new() |> request.set_path("/a")
  let result_b = send(request_b)
  let assert Ok(b) = result_b

  todo as "something with a and b"
}
```

The simple approach here is to pass the client at runtime so different HTTP clients can be used on each runtime.

Gleam has great HTTP clients, there is [gleam_httpc](https://hexdocs.pm/gleam_httpc/) for the BEAM
and [gleam_fetch](https://hexdocs.pm/gleam_fetch/) for JavaScript.

### The problem

The problem is function colouring. The two clients have different type signatures for their `send` function.
httpc returns a `Result(Response, _)` and fetch returns a `Promise(Result(Response, _))`.

This difference reflects underlying differences in the runtimes and I think is a good thing to capture this in the type system.
However this difference blocks us from reusing `my_func` as it is.

### The solution

Instead of passing in the client our code return data type with information on what to do next.
In this example our code is either finished or needs to make an HTTP request.
We call this returned type `Effect` and it has two variants `Fetch` and `Done`.

```gleam
pub type Effect(t) {
  Fetch(Request, fn(Result(Response, String)) -> Effect(t))
  Done(t)
}
```

To write our business logic we make use of `use` to cleanly compose our effects.

```gleam

pub fn my_func() {
  let request_a = request.new() |> request.set_path("/a")
  use result_a <- Fetch(request_a)
  let assert Ok(a) = result_a

  let request_b = request.new() |> request.set_path("/a")
  use result_b <- Fetch(request_b)
  let assert Ok(b) = result_b
  Done(todo as "something with a and b")
}
```

The `my_func` above only describes the computation but does not run it.
To run the function requires a `run` function, there are libraries for running tasks in different environments.

Simple run functions can be implemented as follows:

for BEAM
```gleam
pub fn run(effect) {
  case effect {
    Fetch(request, resume) -> run(resume(httpc.send(request)))
    Done(value) -> value
  }
}
```

for JS
```gleam
pub fn run(effect) {
  case effect {
    Fetch(request, resume) -> {
      use result <- promise.await(fetch.send(request))
      run(resume(result))
    }
    Done(value) -> promise.resolve(value)
  }
}
```

**Note:** The full midas effect type defines many side effects so your runner will either have to implement them, panic or return a default value for the task.

That's as far as we take this toy implementation, the rest of the documentation will cover using the actual library.

## Usage

Midas separates defining tasks from running tasks.
So first we shall define a task that makes a web request and logs when it has completed.

```gleam
import midas/task as t

pub fn task() {
  let request = // ...
  use response <- t.do(t.fetch(request))
  use Nil <- t.do(t.log("Fetched"))
  t.done(Nil)
}
```

To run this in the browser use [midas_browser](https://github.com/midas-framework/midas_browser):

```gleam
import midas/browser

pub fn main(){
  use result <- promise.await(browser.run(task()))
  case result {
    Ok(_) -> // ...
    Error(_) -> // ...
  }
}
```

## Testing

Because we have isolated all side effects testing is easy.
Instead of using any runner the simplest approach is to assert on each effect in turn.

First assert that the effect is as expected, for example that the effect is `Fetch` and that the request has the right path.
Then resume the program and assert on the next effect, or done if no more effects.

```gleam
import midas/effect as e

pub fn task_success_test() {
  let assert e.Fetch(request:, resume:) = task()
  assert request.path == "/a/b/c"
  
  let response = response.new(200)
  let assert e.Log(message:, resume:) = resume(Ok(response))
  assert messages == "Fetched"
  
  let assert e.Done(Ok(value)) = resume(Ok(Nil))
}

pub fn network_error_test() {
  let assert e.Fetch(request:, resume:) = task()
  assert request.path == "/a/b/c"

  let reason = e.NetworkError("something bad")
  let assert e.Done(Error(reason)) = resume(Error(reason))
  assert string.contains(snag.pretty_print(reason), "something bad")
}
```

## Working with errors

The `midas/task` module assumes that the return value of a task is a result.
All the helper functions assume that an error from an effect, such as a network error from a fetch, should be returned as an error from the task.
This is easier to work with and all effects are snags.
However this prevents you from recovering or implementing retries.

For more explicit error handling you can use the `midas/effect` module directly.

## A note of effects

The `midas/task` module defines an `Effect` type which represents all the effects that can be defined for any task.
Gleam doesn't have an extensible type so the `Effect` type is essentially a single namespace defining all possible effects.

This is not really a problem because any effect might return an error.
So it is always possible to return an error that indicates that the effect is not implemented by a given runner.

