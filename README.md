# Midas

[![Package Version](https://img.shields.io/hexpm/v/midas)](https://hex.pm/packages/midas)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/midas/)

Separate defining and running effectful code.
Midas builds on the continuation monad to abstract effectful code, so that it can be reused on JS and BEAM runtimes.

Benefits of abstracting effects include:
- Reuse code across environments
- Switch out implementations, i.e. choose different HTTP clients
- No need to use mocks for testing.
- Allows middleware, for example to add spans for tracing.

```sh
gleam add midas@3
```

### Example

Start with some business logic that is to be reused over JavaScript and the BEAM runtimes.
This logic is going to require making HTTP requests.

With continuations we can specify all our logic in a portable task.

```gleam
import midas/continuation.{type Continuation as K, done, try_then}
import midas/effect

pub type Context {
  Context(fetch: effect.Fetch(t))
}

fn task(context: Context) -> K(t,Result(String, effect.FetchError)) {
  let request_a = request.new() |> request.set_path("/a")
  let a = try_then(request_a)
  
  let request_b = request.new() |> request.set_path("/b")
  let b = try_then(request_b)

  done("Finished")
}
```

- This task is portable and runners for each of BEAM, JS and testing can be implemented.
- A `Context` type is not necessary but is useful if more than one effect is needed.
- The type `effect.Fetch(t)` is an alias for `fn(Request(BitArray)) -> K(t, Result(response.Response(BitArray), FetchError))`.
  `midas/effect` defines some useful effects but any effect can be specified.
- This task works over a continuation that returns a result, `done` and `try_then` are helpers for working with continuations of results.
  It is not necessary for continuations to return results. use `return` and `then` for bare values, this is useful in request handlers where all return values are an http response.

No let's look at our runners. First on the BEAM.
```gleam
import gleam/httpc

// mapping error reasons is a bit clunky.
// maybe one day gleam_http will define a common error type.
fn fetch(request) {
  case httpc.send_bits(request) {
    Ok(response) -> Ok(response)
    Error(httpc.InvalidUtf8Response) ->
      Error(effect.UnableToReadBody)
    Error(httpc.FailedToConnect(ip4: ConnectError, ip6: ConnectError)) ->
      Error(effect.NetworkError("failed to connect"))
    Error(httpc.ResponseTimeout) ->
      Error(effect.NetworkError("response timeout"))
  }
  |> return
}

pub fn beam_run(task) {
  task(Context(fetch:))(fn(x) { x })
}
```

And on JS

```gleam
import gleam/fetch

// mapping error reasons is a bit clunky.
// maybe one day gleam_http will define a common error type.
fn fetch(request) {
  fn(then){
    use response <- promise.await(fetch.send_bits(request))
    case response {
      Ok(response) -> {
        use response <- promise.await(fetch.read_bytes_body(request))
        then(response)
      }
      Error(reason) -> then(Error(reason))
    }
  }
}

pub fn js_run(task) {
  task(Context(fetch:))(promise.resolve)
}
```

## Testing

We can define a runner that doesn't make and side effect, instead just returing the current state of the task.
This is a defunctionalisation transformation.
It makes testing effects consistent and removes the need for mocking.

```gleam
pub type Effect(t) {
  Fetch(Request(BitArray), fn(Result(Response(BitArray, FetchError))) -> Effect(t))
  Pure(t)
}

pub fn run_test(task) {
  let context = Context(fetch: fn(request) { Fetch(request, _) })

  task(context)(Pure)
}

pub fn task_success_test() {
  let assert Fetch(request:, resume:) = run_test(task)
  assert request.path == "/a"
  
  let response = response.new(200)
  let assert Fetch(request:,resume) = resume(Ok(response))
  assert request.path == "/b"
  
  let response = response.new(200)  
  let assert Pure(Ok(value)) = resume(Ok(response))
}

pub fn network_error_test() {
  let assert Fetch(request:, resume:) = task()
  assert request.path == "/a"

  let reason = NetworkError("something bad")
  let assert Pure(Error(reason)) = resume(Error(reason))
  assert "something bad" == reason
}
```
