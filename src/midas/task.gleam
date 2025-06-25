import filepath
import gleam/http/request.{type Request, Request}
import gleam/http/response.{type Response}
import gleam/list
import gleam/option.{type Option}
import gleam/uri.{type Uri}
import snag.{type Snag}

pub type HashAlgorithm {
  SHA1
  SHA256
  SHA384
  SHA512
}

pub type Effect(a) {
  Done(a)
  Abort(Snag)
  Bundle(
    module: String,
    function: String,
    resume: fn(Result(String, String)) -> Effect(a),
  )
  // uri should become a uri type
  Follow(uri: String, resume: fn(Result(Uri, Nil)) -> Effect(a))
  Fetch(
    request: Request(BitArray),
    resume: fn(Result(Response(BitArray), FetchError)) -> Effect(a),
  )
  Hash(
    algorithm: HashAlgorithm,
    bytes: BitArray,
    resume: fn(Result(BitArray, String)) -> Effect(a),
  )
  List(directory: String, resume: fn(Result(List(String), String)) -> Effect(a))
  Log(message: String, resume: fn(Result(Nil, Nil)) -> Effect(a))
  Read(file: String, resume: fn(Result(BitArray, String)) -> Effect(a))
  Serve(
    port: Option(Int),
    handle: fn(Request(BitArray)) -> Response(BitArray),
    resume: fn(Result(Nil, String)) -> Effect(a),
  )
  StrongRandom(length: Int, resume: fn(Result(BitArray, String)) -> Effect(a))
  Write(
    file: String,
    bytes: BitArray,
    resume: fn(Result(Nil, String)) -> Effect(a),
  )
  Visit(uri: Uri, resume: fn(Result(Nil, String)) -> Effect(a))
  Zip(
    files: List(#(String, BitArray)),
    resume: fn(Result(BitArray, Nil)) -> Effect(a),
  )
}

pub type FetchError {
  NetworkError(String)
  UnableToReadBody
  NotImplemented
}

pub fn do(eff, then) {
  case eff {
    Done(value) -> then(value)
    Abort(reason) -> Abort(reason)
    Bundle(m, f, resume) -> Bundle(m, f, fn(reply) { do(resume(reply), then) })
    Follow(lift, resume) -> Follow(lift, fn(reply) { do(resume(reply), then) })
    Fetch(lift, resume) -> Fetch(lift, fn(reply) { do(resume(reply), then) })
    Hash(algorithm, bytes, resume) ->
      Hash(algorithm, bytes, fn(reply) { do(resume(reply), then) })
    List(lift, resume) -> List(lift, fn(reply) { do(resume(reply), then) })
    Log(lift, resume) -> Log(lift, fn(reply) { do(resume(reply), then) })
    Read(lift, resume) -> Read(lift, fn(reply) { do(resume(reply), then) })
    Serve(port, handle, resume) ->
      Serve(port, handle, fn(reply) { do(resume(reply), then) })
    StrongRandom(length, resume) ->
      StrongRandom(length, fn(reply) { do(resume(reply), then) })
    Write(file, bytes, resume) ->
      Write(file, bytes, fn(reply) { do(resume(reply), then) })
    Visit(uri, resume) -> Visit(uri, fn(reply) { do(resume(reply), then) })
    Zip(lift, resume) -> Zip(lift, fn(reply) { do(resume(reply), then) })
  }
}

pub fn each(items, run) {
  sequential(list.map(items, run))
}

pub fn sequential(tasks) {
  do_sequential(tasks, [])
}

fn do_sequential(tasks, acc) {
  case tasks {
    [] -> Done(list.reverse(acc))
    [next, ..rest] -> {
      case next {
        Done(x) -> do_sequential(rest, [x, ..acc])
        Abort(reason) -> Abort(reason)
        Bundle(m, f, then) ->
          Bundle(m, f, fn(reply) { do_sequential([then(reply), ..rest], acc) })
        Fetch(value, then) ->
          Fetch(value, fn(reply) { do_sequential([then(reply), ..rest], acc) })
        Follow(value, then) ->
          Follow(value, fn(reply) { do_sequential([then(reply), ..rest], acc) })
        Hash(a, b, then) ->
          Hash(a, b, fn(reply) { do_sequential([then(reply), ..rest], acc) })
        List(value, then) ->
          List(value, fn(reply) { do_sequential([then(reply), ..rest], acc) })
        Log(value, then) ->
          Log(value, fn(reply) { do_sequential([then(reply), ..rest], acc) })
        Read(value, then) ->
          Read(value, fn(reply) { do_sequential([then(reply), ..rest], acc) })
        Serve(p, h, then) ->
          Serve(p, h, fn(reply) { do_sequential([then(reply), ..rest], acc) })
        StrongRandom(l, then) ->
          StrongRandom(l, fn(reply) {
            do_sequential([then(reply), ..rest], acc)
          })
        Write(f, b, then) ->
          Write(f, b, fn(reply) { do_sequential([then(reply), ..rest], acc) })
        Visit(uri, then) ->
          Visit(uri, fn(reply) { do_sequential([then(reply), ..rest], acc) })
        Zip(value, then) ->
          Zip(value, fn(reply) { do_sequential([then(reply), ..rest], acc) })
      }
    }
  }
}

pub fn try(result, then) {
  case result {
    Ok(value) -> then(value)
    Error(reason) -> Abort(reason)
  }
}

fn result_to_effect(result, error_handler) {
  case result {
    Ok(value) -> Done(value)
    Error(reason) -> Abort(error_handler(reason))
  }
}

pub fn done(value) {
  Done(value)
}

pub fn abort(value) {
  Abort(value)
}

pub fn bundle(m, f) {
  Bundle(m, f, result_to_effect(_, snag.new))
}

pub fn fetch(request) {
  Fetch(request, result_to_effect(_, fetch_error_reason))
}

fn fetch_error_reason(reason) {
  case reason {
    NetworkError(message) -> "Network Error: " <> message
    UnableToReadBody -> "UnableToReadBody"
    NotImplemented -> "NotImplemented"
  }
  |> snag.new
}

pub fn follow(uri) {
  Follow(uri, result_to_effect(_, follow_error_reason))
}

fn follow_error_reason(_: Nil) {
  snag.new("Failed to parse uri.")
}

pub fn log(message) {
  Log(message, result_to_effect(_, log_error_reason))
}

fn log_error_reason(_: Nil) {
  snag.new("Failed to log.")
}

pub fn list(file) {
  List(file, result_to_effect(_, list_error_reason))
}

fn list_error_reason(message) {
  snag.new("Failed to list: " <> message)
}

pub fn hash(algorithm, bytes) {
  Hash(algorithm, bytes, result_to_effect(_, hash_error_reason))
}

fn hash_error_reason(message) {
  snag.new("Failed to hash: " <> message)
}

pub fn read(file) {
  Read(file, result_to_effect(_, read_error_reason))
}

fn read_error_reason(message) {
  snag.new("Failed to read: " <> message)
}

pub fn serve(port, handle) {
  Serve(port, handle, result_to_effect(_, serve_error_reason))
}

pub fn serve_static(port, files) {
  let handle = fn(request) {
    let request.Request(path: path, ..) = request
    let mime = case filepath.extension(path) {
      Ok(".js") -> "application/javascript"
      Ok(_) -> "application/octet-stream"
      // index pages are assumed to be html
      Error(Nil) -> "text/html"
    }
    case list.key_find(files, path) {
      Ok(content) ->
        response.new(200)
        |> response.set_header("content-type", mime)
        |> response.set_body(content)
      Error(Nil) ->
        response.new(404)
        |> response.set_body(<<>>)
    }
  }
  serve(port, handle)
}

fn serve_error_reason(message) {
  snag.new("Failed to start server: " <> message)
}

pub fn strong_random(length) {
  StrongRandom(length, result_to_effect(_, strong_error_reason))
}

fn strong_error_reason(message) {
  snag.new("Failed to generate strong random: " <> message)
}

pub fn proxy(task, scheme, host, port, prefix) {
  case task {
    Fetch(request, resume) -> {
      let request = via_proxy(request, scheme, host, port, prefix)
      Fetch(request, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    }
    Abort(_) | Done(_) -> task
    Bundle(f, m, resume) ->
      Bundle(f, m, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    Follow(lift, resume) ->
      Follow(lift, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    Hash(a, b, resume) ->
      Hash(a, b, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    List(lift, resume) ->
      List(lift, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    Log(lift, resume) ->
      Log(lift, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    Read(lift, resume) ->
      Read(lift, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    Serve(p, h, resume) ->
      Serve(p, h, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    StrongRandom(l, resume) ->
      StrongRandom(l, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    Write(a, b, resume) ->
      Write(a, b, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    Visit(uri, resume) ->
      Visit(uri, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    Zip(lift, resume) ->
      Zip(lift, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
  }
}

fn via_proxy(original, scheme, host, port, prefix) {
  let Request(method, headers, body, _scheme, _host, _port, path, query) =
    original
  Request(method, headers, body, scheme, host, port, prefix <> path, query)
}

pub fn write(file, bytes) {
  Write(file, bytes, result_to_effect(_, write_error_reason))
}

fn write_error_reason(message) {
  snag.new("Failed to write: " <> message)
}

pub fn visit(message) {
  Visit(message, result_to_effect(_, visit_error_reason))
}

fn visit_error_reason(reason) {
  snag.new("Failed to visit: " <> reason)
}

pub fn zip(message) {
  Zip(message, result_to_effect(_, zip_error_reason))
}

fn zip_error_reason(_: Nil) {
  snag.new("Failed to zip.")
}

pub fn expect_done(task) {
  case task {
    Done(value) -> Ok(value)
    other -> Error(other)
  }
}

pub fn expect_abort(task) {
  case task {
    Abort(reason) -> Ok(reason)
    other -> Error(other)
  }
}

pub fn expect_bundle(task) {
  case task {
    Bundle(module, function, resume) -> Ok(#(module, function, resume))
    other -> Error(other)
  }
}

pub fn expect_follow(task) {
  case task {
    Follow(lift, resume) -> Ok(#(lift, resume))
    other -> Error(other)
  }
}

pub fn expect_fetch(task) {
  case task {
    Fetch(lift, resume) -> Ok(#(lift, resume))
    other -> Error(other)
  }
}

pub fn expect_hash(task) {
  case task {
    Hash(algorithm, bytes, resume) -> Ok(#(algorithm, bytes, resume))
    other -> Error(other)
  }
}

pub fn expect_list(task) {
  case task {
    List(lift, resume) -> Ok(#(lift, resume))
    other -> Error(other)
  }
}

pub fn expect_log(task) {
  case task {
    Log(lift, resume) -> Ok(#(lift, resume))
    other -> Error(other)
  }
}

pub fn expect_read(task) {
  case task {
    Read(lift, resume) -> Ok(#(lift, resume))
    other -> Error(other)
  }
}

pub fn expect_serve(task) {
  case task {
    Serve(port, handle, resume) -> Ok(#(port, handle, resume))
    other -> Error(other)
  }
}

pub fn expect_strong_random(task) {
  case task {
    StrongRandom(length, resume) -> Ok(#(length, resume))
    other -> Error(other)
  }
}

pub fn expect_write(task) {
  case task {
    Write(file, bytes, resume) -> Ok(#(file, bytes, resume))
    other -> Error(other)
  }
}

pub fn expect_visit(task) {
  case task {
    Visit(uri, resume) -> Ok(#(uri, resume))
    other -> Error(other)
  }
}

pub fn expect_zip(task) {
  case task {
    Zip(lift, resume) -> Ok(#(lift, resume))
    other -> Error(other)
  }
}
