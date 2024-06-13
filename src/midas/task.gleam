import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/uri.{type Uri}
import snag.{type Snag}

pub type Effect(a) {
  Done(a)
  Abort(Snag)
  Bundle(
    module: String,
    function: String,
    resume: fn(Result(String, String)) -> Effect(a),
  )
  Follow(uri: String, resume: fn(Result(Uri, Nil)) -> Effect(a))
  Fetch(
    request: Request(BitArray),
    resume: fn(Result(Response(BitArray), FetchError)) -> Effect(a),
  )
  Log(message: String, fn(Result(Nil, Nil)) -> Effect(a))
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
    Log(lift, resume) -> Log(lift, fn(reply) { do(resume(reply), then) })
    Zip(lift, resume) -> Zip(lift, fn(reply) { do(resume(reply), then) })
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

pub fn zip(message) {
  Zip(message, result_to_effect(_, zip_error_reason))
}

fn zip_error_reason(_: Nil) {
  snag.new("Failed to zip.")
}
