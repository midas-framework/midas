import gleam/http/request.{Request}
import gleam/list
import gleam/result
import midas/effect as e
import snag

/// A task is an effect that returns a result.
/// All errors in a task are snag Errors.
pub type Task(return, key) =
  e.Effect(snag.Result(return), key)

pub fn do(eff, next) {
  e.do(eff, fn(value) {
    case value {
      Ok(v) -> next(v)
      Error(reason) -> e.Done(Error(reason))
    }
  })
}

pub fn try(result, then) {
  case result {
    Ok(value) -> then(value)
    Error(reason) -> e.Done(Error(reason))
  }
}

pub fn done(value) {
  e.Done(Ok(value))
}

pub fn abort(reason) {
  e.Done(Error(reason))
}

pub fn each(items, run) {
  sequential(list.map(items, run))
}

pub fn sequential(tasks: List(Task(return, key))) -> Task(List(return), key) {
  do_sequential(tasks, [])
}

fn do_sequential(tasks, acc) {
  case tasks {
    [] -> e.Done(Ok(list.reverse(acc)))
    [next, ..rest] -> {
      case next {
        e.Done(Ok(x)) -> do_sequential(rest, [x, ..acc])
        e.Done(Error(reason)) -> e.Done(Error(reason))
        e.Bundle(m, f, then) ->
          e.Bundle(m, f, fn(reply) { do_sequential([then(reply), ..rest], acc) })
        e.ExportJsonWebKey(k, then) ->
          e.ExportJsonWebKey(k, fn(reply) {
            do_sequential([then(reply), ..rest], acc)
          })
        e.Fetch(value, then) ->
          e.Fetch(value, fn(reply) { do_sequential([then(reply), ..rest], acc) })
        e.Follow(value, then) ->
          e.Follow(value, fn(reply) {
            do_sequential([then(reply), ..rest], acc)
          })
        e.GenerateKeyPair(a, e, u, then) ->
          e.GenerateKeyPair(a, e, u, fn(reply) {
            do_sequential([then(reply), ..rest], acc)
          })
        e.Hash(a, b, then) ->
          e.Hash(a, b, fn(reply) { do_sequential([then(reply), ..rest], acc) })
        e.List(value, then) ->
          e.List(value, fn(reply) { do_sequential([then(reply), ..rest], acc) })
        e.Log(value, then) ->
          e.Log(value, fn(reply) { do_sequential([then(reply), ..rest], acc) })
        e.Read(value, then) ->
          e.Read(value, fn(reply) { do_sequential([then(reply), ..rest], acc) })
        e.Serve(p, h, then) ->
          e.Serve(p, h, fn(reply) { do_sequential([then(reply), ..rest], acc) })
        e.Sign(a, k, d, then) ->
          e.Sign(a, k, d, fn(reply) {
            do_sequential([then(reply), ..rest], acc)
          })
        e.StrongRandom(l, then) ->
          e.StrongRandom(l, fn(reply) {
            do_sequential([then(reply), ..rest], acc)
          })
        e.UnixNow(then) ->
          e.UnixNow(fn(reply) { do_sequential([then(reply), ..rest], acc) })
        e.Visit(uri, then) ->
          e.Visit(uri, fn(reply) { do_sequential([then(reply), ..rest], acc) })
        e.Write(f, b, then) ->
          e.Write(f, b, fn(reply) { do_sequential([then(reply), ..rest], acc) })
        e.Zip(value, then) ->
          e.Zip(value, fn(reply) { do_sequential([then(reply), ..rest], acc) })
      }
    }
  }
}

fn result_to_effect(result, error_handler) {
  e.Done(result.map_error(result, error_handler))
}

pub fn bundle(m, f) {
  e.Bundle(m, f, result_to_effect(_, snag.new))
}

pub fn export_jwk(key) {
  e.ExportJsonWebKey(key, result_to_effect(_, export_jwk_error_reason))
}

fn export_jwk_error_reason(message) {
  snag.new("Failed to export_key: " <> message)
}

pub fn fetch(request) {
  e.Fetch(request, result_to_effect(_, fetch_error_reason))
}

fn fetch_error_reason(reason) {
  case reason {
    e.NetworkError(message) -> "Network Error: " <> message
    e.UnableToReadBody -> "UnableToReadBody"
    e.NotImplemented -> "NotImplemented"
  }
  |> snag.new
}

pub fn follow(uri) {
  e.Follow(uri, result_to_effect(_, follow_error_reason))
}

fn follow_error_reason(_: Nil) {
  snag.new("Failed to parse uri.")
}

pub fn log(message) {
  e.Log(message, result_to_effect(_, log_error_reason))
}

fn log_error_reason(_: Nil) {
  snag.new("Failed to log.")
}

pub fn list(file) {
  e.List(file, result_to_effect(_, list_error_reason))
}

fn list_error_reason(message) {
  snag.new("Failed to list: " <> message)
}

pub fn generate_keypair(algorithm, extractable, usages) {
  e.GenerateKeyPair(algorithm, extractable, usages, result_to_effect(
    _,
    generate_keypair_error_reason,
  ))
}

fn generate_keypair_error_reason(message) {
  snag.new("Failed to generate_keypair: " <> message)
}

pub fn hash(algorithm, bytes) {
  e.Hash(algorithm, bytes, result_to_effect(_, hash_error_reason))
}

fn hash_error_reason(message) {
  snag.new("Failed to hash: " <> message)
}

pub fn read(file) {
  e.Read(file, result_to_effect(_, read_error_reason))
}

fn read_error_reason(message) {
  snag.new("Failed to read: " <> message)
}

pub fn sign(algorithm, key, data) {
  e.Sign(algorithm, key, data, result_to_effect(_, sign_error_reason))
}

fn sign_error_reason(message) {
  snag.new("Failed to sign: " <> message)
}

pub fn serve(port, handle) {
  e.Serve(port, handle, result_to_effect(_, serve_error_reason))
}

fn serve_error_reason(message) {
  snag.new("Failed to start server: " <> message)
}

pub fn strong_random(length) {
  e.StrongRandom(length, result_to_effect(_, strong_error_reason))
}

fn strong_error_reason(message) {
  snag.new("Failed to generate strong random: " <> message)
}

pub fn proxy(task, scheme, host, port, prefix) {
  case task {
    e.Done(_) -> task
    e.Fetch(request, resume) -> {
      let request = via_proxy(request, scheme, host, port, prefix)
      e.Fetch(request, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    }
    e.Bundle(f, m, resume) ->
      e.Bundle(f, m, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    e.ExportJsonWebKey(k, resume) ->
      e.ExportJsonWebKey(k, fn(x) {
        proxy(resume(x), scheme, host, port, prefix)
      })
    e.Follow(lift, resume) ->
      e.Follow(lift, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    e.GenerateKeyPair(a, e, u, resume) ->
      e.GenerateKeyPair(a, e, u, fn(x) {
        proxy(resume(x), scheme, host, port, prefix)
      })
    e.Hash(a, b, resume) ->
      e.Hash(a, b, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    e.List(lift, resume) ->
      e.List(lift, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    e.Log(lift, resume) ->
      e.Log(lift, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    e.Read(lift, resume) ->
      e.Read(lift, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    e.Serve(p, h, resume) ->
      e.Serve(p, h, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    e.Sign(a, k, d, resume) ->
      e.Sign(a, k, d, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    e.StrongRandom(l, resume) ->
      e.StrongRandom(l, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    e.UnixNow(resume) ->
      e.UnixNow(fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    e.Visit(uri, resume) ->
      e.Visit(uri, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    e.Write(a, b, resume) ->
      e.Write(a, b, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    e.Zip(lift, resume) ->
      e.Zip(lift, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
  }
}

fn via_proxy(original, scheme, host, port, prefix) {
  let Request(method, headers, body, _scheme, _host, _port, path, query) =
    original
  Request(method, headers, body, scheme, host, port, prefix <> path, query)
}

pub fn unix_now() {
  e.UnixNow(done)
}

pub fn visit(message) {
  e.Visit(message, result_to_effect(_, visit_error_reason))
}

fn visit_error_reason(reason) {
  snag.new("Failed to visit: " <> reason)
}

pub fn write(file, bytes) {
  e.Write(file, bytes, result_to_effect(_, write_error_reason))
}

fn write_error_reason(message) {
  snag.new("Failed to write: " <> message)
}

pub fn zip(message) {
  e.Zip(message, result_to_effect(_, zip_error_reason))
}

fn zip_error_reason(_: Nil) {
  snag.new("Failed to zip.")
}
