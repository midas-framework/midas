//// The effect module defines all the side effects supported by Midas.
//// It also defines functions to compose effectful functions
//// 
//// *Gleam does not offer extensible types and so adding new types will require contribution to this project.*
//// 
//// Use this module to explicitly handle errors, use the `midas/task` module for a simpler API where all errors are snag errors.

import filepath
import gleam/http/request.{type Request, Request}
import gleam/http/response.{type Response}
import gleam/json
import gleam/list
import gleam/option.{type Option}
import gleam/uri.{type Uri}

pub type HashAlgorithm {
  Sha1
  Sha256
  Sha384
  Sha512
}

pub type KeyPair(key) {
  KeyPair(public: key, private: key)
}

pub type KeyPairAlgorithm {
  EcKeyGenParams(name: String, named_curve: String)
}

pub type KeyUsage {
  CanEncrypt
  CanDecrypt
  CanSign
  CanVerify
  CanDeriveKey
  CanDeriveBits
  CanWrapKey
  CanUnwrapKey
}

pub type SignAlgorithm {
  EcdsaParams(hash: HashAlgorithm)
}

pub type Effect(a, key) {
  Done(a)
  Bundle(
    module: String,
    function: String,
    resume: fn(Result(String, String)) -> Effect(a, key),
  )
  ExportJsonWebKey(
    key: key,
    resume: fn(Result(json.Json, String)) -> Effect(a, key),
  )
  Follow(uri: Uri, resume: fn(Result(Uri, Nil)) -> Effect(a, key))
  Fetch(
    request: Request(BitArray),
    resume: fn(Result(Response(BitArray), FetchError)) -> Effect(a, key),
  )
  GenerateKeyPair(
    algorithm: KeyPairAlgorithm,
    extractable: Bool,
    usages: List(KeyUsage),
    resume: fn(Result(KeyPair(key), String)) -> Effect(a, key),
  )
  Hash(
    algorithm: HashAlgorithm,
    bytes: BitArray,
    resume: fn(Result(BitArray, String)) -> Effect(a, key),
  )
  List(
    directory: String,
    resume: fn(Result(List(String), String)) -> Effect(a, key),
  )
  Log(message: String, resume: fn(Result(Nil, Nil)) -> Effect(a, key))
  Read(file: String, resume: fn(Result(BitArray, String)) -> Effect(a, key))
  Serve(
    port: Option(Int),
    handle: fn(Request(BitArray)) -> Response(BitArray),
    resume: fn(Result(Nil, String)) -> Effect(a, key),
  )
  Sign(
    algorithm: SignAlgorithm,
    key: key,
    data: BitArray,
    resume: fn(Result(BitArray, String)) -> Effect(a, key),
  )
  StrongRandom(
    length: Int,
    resume: fn(Result(BitArray, String)) -> Effect(a, key),
  )
  Write(
    file: String,
    bytes: BitArray,
    resume: fn(Result(Nil, String)) -> Effect(a, key),
  )
  Visit(uri: Uri, resume: fn(Result(Nil, String)) -> Effect(a, key))
  Zip(
    files: List(#(String, BitArray)),
    resume: fn(Result(BitArray, Nil)) -> Effect(a, key),
  )
  UnixNow(resume: fn(Int) -> Effect(a, key))
}

pub fn bundle(module, function) {
  Bundle(module, function, Done)
}

pub fn export_jwk(key) {
  ExportJsonWebKey(key, Done)
}

pub fn fetch(request) {
  Fetch(request, Done)
}

pub fn follow(uri) {
  Follow(uri, Done)
}

pub fn generate_keypair(algorithm, extractable, usages) {
  GenerateKeyPair(algorithm, extractable, usages, Done)
}

pub fn hash(algorithm, bytes) {
  Hash(algorithm, bytes, Done)
}

pub fn list(directory) {
  List(directory, Done)
}

pub fn log(message) {
  Log(message, Done)
}

pub fn read(file) {
  Read(file, Done)
}

pub fn serve(port, handle) {
  Serve(port, handle, Done)
}

pub fn sign(algorithm, key, data) {
  Sign(algorithm, key, data, Done)
}

pub fn strong_random(length) {
  StrongRandom(length, Done)
}

pub fn write(file, bytes) {
  Write(file, bytes, Done)
}

pub fn visit(uri) {
  Visit(uri, Done)
}

pub fn zip(files) {
  Zip(files, Done)
}

pub fn unix_now() {
  UnixNow(Done)
}

pub type FetchError {
  NetworkError(String)
  UnableToReadBody
  NotImplemented
}

pub fn do(eff, next) {
  case eff {
    Done(value) -> next(value)
    Bundle(m, f, resume) -> Bundle(m, f, fn(reply) { do(resume(reply), next) })
    ExportJsonWebKey(k, resume) ->
      ExportJsonWebKey(k, fn(reply) { do(resume(reply), next) })
    Follow(lift, resume) -> Follow(lift, fn(reply) { do(resume(reply), next) })
    Fetch(lift, resume) -> Fetch(lift, fn(reply) { do(resume(reply), next) })
    GenerateKeyPair(a, e, u, resume) ->
      GenerateKeyPair(a, e, u, fn(reply) { do(resume(reply), next) })
    Hash(algorithm, bytes, resume) ->
      Hash(algorithm, bytes, fn(reply) { do(resume(reply), next) })
    List(lift, resume) -> List(lift, fn(reply) { do(resume(reply), next) })
    Log(lift, resume) -> Log(lift, fn(reply) { do(resume(reply), next) })
    Read(lift, resume) -> Read(lift, fn(reply) { do(resume(reply), next) })
    Serve(port, handle, resume) ->
      Serve(port, handle, fn(reply) { do(resume(reply), next) })
    Sign(a, k, d, resume) ->
      Sign(a, k, d, fn(reply) { do(resume(reply), next) })
    StrongRandom(length, resume) ->
      StrongRandom(length, fn(reply) { do(resume(reply), next) })
    UnixNow(resume) -> UnixNow(fn(reply) { do(resume(reply), next) })
    Visit(uri, resume) -> Visit(uri, fn(reply) { do(resume(reply), next) })
    Write(file, bytes, resume) ->
      Write(file, bytes, fn(reply) { do(resume(reply), next) })
    Zip(lift, resume) -> Zip(lift, fn(reply) { do(resume(reply), next) })
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
        Bundle(m, f, do) ->
          Bundle(m, f, fn(reply) { do_sequential([do(reply), ..rest], acc) })
        ExportJsonWebKey(k, do) ->
          ExportJsonWebKey(k, fn(reply) {
            do_sequential([do(reply), ..rest], acc)
          })
        Fetch(value, do) ->
          Fetch(value, fn(reply) { do_sequential([do(reply), ..rest], acc) })
        Follow(value, do) ->
          Follow(value, fn(reply) { do_sequential([do(reply), ..rest], acc) })
        GenerateKeyPair(a, e, u, do) ->
          GenerateKeyPair(a, e, u, fn(reply) {
            do_sequential([do(reply), ..rest], acc)
          })
        Hash(a, b, do) ->
          Hash(a, b, fn(reply) { do_sequential([do(reply), ..rest], acc) })
        List(value, do) ->
          List(value, fn(reply) { do_sequential([do(reply), ..rest], acc) })
        Log(value, do) ->
          Log(value, fn(reply) { do_sequential([do(reply), ..rest], acc) })
        Read(value, do) ->
          Read(value, fn(reply) { do_sequential([do(reply), ..rest], acc) })
        Serve(p, h, do) ->
          Serve(p, h, fn(reply) { do_sequential([do(reply), ..rest], acc) })
        Sign(a, k, d, do) ->
          Sign(a, k, d, fn(reply) { do_sequential([do(reply), ..rest], acc) })
        StrongRandom(l, do) ->
          StrongRandom(l, fn(reply) { do_sequential([do(reply), ..rest], acc) })
        UnixNow(do) ->
          UnixNow(fn(reply) { do_sequential([do(reply), ..rest], acc) })
        Visit(uri, do) ->
          Visit(uri, fn(reply) { do_sequential([do(reply), ..rest], acc) })
        Write(f, b, do) ->
          Write(f, b, fn(reply) { do_sequential([do(reply), ..rest], acc) })
        Zip(value, do) ->
          Zip(value, fn(reply) { do_sequential([do(reply), ..rest], acc) })
      }
    }
  }
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
  Serve(port, handle, Done)
}

pub fn proxy(task, scheme, host, port, prefix) {
  case task {
    Fetch(request, resume) -> {
      let request = via_proxy(request, scheme, host, port, prefix)
      Fetch(request, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    }
    Done(_) -> task
    Bundle(f, m, resume) ->
      Bundle(f, m, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    ExportJsonWebKey(k, resume) ->
      ExportJsonWebKey(k, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    Follow(lift, resume) ->
      Follow(lift, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    GenerateKeyPair(a, e, u, resume) ->
      GenerateKeyPair(a, e, u, fn(x) {
        proxy(resume(x), scheme, host, port, prefix)
      })
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
    Sign(a, k, d, resume) ->
      Sign(a, k, d, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    StrongRandom(l, resume) ->
      StrongRandom(l, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    UnixNow(resume) ->
      UnixNow(fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    Visit(uri, resume) ->
      Visit(uri, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    Write(a, b, resume) ->
      Write(a, b, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
    Zip(lift, resume) ->
      Zip(lift, fn(x) { proxy(resume(x), scheme, host, port, prefix) })
  }
}

fn via_proxy(original, scheme, host, port, prefix) {
  let Request(method, headers, body, _scheme, _host, _port, path, query) =
    original
  Request(method, headers, body, scheme, host, port, prefix <> path, query)
}
