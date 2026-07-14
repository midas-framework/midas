import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/json
import gleam/option.{type Option}
import gleam/uri.{type Uri}
import midas/effect.{
  type FetchError, type HashAlgorithm, type KeyPair, type KeyPairAlgorithm,
  type KeyUsage, type SignAlgorithm,
}

pub type Effect(r, key) {
  Done(r)
  Bundle(
    module: String,
    function: String,
    resume: fn(Result(String, String)) -> Effect(r, key),
  )
  ExportJsonWebKey(key: key, resume: fn(json.Json) -> Effect(r, key))
  Follow(uri: Uri, resume: fn(Result(Uri, Nil)) -> Effect(r, key))
  Fetch(
    request: Request(BitArray),
    resume: fn(Result(Response(BitArray), FetchError)) -> Effect(r, key),
  )
  GenerateKeyPair(
    algorithm: KeyPairAlgorithm,
    extractable: Bool,
    usages: List(KeyUsage),
    resume: fn(Result(KeyPair(key), String)) -> Effect(r, key),
  )
  Hash(
    algorithm: HashAlgorithm,
    bytes: BitArray,
    resume: fn(BitArray) -> Effect(r, key),
  )

  Log(message: String, resume: fn(Nil) -> Effect(r, key))
  ReadDirectory(
    directory: String,
    resume: fn(Result(List(String), String)) -> Effect(r, key),
  )
  ReadFile(file: String, resume: fn(Result(BitArray, String)) -> Effect(r, key))
  Serve(
    port: Option(Int),
    handle: fn(Request(BitArray)) -> Response(BitArray),
    resume: fn(Result(Int, String)) -> Effect(r, key),
  )
  Sign(
    algorithm: SignAlgorithm,
    key: key,
    data: BitArray,
    resume: fn(Result(BitArray, String)) -> Effect(r, key),
  )
  StrongRandom(length: Int, resume: fn(BitArray) -> Effect(r, key))
  WriteFile(
    file: String,
    bytes: BitArray,
    resume: fn(Result(Nil, String)) -> Effect(r, key),
  )
  Visit(uri: Uri, resume: fn(Result(Nil, String)) -> Effect(r, key))
  Zip(
    files: List(#(String, BitArray)),
    resume: fn(Result(BitArray, Nil)) -> Effect(r, key),
  )
  UnixNow(resume: fn(Int) -> Effect(r, key))
}

pub fn bundle(module, function) {
  Bundle(module, function, _)
}

pub fn export_jwk(key) {
  ExportJsonWebKey(key, _)
}

pub fn fetch(request) {
  Fetch(request, _)
}

pub fn follow(uri) {
  Follow(uri, _)
}

pub fn generate_keypair(algorithm, extractable, usages) {
  GenerateKeyPair(algorithm, extractable, usages, _)
}

pub fn hash(algorithm, bytes) {
  Hash(algorithm, bytes, _)
}

pub fn log(message) {
  Log(message, _)
}

pub fn read_directory(directory) {
  ReadDirectory(directory, _)
}

pub fn read_file(file) {
  ReadFile(file, _)
}

pub fn serve(port, handle) {
  Serve(port, handle, _)
}

pub fn sign(algorithm, key, data) {
  Sign(algorithm, key, data, _)
}

pub fn strong_random(length) {
  StrongRandom(length, _)
}

pub fn unix_now() {
  UnixNow
}

pub fn write_file(file, bytes) {
  WriteFile(file, bytes, _)
}

pub fn visit(uri) {
  Visit(uri, _)
}

pub fn zip(files) {
  Zip(files, _)
}
