//// The effect module defines signatures for standard side effects.
//// 

import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/json
import gleam/option.{type Option}
import gleam/uri.{type Uri}
import midas/continuation.{type Continuation as K}

/// platform agnostic copy of the type in gleam_crypto.
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

pub type FetchError {
  NetworkError(String)
  UnableToReadBody
  NotImplemented
}

pub fn describe_fetch_error(reason) {
  case reason {
    NetworkError(message) -> "Network Error: " <> message
    UnableToReadBody -> "UnableToReadBody"
    NotImplemented -> "NotImplemented"
  }
}

/// Function accepts a module and function and returns bundled code.
/// 
/// Fallible as code might fail compilation
pub type Bundle(t) =
  fn(String, String) -> K(t, Result(String, String))

/// A function to export key material as JSON
/// 
/// All keys have a valid JSON encoding
pub type ExportJsonWebKey(t, key) =
  fn(key) -> K(t, json.Json)

pub type Fetch(t) =
  fn(Request(BitArray)) -> K(t, Result(response.Response(BitArray), FetchError))

/// Uri might not exist or resolve
pub type Follow(t) =
  fn(Uri) -> K(t, Result(Uri, String))

/// algorithm, extractable, uses may fail if uses does not match algorithm
pub type GenerateKeypair(t, key) =
  fn(KeyPairAlgorithm, Bool, List(KeyUsage)) ->
    K(t, Result(KeyPair(key), String))

pub type Hash(t) =
  fn(HashAlgorithm, BitArray) -> K(t, BitArray)

/// May fail for non existant directory or file system permissions.
pub type Log(t) =
  fn(String) -> K(t, Nil)

/// May fail for non existant directory or file system permissions.
pub type ReadDirectory(t) =
  fn(String) -> K(t, Result(List(String), String))

pub type ReadFile(t) =
  fn(String) -> K(t, Result(BitArray, String))

/// can fail if no port available.
pub type Serve(t) =
  fn(Option(Int), fn(Request(BitArray)) -> Response(BitArray)) ->
    K(t, Result(Int, String))

pub type Sign(t, key) =
  fn(SignAlgorithm, key, BitArray) -> K(t, Result(BitArray, String))

/// Does not fail if configured
pub type StrongRandom(t) =
  fn(Int) -> K(t, BitArray)

pub type UnixNow(t) =
  fn() -> K(t, Int)

/// Can fail on permissions
pub type WriteFile(t) =
  fn(String) -> K(t, Result(Nil, String))

/// Uri might not exist or resolve
pub type Visit(t) =
  fn(Uri) -> K(t, Result(Nil, String))

/// Uri might not exist or resolve
pub type Zip(t) =
  fn(List(#(String, BitArray))) -> K(t, Result(BitArray, String))
