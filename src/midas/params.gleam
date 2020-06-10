//// Handle untrusted input at your system boundaries.
////
//// This module works on lists of string pairs,
//// as this data structure can represent data where the order is important or not.
////
//// ## Examples
////
////    let raw = map.to_list(os.get_env())
////    let Ok(raw) = http.get_query(request)
////    let Ok(raw) = http.get_form(request)
////
//// More complex data structures, such as JSON, cannot use these functions, yet.
////
//// ## Questions
////
//// - Is there a better name, e.g. spec
////   `spec.required(params, "key", as_blah)`
//// - Should the raw data structure be binaries not strings,
////   Several functions can be parameterised but the as functions probably can't
//// - Should we offer a pop version so that you can test for unused keys.

import gleam/list
import gleam/option.{Some, None}
import gleam/result

/// Possible reasons for a field to be invalid.
pub type Invalid(a) {
  Missing(key: String)
  CastFailure(key: String, help: a)
}

fn find(form, key) {
  case list.key_find(form, key) {
    // This one reference to an empty string makes it much less versatile, but in reality most inputs have string keys.
    Ok("") -> Error(Missing(key))
    Error(Nil) -> Error(Missing(key))
    Ok(value) -> Ok(value)
  }
}

/// Require a value for the associated key in the input data.
///
/// A value of an empty string `""` is also considered a missing value.
/// Check `boolean` for testing just the presence of a field.
pub fn required(from form, get key, cast) {
  try raw = find(form, key)
  cast(raw)
  |> result.map_error(CastFailure(key, _))
}

/// Fetch an optional value from the associated key.
///
/// This can return a `CastFailure` error, when a value is present but the cast function errors.
/// To ignore cast errors unwrap the result, e.g.
///
///    let foo = params.optional(form, "foo", Ok) |> result.unwrap(None)
pub fn optional(from form, get key, cast) {
  case find(form, key) {
    Ok(raw) -> case cast(raw) {
      Ok(value) -> Ok(Some(value))
      Error(reason) -> Error(CastFailure(key, reason))
    }
    Error(Missing(_key)) -> Ok(None)
  }
}

/// Fetch an overridable value from the associated key, with a fallback when key not present in data.
///
/// This can return a `CastFailure` error, when a value is present but the cast function errors.
pub fn overridable(from form, get key, cast, or fallback) {
  case find(form, key) {
    Ok(raw) -> case cast(raw) {
      Ok(value) -> Ok(value)
      Error(reason) -> Error(CastFailure(key, reason))
    }
    Error(Missing(_key)) -> Ok(fallback)
  }
}

/// Returns true if the given key is present.
///
/// NOTE: This function is different to others in the module,
/// an empty string associated with a given key will still return `True`.
pub fn boolean(form, key) {
  case list.key_find(form, key) {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// Check that a value is not present in the data.
///
/// Useful for indicating fields that might be provided but are unsupported.
/// If there is a value for the key a `CastFailure` will be returned as the reason for the error.
pub fn disallowed(from form, get key, reason) {
  case find(form, key) {
    Ok(raw) -> Error(CastFailure(key, reason))
    Error(Missing(_key)) -> Ok(Nil)
  }
}
