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

import gleam/int
import gleam/list
import gleam/option.{Some, None}
import gleam/result
import gleam/string

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

/// Validation rules that may be applied to strings.
///
/// Rules are applied in order, Trim should normally be first in the list.
pub type StringValidations {
  Trim
  Allow(List(String))
  Disallow(List(String))
  MinLength(Int)
  MaxLength(Int)
}

// TODO pattern or Ascii, Alphanumeric
fn run_string_validation(validation, raw) {
  case validation {
    Trim -> Ok(string.trim(raw))
    Allow(allowed) -> case list.contains(allowed, raw) {
      True -> Ok(raw)
      False -> Error("not an allowed value")
    }
    MinLength(min) -> case string.length(raw) >= min {
      True -> Ok(raw)
      False -> Error(
        string.append("less than minimum length of ", int.to_string(min)),
      )
    }
    MaxLength(max) -> case string.length(raw) <= max {
      True -> Ok(raw)
      False -> Error(
        string.append("greater than maximum length of ", int.to_string(max)),
      )
    }
    Disallow(disallowed) -> case list.contains(disallowed, raw) {
      True -> Error("is a disallowed value")
      False -> Ok(raw)
    }
  }
}

/// Cast an input value as a string that obeys the given validation rules
pub fn as_string(raw, validations) {
  case validations {
    [] -> Ok(raw)
    [validation, ..rest] -> {
      try raw = run_string_validation(validation, raw)
      as_string(raw, rest)
    }
  }
}

// should be possible to reuse for floats
pub type NumberValidations {
  Min(Int)
  Max(Int)
}

fn run_number_validation(validation, number) {
  case validation {
    Min(min) -> case number >= min {
      True -> Ok(number)
      False -> Error(string.append("less than minimum of ", int.to_string(min)))
    }
    Max(max) -> case number <= max {
      True -> Ok(number)
      False -> Error(
        string.append("greater than maximum of ", int.to_string(max)),
      )
    }
  }
}

fn run_number_validations(number, validations) {
  case validations {
    [] -> Ok(number)
    [validation, ..rest] -> {
      try number = run_number_validation(validation, number)
      run_number_validations(number, rest)
    }
  }
}

pub fn as_integer(raw, validations) {
  case int.parse(string.trim(raw)) {
    Ok(number) -> run_number_validations(number, validations)
    Error(Nil) -> Error("not an integer value")
  }
}
