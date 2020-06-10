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
