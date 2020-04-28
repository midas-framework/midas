import gleam/int
import gleam/map.{Map}
import gleam/result

type ENV =
  Map(String, String)

type Parser(a, b) =
  fn(String) -> Result(a, b)

external fn os_get_env() -> List(String) =
  "os" "getenv"

fn get_env() -> Map(String, String) {
  os_get_env()
  |> list.fold(
    [],
    fn(env_var_name_value, done) {
      let tuple(key, maybe_value) = midas_utils.split_on(env_var_name_value, "")
      case maybe_value {
        Ok(value) -> [tuple(key, value)]
        Error(Nil) -> []
      }
    },
  )
  |> map.from_list()
}

fn try_parse(raw, parser) {
  case parser(raw) {
    Ok(value) -> Ok(value)
    Error(_) -> Error(Nil)
  }
}

pub fn required(env: ENV, key: String, parser: Parser(a, b)) -> Result(a, Nil) {
  map.get(env, key)
  |> result.then(try_parse(_, parser))
  // Unwrap with log warning
}

pub fn optional(env: ENV, key: String, parser: Parser(a, b), fallback: a) {
  map.get(env, key)
  |> result.then(try_parse(_, parser))
  |> result.unwrap(fallback)
}
