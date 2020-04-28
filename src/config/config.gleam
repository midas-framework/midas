import gleam/int
import gleam/list
import gleam/map.{Map}
import gleam/result
import midas_utils

type ENV =
  Map(String, String)

type Parser(a, b) =
  fn(String) -> Result(a, b)

// https://erlang.org/doc/man/os.html#getenv-0
// https://github.com/elixir-lang/elixir/blob/v1.10.3/lib/elixir/lib/system.ex#L438-L444
external fn os_get_env() -> List(String) =
  "config_native" "getenv"

pub fn get_env() -> Map(String, String) {
  os_get_env()
  |> list.fold(
    [],
    fn(env_var_name_value, done) {
      let tuple(
        key,
        maybe_value,
      ) = midas_utils.split_on(env_var_name_value, "=")
      case maybe_value {
        Ok(value) -> [tuple(key, value), ..done]
        Error(Nil) -> done
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
}

// Unwrap with log warning
pub fn optional(env: ENV, key: String, parser: Parser(a, b), fallback: a) {
  map.get(env, key)
  |> result.then(try_parse(_, parser))
  |> result.unwrap(fallback)
}
