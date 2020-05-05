// Path is required in erlang map, for consistency an empty path could be considered Error(Nil), but this might not feel consistent with query/fragments
import gleam/result.{Option}

pub type Uri {
  Uri(
    scheme: Option(String),
    userinfo: Option(String),
    host: Option(String),
    port: Option(Int),
    path: String,
    query: Option(String),
    fragment: Option(String),
  )
}

pub external fn parse(String) -> Result(Uri, Nil) =
  "uri_native" "parse"

pub external fn parse_query(
  String,
) -> Result(List(tuple(String, String)), Nil) =
  "uri_native" "parse_query"

pub external fn query_to_string(List(tuple(String, String))) -> String =
  "uri_native" "query_to_string"

pub external fn to_string(Uri) -> String =
  "uri_native" "to_string"
