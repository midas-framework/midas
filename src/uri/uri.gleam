import gleam/result.{Option}

pub type Uri {
  Uri(
    scheme: Option(String),
    userinfo: Option(String),
    host: Option(String),
    port: Option(Int),
    path: Option(String),
    query: Option(String),
    fragment: Option(String),
  )
}

pub external fn parse(String) -> Result(Uri, Nil) =
  "uri_native" "parse"


// to_string
// compose/dissect
