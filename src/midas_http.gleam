import midas/request.{Request}

external fn do_parse(String) -> Result(tuple(String, String, List(tuple(String, String))), Nil)
    = "midas_http_native" "parse"


pub fn parse(buffer: String) -> Result(Request, Nil) {
    let Ok(tuple(method, full_path, [tuple(_, value) | _])) = do_parse(buffer)
    // TODO pop headers
    let request = Request(authority: value, path: full_path, headers: [])
    Ok(request)
}
