import gleam/list


pub type Header =
  tuple(String, String)

pub type Headers =
  List(Header)

pub fn append(headers: Headers, key: String, value: String) {
    [tuple(key, value) | headers]
}

fn match_key(pair, search) {
    let tuple(key, _value) = pair
    key == search
}

pub fn find(headers, key) {
    case list.filter(headers, match_key(_, key)) {
        [] -> Error(Nil)
        [tuple(_key, value)] -> Ok(value)
    }
}
