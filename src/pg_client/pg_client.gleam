import gleam/dynamic.{Dynamic}
import gleam/option.{Some, None}
import gleam/string
import gleam/uri
import process/process.{Pid}
import utils/charlist.{Charlist}

type PgoOptions {
  Host(Charlist)
  Port(Int)
  User(Charlist)
  Password(Charlist)
  Database(Charlist)
}

// A Map(list_key any) in gleam that reduces writing erlang code could be interestig
external fn do_start_link(List(PgoOptions)) -> Result(Pid(a), Nil) =
  "pg_client_native" "start_link"

// Could be spawn link, would accept a databaseurl type,
pub fn start_link(database_url) {
  // case uri.parse(database_url) {
  //   Ok(
  //     uri.Uri(
  //       Some("postgres"),
  //       Some(userinfo),
  //       Some(host),
  //       Some(port),
  //       path,
  //       None,
  //       None,
  //     ),
  // ) -> case string.split_once(userinfo, ":") {
  //     tuple(user, Some(password)) -> case string.split_once(path, "/") {
  //       tuple("", Some(database)) -> {
  //         let options = [
  //             Host(charlist.binary_to_list(host)),
  //             Port(port),
  //             User(charlist.binary_to_list(user)),
  //             Password(charlist.binary_to_list(password)),
  //             Database(charlist.binary_to_list(database)),
  //           ]
  //         do_start_link(options)
  //       }
  //       _ -> Error(Nil)
  //     }
  //     _ -> Error(Nil)
  //   }
  //   _ -> Error(Nil)
  // }
  todo
}

pub type PgType {
  PgString(String)
  PgInt(Int)
  PgBool(Bool)
  PgDateTime(Dynamic)
}

pub type SqlReturn {
  Select(Int, List(List(PgType)))
  Insert(Int, List(List(PgType)))
}

pub external fn query(String, List(PgType)) -> Result(SqlReturn, Nil) =
  "pg_client_native" "query"
