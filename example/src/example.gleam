import gleam/http
import gleam/int
import gleam/io
import gleam/javascript/promise
import gleam/option.{None}
import midas/node
import midas/sdk/github
import midas/sdk/github/schema
import midas/task as t

pub fn main() {
  let task = {
    use token <- t.do(authenticate(True))

    use repos <- t.do(github.activity_list_repos_starred_by_authenticated_user(
      token,
      None,
      None,
      None,
      None,
    ))
    let assert [repo, ..] = repos
    io.debug(repo)
    t.done(Nil)
  }
  let task = t.proxy(task, http.Https, "eyg-backend.fly.dev", None, "/github")
  promise.map(node.run(task, ""), io.debug)
}

pub fn authenticate(local) {
  let state = int.to_string(int.random(1_000_000_000))
  let state = case local {
    True -> "LOCAL" <> state
    False -> state
  }
  github.do_authenticate(eyg_website, state)
}

pub const eyg_website = github.App(
  "9a97af38f5da0d31aa54",
  "hidden",
  "https://eyg.run/auth/github",
)
