import gleam/int
import gleam/io
import midas/node
import midas/sdk/github

pub fn main() {
  let task = {
    authenticate(True)
  }
  node.run(task, "")
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
