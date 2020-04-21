import midas
import gleam/expect

pub fn hello_world_test() {
  midas.hello_world()
  |> expect.equal(_, "Hello, from midas!")
}
