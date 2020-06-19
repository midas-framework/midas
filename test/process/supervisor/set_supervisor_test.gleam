import gleam/option.{Some}
import process/process
import process/process.{From, Pid, BarePid, ExitReason, Infinity, Milliseconds}
import process/supervisor/set_supervisor
import gleam/should

// start child fn takes test pid
fn start_child(_: Nil) {
  process.spawn_link(
    fn(receive) {
      try Nil = receive(Infinity)
      Ok(Nil)
    },
  )
}

pub fn listing_children_test() {
  let supervisor = set_supervisor.spawn_link(start_child)
  should.equal(Ok([]), set_supervisor.which_children(supervisor))
  let Ok(c1) = set_supervisor.start_child(supervisor, Nil)
  let Ok(c2) = set_supervisor.start_child(supervisor, Nil)

  set_supervisor.which_children(supervisor)
  |> should.equal(Ok([c1, c2]))

  process.send(c1, Nil)

  // Sleep needed for message to arrive, do with a monitor instead
  process.sleep(100)
  should.equal(Ok([c2]), set_supervisor.which_children(supervisor))
}

pub fn will_stop_children_for_unknown_exit() {
  // Assumes exit to be parent or other link
  // If parent then exit on normal everything else dont?
  let supervisor = set_supervisor.spawn_link(start_child)
  should.equal(Ok([]), set_supervisor.which_children(supervisor))
  let Ok(c1) = set_supervisor.start_child(supervisor, Nil)
  // monitor child
  todo
}
