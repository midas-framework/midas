import process/process
import process/process.{From, Pid, BarePid, ExitReason, Normal, Infinity, Milliseconds}
import process/supervisor/set_supervisor
import midas_utils
import gleam/expect

// start child fn takes test pid
fn start_child() {
  process.spawn_link(
    fn(receive) {
      let Ok(_) = receive(Infinity)
      Normal
    },
  )
}

pub fn listing_children_test() {
  let supervisor = set_supervisor.spawn_link(start_child)
  expect.equal(Ok([]), set_supervisor.which_children(supervisor))
  let Ok(c1) = set_supervisor.start_child(supervisor)
  expect.equal(Ok([c1]), set_supervisor.which_children(supervisor))
  let Ok(c2) = set_supervisor.start_child(supervisor)
  expect.equal(Ok([c1, c2]), set_supervisor.which_children(supervisor))
  process.send(c1, Nil)

  // TODO remove
  process.do_sleep(100)
  expect.equal(Ok([c2]), set_supervisor.which_children(supervisor))
}
