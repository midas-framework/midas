import gleam/option.{Some}
import process/process
import process/process.{From, Pid, BarePid, ExitReason, Infinity, Milliseconds}
import process/supervisor/set_supervisor
import midas_utils
import gleam/should

// start child fn takes test pid
fn start_child() {
  process.spawn_link(
    fn(receive) {
      let Some(_) = receive(Infinity)
      Nil
    },
  )
}

pub fn listing_children_test() {
  let supervisor = set_supervisor.spawn_link(start_child)
  should.equal(Ok([]), set_supervisor.which_children(supervisor))
  let Ok(c1) = set_supervisor.start_child(supervisor)
  should.equal(Ok([c1]), set_supervisor.which_children(supervisor))
  let Ok(c2) = set_supervisor.start_child(supervisor)
  should.equal(Ok([c1, c2]), set_supervisor.which_children(supervisor))
  process.send(c1, Nil)

  // TODO remove
  process.sleep(100)
  should.equal(Ok([c2]), set_supervisor.which_children(supervisor))
}
