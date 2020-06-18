import gleam/option.{Some}
import process/process
import process/process.{From, Pid, BarePid, ExitReason, Infinity, Milliseconds}
import process/supervisor/set_supervisor
import gleam/should

// start child fn takes test pid
fn start_child(_: Nil) {
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
  let Ok(c1) = set_supervisor.start_child(supervisor, Nil)
  should.equal(Ok([c1]), set_supervisor.which_children(supervisor))
  let Ok(c2) = set_supervisor.start_child(supervisor, Nil)
  should.equal(Ok([c1, c2]), set_supervisor.which_children(supervisor))
  process.send(c1, Nil)

  // Sleep needed for message to arrive, do with a monitor instead
  process.sleep(100)
  should.equal(Ok([c2]), set_supervisor.which_children(supervisor))
}
