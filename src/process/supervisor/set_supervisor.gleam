// Other Possible name: Fleet Clone, Copy, Replica, Homogenious
import gleam/io
import gleam/list
import gleam/option.{Some}
import process/process
import process/process.{From, Pid, BarePid, ExitReason, Infinity, Milliseconds, TrapExit}

pub type Messages(m, c) {
  StartChild(From(Pid(m)), c)
  WhichChildren(From(List(Pid(m))))
  Exit(BarePid, ExitReason)
}

fn pop(haystack, predicate, done) {
  case haystack {
    [] -> Error(Nil)
    [x, ..rest] -> case predicate(x) {
      True -> Ok(tuple(x, list.append(list.reverse(done), rest)))
      False -> pop(rest, predicate, [x, ..done])
    }
  }
}

fn loop(receive, start_child, children) {
  try message = receive(Infinity)
  case message {
    // Could instead pass in a function that returns the right pid?
    StartChild(from, config) -> {
      let child = start_child(config)
      let children = [child, ..children]
      process.reply(from, child)
      loop(receive, start_child, children)
    }
    WhichChildren(from) -> {
      process.reply(from, list.reverse(children))
      loop(receive, start_child, children)
    }
    Exit(down_pid, _reason) -> {
      let predicate = fn(pid) { process.bare(pid) == down_pid }
      // If unknown pid assume parent, and crash.
      case pop(children, predicate, []) {
        Ok(tuple(_found, children)) -> loop(receive, start_child, children)
      }
    }
  }
}

// Error(Nil) -> process.kill(process.unsafe_self())
// Currently only supports temporary children
// Take RestartStrategy as an argument when starting and compare reason to it.
fn init(receive, start_child) {
  process.process_flag(TrapExit(True))
  loop(receive, start_child, [])
}

pub fn spawn_link(start_child: fn(c) -> Pid(a)) -> Pid(Messages(a, c)) {
  process.spawn_link(init(_, start_child))
}

pub fn start_child(supervisor: Pid(Messages(m, c)), config: c) {
  process.call(supervisor, StartChild(_, config), Milliseconds(5000))
}

pub fn which_children(supervisor) {
  process.call(supervisor, WhichChildren(_), Milliseconds(5000))
}
