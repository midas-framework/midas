// Other Possible name: Fleet Clone, Copy, Replica, Homogenious
import gleam/io
import gleam/list
import gleam/option.{Some}
import process/process
import process/process.{From, Pid, BarePid, ExitReason, Infinity, Milliseconds, TrapExit}

pub type Restart {
  Permanent
  Temporary
}

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

fn loop(receive, start_child, restart, children) {
  try message = receive(Infinity)
  case message {
    // Could instead pass in a function that returns the right pid?
    StartChild(from, config) -> {
      let child = start_child(config)
      let children = [tuple(child, config), ..children]
      process.reply(from, child)
      loop(receive, start_child, restart, children)
    }
    WhichChildren(from) -> {
      process.reply(
        from,
        list.reverse(children)
        |> list.map(
          fn(r) {
            let tuple(pid, _config) = r
            pid
          },
        ),
      )
      loop(receive, start_child, restart, children)
    }
    Exit(down_pid, _reason) -> {
      let predicate = fn(child) {
        let tuple(pid, _config) = child
        process.bare(pid) == down_pid
      }
      // If unknown pid assume parent, and crash.
      case pop(children, predicate, []) {
        Ok(tuple(child, children)) -> case restart {
          Temporary -> loop(receive, start_child, restart, children)
          Permanent -> {
            let tuple(_pid, config) = child
            let pid = start_child(config)
            let children = [tuple(pid, config), ..children]
            loop(receive, start_child, restart, children)
          }
        }
      }
    }
  }
}

// Error(Nil) -> process.kill(process.unsafe_self())
// Currently only supports temporary children
// Take RestartStrategy as an argument when starting and compare reason to it.
fn init(receive, start_child, restart) {
  process.process_flag(TrapExit(True))
  loop(receive, start_child, restart, [])
}

pub fn spawn_link(
  start_child: fn(c) -> Pid(a),
  restart: Restart,
) -> Pid(Messages(a, c)) {
  process.spawn_link(init(_, start_child, restart))
}

pub fn start_child(supervisor: Pid(Messages(m, c)), config: c) {
  process.call(supervisor, StartChild(_, config), Milliseconds(5000))
}

pub fn which_children(supervisor) {
  process.call(supervisor, WhichChildren(_), Milliseconds(5000))
}
