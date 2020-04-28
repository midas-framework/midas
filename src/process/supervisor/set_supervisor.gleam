// Other Possible name: Fleet Clone, Copy, Replica, Homogenious
import gleam/list
import process/process
import process/process.{From, Pid, BarePid, ExitReason, Normal, Infinity, Milliseconds, TrapExit}
import midas_utils

pub type Messages(m) {
  StartChild(From(Pid(m)))
  WhichChildren(From(List(Pid(m))))
  EXIT(BarePid, ExitReason)
}

fn pop(haystack, predicate, done) {
  case haystack {
    [] -> tuple(Error(Nil), list.reverse(done))
    [x, ..rest] -> case predicate(x) {
      True -> tuple(Ok(x), list.append(list.reverse(done), rest))
      False -> pop(rest, predicate, [x, ..done])
    }
  }
}

fn loop(receive, start_child, children) {
  case receive(Infinity) {
    Ok(StartChild(from)) -> {
      let child = start_child()
      let children = [child, ..children]
      process.reply(from, child)
      loop(receive, start_child, children)
    }
    Ok(WhichChildren(from)) -> {
      process.reply(from, list.reverse(children))
      loop(receive, start_child, children)
    }
    Ok(EXIT(down_pid, _)) -> {
      let predicate = fn(pid) { process.bare(pid) == down_pid }
      let tuple(_found, children) = pop(children, predicate, [])
      loop(receive, start_child, children)
    }
  }
}

fn init(receive, start_child) {
  process.process_flag(TrapExit(True))
  loop(receive, start_child, [])
}

pub fn spawn_link(start_child: fn() -> Pid(a)) -> Pid(Messages(a)) {
  process.spawn_link(init(_, start_child))
}

pub fn start_child(supervisor: Pid(Messages(m))) {
  process.call(supervisor, StartChild(_), Milliseconds(5000))
}

pub fn which_children(supervisor) {
  process.call(supervisor, WhichChildren(_), Milliseconds(5000))
}
