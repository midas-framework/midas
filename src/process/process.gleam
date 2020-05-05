import gleam/result.{Option}

pub type Wait {
  Infinity
  Milliseconds(Int)
}

// Need a type of Timeout that is equivalent to Option
// Can't be defined in 0.7 TODO move to master
// pub type Timeout {
//   Timeout
// }

// WORKING WITH PROCESSES
pub type Pid(m) {
  Pid(Pid(m))
}

pub type ExitReason {
  Normal
  Kill
  Killed
}

// https://erlang.org/doc/reference_manual/errors.html#exit_reasons
// Other types e.g. BadArith(Stack) can all be enumerated here.

// receive can only fail due to timeout so option is an acceptable response type
type Receive(m) =
  fn(Wait) -> Option(m)

type Run(m) =
  fn(Receive(m)) -> ExitReason

pub external fn spawn_link(Run(m)) -> Pid(m) =
  "process_native" "spawn_link"

pub external fn send(Pid(m), m) -> m =
  "erlang" "send"

pub external fn unsafe_self() -> Pid(m) =
  "erlang" "self"

pub fn self(_: Receive(m)) -> Pid(m) {
  unsafe_self()
}

// This can be typed because an untyped pid is only the result of a DOWN or EXIT Message
// TODO make exit reason Killed and have a special fn for kill
pub external fn exit(Pid(m), ExitReason) -> Bool =
  "erlang" "exit"

// Add a clause that turned Killed into Kill
// Or just StopReason
// type Kill {
//     Kill
// }
//
// external fn kill(Pid(m), Kill) -> Bool
// = "erlang" "exit"
//
// pub fn exit(pid, reason) {
//     case reason {
//         Killed -> kill(pid, Kill)
//         _ -> do_exit(pid, reason)
//     }
// }
// The return value of a run function should just be the exit reason of a process.
// Then a Down message can just be treated as a promise for the result of the computation
// erlang:is_process_alive
// WORKING WITH UNTYPED PROCESSES
pub type BarePid {
  BarePid(BarePid)
}

pub external fn bare(Pid(a)) -> BarePid =
  "process_native" "identity"

// WORKING WITH MONITORS AND REFERENCES
pub type Ref {
  Ref(Ref)
}

pub type MonitorType {
  Process
}

pub external fn monitor(MonitorType, Pid(m)) -> Ref =
  "erlang" "monitor"

pub fn monitor_process(pid) {
  monitor(Process, pid)
}

pub type DemonitorOptions {
  Flush
}

pub external fn demonitor(Ref, List(DemonitorOptions)) -> Bool =
  "erlang" "demonitor"

// PROCESS FLAGS, WARNING:
// Can change the messages expected in a receive function
pub type ProcessFlag {
  TrapExit(Bool)
}

pub external fn process_flag(ProcessFlag) -> ProcessFlag =
  "process_native" "process_flag"

// CALL PROCESS
pub type From(r) {
  From(Ref, Pid(tuple(Ref, r)))
}

// Can check pid is self
// Need error because process could have terminated
// needs to be separate receive fn because we want to ignore exits and motiors from other pids
// Might be more expressive to "Gone/Slow", I can't have a separate Type that includes a Timout Branch
pub type CallError {
        Timeout // Slow
        Gone    // Gone(ExitReason) To not conflict with Down types
    }

pub external fn receive_reply(Ref, Wait) -> Result(r, CallError) =
  "process_native" "receive_reply"

pub fn call(
  pid: Pid(m),
  constructor: fn(From(r)) -> m,
  wait: Wait,
) -> Result(r, CallError) {
  let reference = monitor_process(pid)
  let from = From(reference, unsafe_self())
  let _message = send(pid, constructor(from))
  receive_reply(reference, wait)
}

pub fn reply(from: From(r), message: r) {
  let From(reference, pid) = from
  send(pid, tuple(reference, message))
}

// Proc lib needs a start and init_ack function
// Will need an external receive ack
pub type OK {
  OK(OK)
}

pub external fn do_sleep(Int) -> OK =
  "timer" "sleep"
