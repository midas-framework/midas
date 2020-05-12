//// Working with typed processes in Gleam.
////
//// This module allows you to:
//// - spawn processes
//// - send and receive messages
//// - call processes, send a message and expect a reply.
//// - monitor processes
//// - link to processes
//// - supervise processes with the `set_supervisor` and `rest_for_one`
////
//// *This module is being developed concurrently with the Midas web framework.
//// It will be extracted before Midas reaches 1.0.
//// Help with this module is greatly appreciated.*
////
//// You will need these imports for the examples to work
////
////    import process/process
////    import process/process.{Normal, Infinity, From}
////
//// ## Starting and stopping processes
////
//// A process is started using `process.spawn_link/1`.
//// The single argument is a function that accepts a receive call and returns an `ExitReason`.
////
//// This is an example of the simplest process that immediately exits with reason `Normal`
////
////    let pid = process.spawn_link(fn(_receive) {
////        Normal
////    })
////
//// ## Sending and receiving messages
////
//// Here is a simple process that sums all the numbers sent to it.
////
////    fn loop(receive, state) {
////      let value = receive(Infinity)
////      loop(receive, state + value)
////    }
////
////    let pid = process.spawn_link(loop(_, 0))
////
////    process.send(pid, 1)
////    process.send(pid, 2)
////
//// By recursively calling loop this function runs forever.
//// The type of the receive function is parameters by the type of messages it accepts.
////
//// Because gleam infers the type of value to be an Int it is known that only integers can be sent to this pid.
//// The following call will not be allowed by the type checker.
////
////    process.send(pid, "0")
////
//// Normally a process will accept more message types than an Int,
//// This is handled by defining a single type with a constructor for each expected message type.
////
//// ## Calling a process
////
//// Send a message to a process an await a reply.
////
////    type Messages {
////      Reverse(From(String), String)
////    }
////
////    fn loop(receive) {
////      let Reverse(from, str) = receive(Infinity)
////      process.reply(from, string.reverse(str))
////      loop(receive)
////    }
////
////    let pid = process.spawn_link(loop(_))
////
////    let Ok(reversed) = process.call(pid, Reverse(_, "hello"))
////
//// The `Reverse` message type includes a from reference that says callers accept a `String` type as a response.
//// Receiving this message works the same way as before, but now we have a from value that we can send a reply to.
//// Sending a call message uses `process.call/2`.
//// The first argument is a Pid.
//// The second argument is a function that takes a from reference and creates a message.
////
//// Having a constructor for the sent message allows the process library to create a from value.
//// Doing this allows the calling process to receive replies without having to specify them as part of their message contract.
//// Again Gleam ensures we can't send a message that isn't understood and we can't reply with a type that isn't expected.


import gleam/result.{Option}

/// A value in milliseconds or infinity.
pub type Wait {
  Infinity
  Milliseconds(Int)
}

// WORKING WITH PROCESSES

/// Identifier for a process
/// This type is parameterised by the message type acceptable to that process.
pub external type Pid(m)

/// Reason for a process to terminate.
///
/// In most cases `Normal` is the correct value to return from a process's run function.
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

// Start a run function with a reference to the type of error messages that the parent process can accept.
// fn (run: fn() -> a, wrap_exit: fn(a) -> Message(parent)) Needs to be in process library that the specific message mapped to will be exit.

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

/// A Pid that is not parameterised by the messages it can receive.
/// Bare Pids are useful when you receive a Pid from a DOWN or EXIT message.
/// They can also be used to check equality between two pids.
pub external type BarePid

/// Create a `BarePid` from a `Pid(m)`
pub external fn bare(Pid(a)) -> BarePid =
  "process_native" "identity"

// WORKING WITH MONITORS AND REFERENCES
pub external type Ref

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
  Timeout
  // Slow
  Gone
}

// Gone(ExitReason) To not conflict with Down types
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
