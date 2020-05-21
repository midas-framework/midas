# Gleam processes

- For this version if a process might accept DOWN or EXIT messages the user needs to remember to add them to the protocol.
  This is to give maximum control to implementers of supervisors etc.
  Also this level of abstraction is infrequently changed so well tested libraries can be used.

### Notes from this article

https://arxiv.org/pdf/1611.06276.pdf

- Great clarification of terms. "Nonetheless, we will stick with the popular names, even if it is as inapposite ascomparing TV channels with TV actors"
- http://hopac.github.io/Hopac/Hopac.html start and start_ignore. Very interesting view of things that appears to be task first.
  - there is probably a way to use monitors to pass tasks between processes. i.e. process a can get the value and send it to process B, if value not computed then B can monitor and wait for it. termination reason Concluded(value)
- "type pollution problem" -> This makes sense and is circumvented if not solved by building in the concept of a call + reply. Essentially selective receive impersonates having a channel. Having the sender understand the receivers type can be handled by sending a mapping function in the call message,
- Stack as the implementation. parameterised in list

## Comments

1. Document HTTP and update README
2. Switch to Run only child specs
3. no name is just happening, doc this + spawn without error
4. Build calls on channels, need this assumption because of late call replies after timeout and returning to normal receive.
5. return more than pid from supervisor, might want reference to ets table or some such. supervisors can make a single call to look up ets table and pass reference to all childrean.
  this can also be done in run function.

  ```rust
  pub fn spec(){
    Permanent(run, [TrapExit(Exit)])
  }
  ```
Defining child spec on the child module doesn't work if parameterised at the supervisor level,
unless PermanentSupervisorChild(Permanent(run, blah))

#### supervisors notes

process always has to handle case it might be too slow and be killed, or node dies. let's go strait to kill!

fn do_loop(receive, loop, state) {
  do_loop(receive, loop, loop(receive(Infinity, state)))
}

fn gen_server(init, loop) {
  fn (receive) {
    let state = init()
    do_loop(receive, loop, state)
  }
}
// Return state loop


fn loop(message) {

  tuple(loop_two(\_, "Foo"), [(Address, message), (from, reply)])
}

Don't think the list of messages is doable, could be a send function, and there might be ways to intercept, but we don't have single state value to look at
Opaque
tuple(BarePid, Dynamic)

```rust
type Envelope {
  Envelope(Pid(Dynamic), Dynamic)
}

process.envelope(pid: Pid(m), message: m) -> Envelope

process.reply_envelope(from: From(m), message: m) -> Envelope {
  let From(ref, pid) = from
  let message = tuple(ref, message)
  envelope(pid, message)
}
```

```rust
handle(m, s) -> tuple(s, List(Envelope))
```

```rust
type Message(a) {
    Push(a)
    Pop(From(a))
}

pub fn handle(message, state) {
    case message {
        Push(new) -> {
          tuple([new, ..state], [])
        }
        Pop(from) -> {
            let tuple(top, state) = list.pop(state)
            tuple(state, [process.reply_envelope(from, top)])
        }
    }
}

pub fn init() {
    []
}

gen_server.spawn_link(init, handle)
```

If ets is started reference can be returned by a call to the pid in an init step
Should write up why No names

Call should be build on channels.

start_link functions must start the link, supervisors do not handle this
https://erlang.org/doc/man/supervisor.html

When terminating children supervisor unlinks and monitors, to make sure a child that has unlinked cannot cause the supervisor to hang
https://github.com/erlang/otp/blob/master/lib/stdlib/src/supervisor.erl#L896

The above leads to a race condition when killing supervisors, they must be given infinity time to kill their own children
After a timeout will always kill,
THis supervisor works on assumption that kill is ok

Using a receive it is possible to not wrap all custom messages in a Message block,
Doing so does allow one to forget to implement Down and Exit messages
It also allows owners of the address to send in the Down and exit messages

Proc lib defines init_ack, and funtions to wait for that, init_ack returns the pid in most cases
spawn_link does nothing but get parents
init_p simply sets parents in new process
start_link uses spawn link, awaits a reply from init_ack and then demonitors if appropriate

Gen module defines call, but also naming that I don't yet want to handle

To keep a protocol private you will need send and receive external functions defined in module.

#### Naming of Pid

I would like to rename the type `Pid(m)` to `Address(m)` and `BarePid` to Pid.
However I think from an understanding point of view for new people explaining that there are now typed pids is easier than explaining what an address is.

#### Handling DOWN & EXIT messages, in worker processes

Enforcing every worker process to handle DOWN messages, that might never arrive is verbose.
Also before completeness checking its just as easy to forget a case clause as it is to forget to add a down/exit message to a processes protocol.
Because this is also low level it is up to the implementer to handle down exit where necessary, all other types in a the protocol are checked.

### Derive self safely from receive

A receive function that took a pid (which should only be self) was too easy to pass other pids too

### Have a Never return type

This unrepresentable Exit reason could ensure that a process never ended.

### Have a ChildPid type

A Pid could be wrapped in a child pid type.
If this was the return of a start_link call then a supervisor could use this information to decide the restart strategy.
This would colocate the function and restart logic, as encouraged by child specs.

```rust
type ChildPid(a) {
    Temporary(Pid(a)) // Recoverable
    Transient(Pid(a)) // Unrecoverable
    Permanent(Pid(a))
}
```

Permanent pids could make use of the Never return type

### Separate Worker and Supervisor processes.

See commit a0fbc35d16b12c9289f9321b9ef5b789d2bd7eea

- Both have the same Pid Type and that pid can only get sent messages, not exit and down.
- Slightly safer, but only assuming exhaustive checking.
- Downside was more verbose, There was also the question of having a hardcoded timeout value or call value otherwise process specific message type had to be nested in the appropriate varient of the general process type.

##### Further Comments

Could Pid be parameterised supervisor or worker then receive could take self as an argument.
That would allow receive to check the pid wasn't send
Is there a performace improvement for not having an anonymous function around receive

#### Heterogenious extensible supervisor

```rust
Pair(
    Spec(start: server.start_link)
    Pair(
        Spec()
        Spec
    )
)

Spec Top level == 1

let (Ok(p), (Ok(p), (Ok(p) , Error(Nil))))


type MySupervisor = Spec(Int, Spec(Float, Spec(String, End)))

let Child(p1, Child(p2, Child(p3, E))) = which_children(supervisor)

Spec(Int, Spec(Float, Spec(Sting, Nil)))

type Spec(a, b) {
    Child(a, Spec(b))
    // This could specify restart mechanism
    Last
}
```

### Thoughts on processes

**These notes are not relevant to understanding Midas as a server or framework. The process code should eventually be extracted from this repo.**

Supervisors should not accept start functions that fail.

One thing that can cause failures is naming processes.
This is a big global state problem and probably could always happen,
but at the registered process level a naming failure should be a hard crash.

Potential an Ignore response is ok, for example not starting a server if the PORT is not set.
There is no reason the supervisor can't handle this but it might be a nicer grouping of logic to put this in the particular process.
This is similar to the logic that lead to child_specs

Having the init and init_ack functionality causes alot of delay and the supervisor has to watch for more race conditions.

config typed properly

```rust
let Ok(pid) = postgresql.start_client(database_url: String)

// Replace that unsafe start API with two step process
// 1. outside supervision tree
let Ok(db_config) = postgresql.parse_url(database_url: String)

// 2.
let pid = postgresql.start_client(db_config)
```

There are some good discussion on this around,
need to look for more on less things being required at startup.
users of the pg_client are better of reporting the errors.

##### Request handlers

These are tasks not "actors", they shouldn't have an open inbox but deal in gen:replies promises.

Super general inbox setup

```rust
let tuple(receive, resolve) = promise.new()

process.spawn(fn(){
  resolve(5)
})

let Ok(value) = receive(Infinity)
```

A sendable trait could be really useful here.

### Resources http://joeduffyblog.com/2015/11/19/asynchronous-everything/
