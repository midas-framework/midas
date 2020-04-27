# Gleam processes

- For this version if a process might accept DOWN or EXIT messages the user needs to remember to add them to the protocol.
  This is to give maximum control to implementers of supervisors etc.
  Also this level of abstraction is infrequently changed so well tested libraries can be used.

## Comments

#### supervisors notes

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
