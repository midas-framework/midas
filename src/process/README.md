# Gleam processes

- For this version if a process might accept DOWN or EXIT messages the user needs to remember to add them to the protocol.
  This is to give maximum control to implementers of supervisors etc.
  Also this level of abstraction is infrequently changed so well tested libraries can be used.

## Comments

#### Naming of Pid

I would like to rename the type `Pid(m)` to `Address(m)` and `BarePid` to Pid.
However I think from an understanding point of view for new people explaining that there are now typed pids is easier than explaining what an address is.

#### Handling DOWN & EXIT messages, in worker processes

Enforcing every worker process to handle DOWN messages, that might never arrive is verbose.
Also before completeness checking its just as easy to forget a case clause as it is to forget to add a down/exit message to a processes protocol.
Because this is also low level it is up to the implementer to handle down exit where necessary, all other types in a the protocol are checked.

### Derive self safely from receive

A receive function that took a pid (which should only be self) was too easy to pass other pids too
