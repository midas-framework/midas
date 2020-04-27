-module(core_process_native).
-export([monitor/1, receive_reply/2, identity/1]).

monitor(Pid) ->
  erlang:monitor(process, Pid).

receive_reply(From, {milliseconds, Milliseconds}) ->
    receive_reply(From, Milliseconds);
receive_reply({from, Ref, _Pid}, Wait) ->
  receive
      {Ref, Message} ->
          erlang:demonitor(Ref, [flush]),
          {ok, Message};
      {'DOWN', Ref, process, _Pid2, _Reason} ->
          {error, down}
  after Wait ->
      erlang:demonitor(Ref, [flush]),
      {error, timeout}
  end.

identity(X) -> X.
