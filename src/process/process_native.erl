-module(process_native).
-export([spawn_link/1, do_receive/1, identity/1, receive_reply/2, process_flag/1]).

spawn_link(Run) ->
  erlang:spawn_link(fun () ->
      Run(fun (Wait) ->
          do_receive(Wait)
      end)
  end).

do_receive({milliseconds, Milliseconds}) ->
    do_receive(Milliseconds);
do_receive(Wait) ->
    receive
        {'DOWN', Ref, process, Pid2, Reason} ->
            {some, {down, Ref, process, Pid2, Reason}};
        {'EXIT', Pid, Reason} ->
            {some, {exit, Pid, Reason}};
        Message ->
            {some, Message}
    after Wait ->
         none
    end.

identity(X) -> X.

process_flag({Flag, Value}) ->
  OldValue = erlang:process_flag(Flag, Value),
  {Flag, OldValue}.

receive_reply(Ref, {milliseconds, Milliseconds}) ->
    receive_reply(Ref, Milliseconds);
receive_reply(Ref, Wait) ->
  receive
      {Ref, Message} ->
          erlang:demonitor(Ref, [flush]),
          {ok, Message};
      {'DOWN', Ref, process, _Pid2, _Reason} ->
          {error, gone}
  after Wait ->
      erlang:demonitor(Ref, [flush]),
      {error, timeout}
  end.
