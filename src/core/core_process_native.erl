-module(core_process_native).
-export([monitor/1, receive_reply/1]).

monitor(Pid) ->
  erlang:monitor(process, Pid).

receive_reply({from, Ref, _Pid}) ->
  receive
    {Ref, Message} ->
      {ok, Message}
      % TODO handle Down or Exir
  end.
