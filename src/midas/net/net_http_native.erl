-module(net_http_native).
-export([listen/1, read_body/3, read_http/2, send/2]).


listen(Port) ->
  TcpOptions = [{mode, binary}, {packet, http_bin}, {active, false}, {reuseaddr, true}],
  gen_tcp:listen(Port, TcpOptions).


read_body(Socket, Length, Timeout) when Length > 0 ->
  inet:setopts(Socket, [{packet, raw}]),
  gen_tcp:recv(Socket, Length, Timeout).

read_http(Socket, Timeout) ->
  gen_tcp:recv(Socket, 0, Timeout).

send(Socket, String) ->
  case gen_tcp:send(Socket, String) of
    ok -> {ok, nil};
    {error, reason} -> {error, reason}
  end.
