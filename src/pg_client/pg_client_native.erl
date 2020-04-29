-module (pg_client_native).
-export ([start_link/1, query/2]).

start_link(OptionsList) ->
  Options = maps:from_list([ {K, erlang:binary_to_list(V)} || {K, V} <- OptionsList]),
  pgo_pool:start_link(default, Options).


query(String, GleamArgs) ->
  Args = [[Value || {_, Value} <- GleamArgs]],
  Return = pgo:query(String, Args),
  case Return of
    #{command := select, num_rows := Count, rows := Rows} ->
      {ok, {select, Count, Rows}}
  end.
