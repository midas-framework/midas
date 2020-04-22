-module(midas_supervisor).

-behaviour(supervisor).

-export([start_link/2, init/1]).

start_link(Handler, Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Handler, Port]).


%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),       % mandatory
%%                  restart => restart(),    % optional
%%                  shutdown => shutdown(),  % optional
%%                  type => worker(),        % optional
%%                  modules => modules()}   % optional
init([Handler, Port]) ->
    {ok, ListenSocket} = midas_tcp_native:listen(Port),
    SupFlags = #{strategy => one_for_one,
                 % Why 0 in template
                 intensity => 10,
                 period => 1},

    ChildSpecs = [#{id => I, start => {midas@server, start_link, [ListenSocket, Handler]}} || I <- lists:seq(1, 100)],
    {ok, {SupFlags, ChildSpecs}}.
