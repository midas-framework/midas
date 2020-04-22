-module(midas_app).

-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1, init/1]).

start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.

%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),       % mandatory
%%                  restart => restart(),    % optional
%%                  shutdown => shutdown(),  % optional
%%                  type => worker(),        % optional
%%                  modules => modules()}   % optional
init([]) ->
    {ok, ListenSocket} = midas_tcp_native:listen(8080),
    SupFlags = #{strategy => one_for_all,
                 % Why 0 in template
                 intensity => 10,
                 period => 1},

    ChildSpecs = [#{id => I, start => {midas@server, start_link, [ListenSocket]}} || I <- lists:seq(1, 100)],
    {ok, {SupFlags, ChildSpecs}}.
