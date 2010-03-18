-module(cerlan).
-behaviour(application).
-include("cerlan.hrl").
-export([start/2, stop/1, init/1, start/0, start_phase/3, reload/0]).

start() ->
    inets:start(),
    crypto:start(),
    net_adm:world(),
    application:start(emongo),
    application:start(cerlan).

start(_Type, _Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_phase(emongo, _, _) ->
    emongo:add_pool(cerlan, "localhost", 27017, "cerlan", 1),
    ok.

stop(_State) ->
    ok.

init(_) ->
    {ok, {{one_for_one, 2, 10}, [
        {cerlan_mochevent, {cerlan_mochevent, start, []}, permanent, 2000, worker, [cerlan_mochevent]},
        {cerlan_data, {cerlan_data, start,[]}, permanent, 2000, worker, [cerlan_data]}
    ]}}.

reload() ->
    [begin
        code:soft_purge(X),
        code:load_abs("./ebin/" ++ atom_to_list(X))
    end || X <- [cerlan, cerlan_dispatch, cerlan_mochevent, cerlan_data, cerlan_thome, cerlan_tuser, cerlan_textra]].

