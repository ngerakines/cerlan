-module(cerlan).
-behaviour(application).
-include("cerlan.hrl").
-export([start/2, stop/1, init/1, start/0, start_phase/3, reload/0]).

start() ->
    inets:start(),
    mnesia:start(),
    crypto:start(),
    net_adm:world(),
    application:start(cerlan).

start(_Type, _Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_phase(mnesia, _, _) ->
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    mnesia:create_schema([node()]),
    case mnesia:system_info(tables) of
        [schema] ->
            mnesia:create_table(user, [{disc_copies, [node()]}, {record_name, user}, {attributes, record_info(fields, user)}]),
            mnesia:create_table(current_streaks, [{disc_copies, [node()]}, {attributes, record_info(fields, user)}]),
            mnesia:create_table(longest_streaks, [{disc_copies, [node()]}, {attributes, record_info(fields, user)}]),
            mnesia:create_table(day, [{disc_copies, [node()]}, {record_name, day}, {attributes, record_info(fields, day)}]),
            mnesia:create_table(project, [{disc_copies, [node()]}, {record_name, project}, {attributes, record_info(fields, project)}]),
            mnesia:add_table_index(user, username),
            mnesia:add_table_index(user, username),
            mnesia:add_table_index(day, date),
            ok;
        _ ->
            ok
    end,
    mnesia:wait_for_tables([user, day], 5000),
    ok;

start_phase(world, _, _) ->
    net_adm:world(),
    ok;

start_phase(pg2, _, _) ->
    pg2:which_groups(),
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
