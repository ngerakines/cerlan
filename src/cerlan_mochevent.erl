-module(cerlan_mochevent).
-export([start/0, dispatch/1, init/1, process_loop/0]).

start() ->
    mochiweb_http:start([{name, webproc}, {ip, "0.0.0.0"}, {port, 8084}, {loop, fun cerlan_mochevent:dispatch/1}]).
    %% proc_lib:start_link(cerlan_mochevent, init, [self()]).

dispatch(Req) ->
    cerlan_dispatch:dispatch(Req).

init(Parent) ->
    register(mochevent_handler, self()),
    proc_lib:init_ack(Parent, {ok, self()}),
    cerlan_mochevent:process_loop().

process_loop() ->
    receive
        {Pid, Id, Method, Uri, Headers, Body} ->
            spawn(fun() -> 
                Req = mochevent_request:new(Pid, Id, Method, Uri, {1, 0}, mochiweb_headers:make(Headers), Body),
                cerlan_dispatch:dispatch(Req)
            end)
    end,
    cerlan_mochevent:process_loop().
