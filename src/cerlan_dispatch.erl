-module(cerlan_dispatch).
-include("cerlan.hrl").
-export([dispatch/1]).

dispatch(Req) ->
    handle_request(Req:get(path), Req).

handle_request("/images/x_1.png", Req) ->
    {ok, Data} = file:read_file("x_1.png"),
    Req:respond({200, [{<<"content-type">>, <<"image/png">>}], Data});
    
handle_request("/images/x_2.png", Req) ->
    {ok, Data} = file:read_file("x_2.png"),
    Req:respond({200, [{<<"content-type">>, <<"image/png">>}], Data});
    
handle_request("/images/x_3.png", Req) ->
    {ok, Data} = file:read_file("x_3.png"),
    Req:respond({200, [{<<"content-type">>, <<"image/png">>}], Data});

handle_request("/styles/main.css", Req) ->
    Body = erlang:iolist_to_binary(cerlan_textra:css()),
    Req:respond({200, [{<<"content-type">>, <<"text/html">>}], Body});

handle_request("/", Req) ->
    Body = erlang:iolist_to_binary(cerlan_thome:index(
        [{X#user.username, X#user.current_streak} || X <- cerlan_data:current_streak_users(), X#user.current_streak > 0],
        [{X#user.username, X#user.longest_streak} || X <- cerlan_data:longest_streak_users(), X#user.longest_streak > 0],
        [{X#user.username, X#user.longest_streak} || X <- cerlan_data:important_users(), X#user.importance > 0]
    )),
    Req:respond({200, [{<<"content-type">>, <<"text/html">>}], Body});

handle_request("/faq", Req) ->
    Body = erlang:iolist_to_binary(cerlan_tfaq:index()),
    Req:respond({200, [{<<"content-type">>, <<"text/html">>}], Body});

handle_request("/update/\~" ++ Username, Req) ->
    Parent = self(),
    spawn(fun() ->
        cerlan_data:force_refresh_user(Username),
        Parent ! done
    end),
    receive done -> ok after 20000 -> ok end,
    Req:respond({302, [{<<"Location">>, list_to_binary("/\~" ++ Username)}], <<"ok">>});

handle_request("/all", Req) ->
    Users = lists:sort(fun(A, B) ->
        case A#user.last_updated == B#user.last_updated of
            true ->
                case A#user.importance == B#user.importance of
                    true ->
                        A#user.username < B#user.username;
                    _ ->
                        A#user.importance > B#user.importance
                end;
            _ ->
                A#user.last_updated > B#user.last_updated
        end
    end, cerlan_data:all_users()),
    Body = erlang:iolist_to_binary(cerlan_thome:all([begin
        {TS, _} = calendar:gregorian_seconds_to_datetime(X#user.last_updated),
        {X#user.username, X#user.longest_streak, TS, erlang:round(X#user.importance)}
    end || X <- Users, X#user.importance =/= 0])),
    Req:respond({200, [{<<"content-type">>, <<"text/html">>}], Body});

handle_request("/json/\~" ++ RawUsername, Req) ->
    {Y, M, {Username, Projects}} = case string:tokens(RawUsername, "/") of
        [UsernameA] ->
            {Ya, Ma, _} = date(), {Ya, Ma, split_projects(UsernameA)};
        [UsernameA, Ya, Ma] ->
            {list_to_integer(Ya), list_to_integer(Ma), split_projects(UsernameA)}
    end,
    Body = case cerlan_data:user_data(Username) of
        [] ->
            erlang:iolist_to_binary(mochijson2:encode({struct, [{<<"error">>, <<"No such user.">>}]}));
        [User] ->
            UserData = [X || X = {_, _, _, {Y1, M1, _}, _} <- cerlan_data:user_dates(User), Y1 == Y, M1 == M],
            ActiveProjecst = build_active_projects(UserData),
            UnfilteredUserData = filtered_user_dates(Projects, UserData),
            Data = lists:reverse(gen_cal(Y, M, UnfilteredUserData)),
            UnencodedJSON = {struct, [
                {<<"user">>, User#user.username},
                {<<"current_streak">>, User#user.current_streak},
                {<<"longest_streak">>, User#user.longest_streak},
                {<<"projects">>, ActiveProjecst},
                {<<"days">>, [Day || {Day, ok} <- lists:flatten(Data)]}
            ]},
            erlang:iolist_to_binary(mochijson2:encode(UnencodedJSON))
    end,
    Req:respond({200, [{<<"content-type">>, <<"application/json">>}], Body});

handle_request("/\~" ++ RawUsername, Req) ->
    {Y, M, {Username, Projects}} = case string:tokens(RawUsername, "/") of
        [UsernameA] ->
            {Ya, Ma, _} = date(), {Ya, Ma, split_projects(UsernameA)};
        [UsernameA, Ya, Ma] ->
            {list_to_integer(Ya), list_to_integer(Ma), split_projects(UsernameA)}
    end,
    Body = case cerlan_data:user_data(Username) of
        [] ->
            spawn(fun() ->
                cerlan_data:create_user(Username)
            end),
            erlang:iolist_to_binary(cerlan_tuser:index(
                {Username, Username},
                "0",
                "0",
                "0",
                integer_to_list(M),
                integer_to_list(Y),
                [],
                last_month(Y, M),
                next_month(Y, M),
                [],
                [{warning, "We don't know about that user but they have been added to our processing list."}]
            ));
        [User] ->
            UserData = [X || X = {_, _, _, {Y1, M1, _}, _} <- cerlan_data:user_dates(User), Y1 == Y, M1 == M],
            ActiveProjecst = build_active_projects(UserData),
            UnfilteredUserData = filtered_user_dates(Projects, UserData),
            Data = lists:reverse(gen_cal(Y, M, UnfilteredUserData)),
            erlang:iolist_to_binary(cerlan_tuser:index(
                {Username, compose_url(Username, Projects)},
                integer_to_list(User#user.longest_streak),
                integer_to_list(User#user.current_streak),
                integer_to_list(erlang:round(User#user.importance)),
                integer_to_list(M),
                integer_to_list(Y),
                Data,
                last_month(Y, M),
                next_month(Y, M),
                ActiveProjecst,
                []
            ))
    end,
    Req:respond({200, [{<<"content-type">>, <<"text/html">>}], Body});

handle_request(_Other, Req) ->
    Req:respond({200, [{<<"content-type">>, <<"text/html">>}], <<"<h1>Not found.</h1>">>}).

gen_cal(Y, M, Dates) ->
    FirstDayOfTheMonth = calendar:day_of_the_week({Y, M, 1}),
    TotalDays = calendar:last_day_of_the_month(Y, M),
    Vals = gen_cal_range(1, lists:usort([Day || {_, _ , _, {_,_, Day}, _} <- Dates]), []),
    FinalVals = case length(Vals) of
        TotalDays -> Vals;
        _ -> Vals ++ [{N, nok} || N <- lists:seq(length(Vals) + 1, TotalDays)]
    end,
    split_range(1, lists:duplicate(FirstDayOfTheMonth, old) ++ FinalVals, [[]]).

gen_cal_range(_, [], Acc) -> lists:reverse(Acc);
gen_cal_range(Day, Dates, Acc) ->
    {Value, NewDates} = case lists:member(Day, Dates) of
        true -> {ok, lists:delete(Day, Dates)};
        false -> {nok, Dates}
    end,
    gen_cal_range(Day + 1, NewDates, [{Day, Value}| Acc]).

split_range(_, [], [X | Y]) ->
    case length(X) of
        7 ->
            [lists:reverse(X) | Y];
        N -> [lists:sort(X) ++ lists:duplicate(7 - N, new) | Y]
    end;
split_range(8, Vals, [X | Y]) ->
    split_range(1, Vals, [[], lists:sort(X) | Y]);
split_range(N, [Val | Vals], [X | Y]) ->
    split_range(N + 1, Vals, [[Val | X] | Y]).

last_month(Y, M) ->
    A = calendar:datetime_to_gregorian_seconds({{Y, M, 1}, {1, 1, 1}}),
    B = A - (60 * 60 * 48),
    {{Yn, Mn, _}, _} = calendar:gregorian_seconds_to_datetime(B),
    {integer_to_list(Yn), integer_to_list(Mn)}.

next_month(Y, M) ->
    LastDay = calendar:last_day_of_the_month(Y, M),
    A = calendar:datetime_to_gregorian_seconds({{Y, M, LastDay}, {1, 1, 1}}),
    B = A + (60 * 60 * 48),
    {{Yn, Mn, _}, _} = calendar:gregorian_seconds_to_datetime(B),
    {integer_to_list(Yn), integer_to_list(Mn)}.

split_projects(Username) ->
    [UsernameA | Others] = string:tokens(Username, ","),
    {UsernameA, Others}.

filtered_user_dates([], UnfilteredUserData) -> UnfilteredUserData;
filtered_user_dates(Projects, UnfilteredUserData) ->
    lists:filter(
        fun(Day) ->
            lists:member(true, [lists:member(X, Projects) || X <- Day#day.projects])
        end,
        UnfilteredUserData
    ).

build_active_projects(UserData) ->
    lists:usort(
        lists:foldl(fun(X, Acc) -> X#day.projects ++ Acc end, [], UserData)
    ).

compose_url(Username, Projects) ->
    string:join([Username | Projects], ",").

