-module(cerlan_data).
-include("cerlan.hrl").
-include("cerlan_github.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([start/0, init/1, process_loop/0]).
-export([user_data/1, create_user/1, user_dates/1, all_users/0]).
-export([current_streak_users/0, longest_streak_users/0, force_refresh_user/1]).

start() ->
    proc_lib:start_link(cerlan_data, init, [self()]).

init(Parent) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    timer:sleep(30000),
    process_loop().

process_loop() ->
    heman:stat_set(<<"cerlan_data">>, <<"process_loop">>, 1),
    Users = mnesia:activity(transaction, fun() -> qlc:e( qlc:q([R || R <- mnesia:table(user) ]) ) end),
    [begin
        timer:sleep(2500),
        (catch force_refresh_user(User#user.username)),
        heman:stat_set(<<"cerlan_data">>, <<"users_processed">>, 1)
    end || User <- Users],
    heman:stat_set(<<"cerlan_data">>, <<"users_processed">>, length(Users)),
    cerlan_data:process_loop().

force_refresh_user(Username) ->
    case user_data(Username) of
        [User] ->
            Projects = try fetch_projects(User) catch
                _:_ ->
                    heman:stat_set(<<"cerlan_data">>, <<"bad_project_fetch">>, 1),
                    []
            end,
            heman:stat_set(<<"cerlan_data">>, <<"known_projects">>, length(Projects)),
            cache_projects(User, Projects),
            Commits = collect_commits(User, Projects),
            heman:stat_set(<<"cerlan_data">>, <<"known_commits">>, length(Commits)),
            update_user_days(User, Commits),
            Data = user_dates(User),
            update_user(User#user{
                longest_streak = find_streak(Data),
                current_streak = find_current_streak(Data),
                last_updated = date()
            });
        _ -> ok
    end.

fetch_projects(User) ->
    heman:stat_set(<<"cerlan_data">>, <<"fetch_projects">>, 1),
    case githubby:user_repos({?LOGIN, ?TOKEN}, User#user.username) of
        {struct, [{<<"repositories">>, Repos}]} ->
            [proplists:get_value(<<"name">>, Values) || {struct, Values} <- Repos];
        _ -> []
    end.

cache_projects(User, Projects) ->
    lists:foreach(
        fun(Project) ->
            heman:stat_set(<<"cerlan_data">>, <<"project_cache">>, 1),
            (catch mnesia:transaction(fun() ->
                mnesia:write(#project{
                    id = {User#user.username, Project},
                    username = User#user.username,
                    project = Project
                })
            end))
        end,
        [binary_to_list(X) || X <- Projects]
    ).

collect_commits(User, Projects) ->
    collect_commits(User, [binary_to_list(X) || X <- Projects], dict:new()).

collect_commits(_, [], Dict) -> dict:to_list(Dict);
collect_commits(User, [Project | Projects], Dict) ->
    timer:sleep(1000),
    Commits = case githubby:user_repos_commits({?LOGIN, ?TOKEN}, User#user.username, Project) of
        {struct, [{<<"commits">>, {struct, []}}]} -> [];
        {struct, [{<<"commits">>, CommitList}]} -> CommitList;
        _ -> []
    end,
    NewDict = lists:foldl(
        fun(CommitDate, TmpDict) ->
            [DateString|_] = string:tokens(CommitDate, "T"),
            Date = list_to_tuple([list_to_integer(X) || X <- string:tokens(DateString, "-")]),
            case dict:is_key(Date, TmpDict) of
                true ->
                    dict:update(Date, fun(Old) -> [Project | Old] end, [Project], TmpDict);
                false ->
                    dict:store(Date, [Project], TmpDict)
            end
        end,
        Dict,
        [binary_to_list(proplists:get_value(<<"authored_date">>, Values)) || {struct, Values} <- Commits, length(Values) > 0]
    ),
    collect_commits(User, Projects, NewDict).

find_streak(Days) ->
    Data = lists:sort([calendar:date_to_gregorian_days(Day#day.date) || Day <- Days]),
    case Data of
        [] -> 0;
        [_] -> 1;
        [X | Y] ->
            calc_long_streak([0], Y, X)
    end.

calc_long_streak(Streaks, [], _) -> lists:max(Streaks);
calc_long_streak([CurrentStreak | PastStreaks], [Day | DaysLeft], Last) ->
    case Day == Last + 1 of
        true ->
            calc_long_streak([CurrentStreak + 1 | PastStreaks], DaysLeft, Day);
        false ->
            calc_long_streak([1, CurrentStreak | PastStreaks], DaysLeft, Day)
    end.

find_current_streak(Dates) ->
    Today = calendar:date_to_gregorian_days(date()),
    case lists:reverse(lists:usort([calendar:date_to_gregorian_days(CagDay#day.date) || CagDay <- Dates])) of
        [] -> 0;
        [Today] -> 1;
        [_] -> 0;
        [LastDay | _] when LastDay =/= Today -> 0;
        [Today | _] = Data -> calc_current_streak(0, [Today | Data])
    end.

calc_current_streak(Acc, []) -> Acc;
calc_current_streak(Acc, [H | T]) -> calc_current_streak(Acc, T, H).

calc_current_streak(Acc, [], _) ->
    Acc;
calc_current_streak(Acc, [Day | Days], Last) when Day == Last ->
    calc_current_streak(Acc + 1, Days, Day);
calc_current_streak(Acc, [Day | Days], Last) when Day == Last - 1 ->
    calc_current_streak(Acc + 1, Days, Day);
calc_current_streak(Acc, _, _) ->
    Acc.

%% % -

update_user(User) ->
    mnesia:transaction(fun() -> mnesia:write(User) end).

update_user_days(User, Days) ->
    lists:foreach(
        fun({Day, Projects}) ->
            (catch mnesia:transaction(fun() ->
                mnesia:write(#day{
                    id = {User#user.username, Day},
                    username = User#user.username,
                    date = Day,
                    projects = lists:usort(Projects)
                })
            end))
        end,
        Days
    ).

create_user(Username) ->
    case (catch githubby:user_info({?LOGIN, ?TOKEN}, Username)) of
        {struct, [{<<"user">>, {struct, UserData}}]} ->
            create_user(Username, proplists:get_value(<<"id">>, UserData));
        _ -> nop
    end.

create_user(Username, UserID) ->
    (catch mnesia:transaction(fun() ->
        mnesia:write(#user{
            id = UserID,
            username = Username,
            longest_streak = 0,
            current_streak = 0
        })
    end)).

all_users() ->
    mnesia:activity(transaction, fun() -> qlc:e( qlc:q([R || R <- mnesia:table(user) ]) ) end).

user_data(Username) ->
    user_data(Username, date()).

user_data(Username, _Date) ->
    mnesia:activity(transaction, fun() ->
        Q = qlc:q([R || R <- mnesia:table(user), R#user.username == Username]),
        qlc:e(Q)
    end).

user_dates(User) ->
    mnesia:activity(transaction, fun() ->
        Q = qlc:q([R || R <- mnesia:table(day), R#day.username == User#user.username]),
        qlc:e(Q)
    end).

current_streak_users() ->
    Users = mnesia:activity(transaction, fun() ->
        Q = qlc:q([R || R <- mnesia:table(user)]),
        qlc:e(Q)
    end),
    SortedUsers = lists:sort(fun(A, B) -> A#user.current_streak > B#user.current_streak end, Users),
    {Top, _} = lists:split(10, SortedUsers),
    Top.

longest_streak_users() ->
    Users = mnesia:activity(transaction, fun() ->
        Q = qlc:q([R || R <- mnesia:table(user)]),
        qlc:e(Q)
    end),
    SortedUsers = lists:sort(fun(A, B) -> A#user.longest_streak > B#user.longest_streak end, Users),
    {Top, _} = lists:split(10, SortedUsers),
    Top.

