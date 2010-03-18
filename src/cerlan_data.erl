-module(cerlan_data).
-include("cerlan.hrl").
-include("cerlan_github.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([start/0, init/1, process_loop/0]).
-export([user_data/1, create_user/1, user_dates/1, all_users/0, important_users/0]).
-export([current_streak_users/0, longest_streak_users/0, refresh_user/1, iterate_users/1]).

% --
-compile(export_all).
% --

start() ->
    proc_lib:start_link(cerlan_data, init, [self()]).

init(Parent) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    register(cerlan_data, self()),
    timer:sleep(30000),
    process_loop().

process_loop() ->
    Sorted = lists:sort(fun(A, B) -> A#user.importance > B#user.importance end, all_users()),
    cerlan_data:iterate_users(Sorted),
    cerlan_data:process_loop().

iterate_users([]) -> ok;
iterate_users([User = #user{ last_updated = -1 } | Users]) ->
    cerlan_data:iterate_users(Users);
iterate_users([User | Users]) ->
    LastUpdated = case User#user.last_updated of
        {_, _, _} -> 90;
        0 -> 60 * 60 * 24 * 4;
        Other -> calendar:datetime_to_gregorian_seconds({date(), time()}) - Other
    end,
    case {LastUpdated, User#user.importance} of
        {L, 0} when L < 60 * 60 * 24 * 4 ->
            ok;
        {L, U} ->
            timer:sleep(2500),
            cerlan_data:refresh_user(User#user.username), ok
            % heman:stat_set(<<"cerlan_data">>, <<"users_processed">>, 1)
    end,
    cerlan_data:iterate_users(Users).

refresh_user(Username) ->
   case user_data(Username) of
        [User] ->
            Projects = try fetch_projects(User) catch
                X:Y ->
                    []
            end,
            cache_projects(User, Projects),
            Commits = collect_commits(User, Projects),
            update_user_days(User, Commits),
            Data = user_dates(User),
            LongStreak = find_streak(Data),
            CurrentStreak = find_current_streak(Data),
            Importance = gauge_importance(User#user{ longest_streak = LongStreak, current_streak = CurrentStreak }, Projects, Commits),
            update_user(User#user{
                longest_streak = LongStreak,
                current_streak = CurrentStreak,
                importance = Importance,
                last_updated = calendar:datetime_to_gregorian_seconds({date(), time()})
            });
        Other ->
            ok
    end.

gauge_importance(User, [], _Commits) ->
    %% No projects gets a 0
    0;
gauge_importance(User, _Projects, []) ->
    %% No commits gets 0
    0;
gauge_importance(User, _Projects, _Commits) when User#user.longest_streak == 0 ->
    %% No streak (has projects but no commits) gets a 0
    0;
gauge_importance(User, Projects, [{Last, _} | _] = Commits) ->
    Avg = lists:foldl(fun({_, X}, Y) -> length(X) + Y end, 0, Commits) / length(Commits),
    Modifier = calendar:date_to_gregorian_days(date()) - calendar:date_to_gregorian_days(Last) + 1,
    Total = 30 + User#user.longest_streak + User#user.current_streak + length(Projects) + Avg - Modifier,
    case Total < 30 of
        true -> 30;
        _ -> erlang:round(Total)
    end.

fetch_projects(User) ->
    case (catch githubby:user_repos({?LOGIN, ?TOKEN}, User#user.username)) of
        {struct, [{<<"repositories">>, Repos}]} ->
            lists:foldl(fun(Project, Projects) ->
                timer:sleep(1500),
                Branches = case (catch githubby:repos_refs({?LOGIN, ?TOKEN}, User#user.username, binary_to_list(Project))) of
                    {struct,[{<<"branches">>, {struct, X}}]} -> [Y || {Y, _} <- X];
                    X ->
                        [<<"master">>]
                end,
                [{Project, Branches} | Projects]
            end, [], [proplists:get_value(<<"name">>, Values) || {struct, Values} <- Repos]);
        X ->
            []
    end.

cache_projects(User, Projects) ->
    lists:foreach(
        fun({Project, Branches}) ->
            emongo:update(cerlan, "projects", [{"username", User#user.username}, {"project", Project}], [
                {"username", User#user.username},
                {"project", Project},
                {"branches", {array, Branches}}
            ], true)
        end,
        Projects
    ).

collect_commits(User, Projects) ->
    ExpandedProjects = lists:foldl(fun({Project, Branches}, Acc) ->
        Acc ++ [{Project, Branch} || Branch <- Branches]
    end, [], Projects),
    collect_commits(User, ExpandedProjects, dict:new()).

collect_commits(_, [], Dict) -> lists:reverse(lists:keysort(1, dict:to_list(Dict)));
collect_commits(User, [{Project, Branch} | Projects], Dict) ->
    timer:sleep(1000),
    Commits = try githubby:user_repos_commits({?LOGIN, ?TOKEN}, User#user.username, Project, Branch) of
        {struct, [{<<"commits">>, {struct, []}}]} -> [];
        {struct,[{<<"error">>, [{struct,[{<<"error">>,<<"api route not recognized">>}]}]}]} -> [];
        {struct, [{<<"commits">>, CommitList}]} -> CommitList
        catch
        X:Y -> []
    end,
    NewDict = lists:foldl(
        fun(CommitDate, TmpDict) ->
            [DateString|_] = string:tokens(CommitDate, "T"),
            Date = erlang:list_to_tuple([list_to_integer(X) || X <- string:tokens(DateString, "-")]),
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
    emongo:update(cerlan, "users", [{"username", User#user.username}], [
        {"id", User#user.id},
        {"username", User#user.username},
        {"longest_streak", User#user.longest_streak},
        {"current_streak", User#user.current_streak},
        {"last_updated", User#user.last_updated},
        {"importance", User#user.importance}
    ]).

update_user_days(User, Days) ->
    lists:foreach(
        fun({Day, Projects}) ->
             DayEpoch = calendar:datetime_to_gregorian_seconds({Day, {0, 0, 0}}),
             emongo:update(cerlan, "days", [{"username", User#user.username}, {"day", DayEpoch}], [
                 {"username", User#user.username},
                 {"day", DayEpoch},
                 {"projects", {array, lists:usort(Projects)}}
             ], true)
        end,
        Days
    ).

create_user(Username) ->
    case (catch githubby:user_info({?LOGIN, ?TOKEN}, Username)) of
        {struct, [{<<"user">>, {struct, UserData}}]} ->
            create_user(Username, proplists:get_value(<<"id">>, UserData)),
            ok;
        _ -> nop
    end.

create_user(Username, UserID) when is_list(Username) ->
    create_user(list_to_binary(Username), UserID);
create_user(Username, UserID) ->
    emongo:insert(cerlan, "users", [
        {"id", UserID},
        {"username", Username},
        {"longest_streak", 0},
        {"current_streak", 0},
        {"last_updated", 0}
    ]).

all_users() ->
    [begin
        #user{
            id = proplists:get_value(<<"id">>, User),
            username = proplists:get_value(<<"username">>, User),
            longest_streak = proplists:get_value(<<"longest_streak">>, User, 0),
            current_streak = proplists:get_value(<<"current_streak">>, User, 0),
            last_updated = proplists:get_value(<<"last_updated">>, User, 0),
            importance = proplists:get_value(<<"importance">>, User, 0)
        }
    end || User <- emongo:find_all(cerlan, "users", [], [])].

user_data(Username) when is_list(Username) ->
    user_data(list_to_binary(Username));
user_data(Username) ->
    transform_user(emongo:find_all(cerlan, "users", [{"username", Username}], [])).

transform_user(Users) ->
    [begin
        #user{
            id = proplists:get_value(<<"id">>, User),
            username = proplists:get_value(<<"username">>, User),
            longest_streak = proplists:get_value(<<"longest_streak">>, User, 0),
            current_streak = proplists:get_value(<<"current_streak">>, User, 0),
            last_updated = proplists:get_value(<<"last_updated">>, User, 0),
            importance = proplists:get_value(<<"importance">>, User, 0)
        }
    end || User <- Users].

user_dates(User) when is_record(User, user) ->
    user_dates(User#user.username);
user_dates(Username) when is_list(Username) ->
    user_dates(list_to_binary(Username));
user_dates(Username) ->
    [ begin
        {array, Projects} = proplists:get_value(<<"projects">>, Day),
        {TS, _} = calendar:gregorian_seconds_to_datetime(proplists:get_value(<<"day">>, Day)),
        #day{
            username = proplists:get_value(<<"username">>, Day),
            date = TS,
            projects = Projects
        }
    end || Day <- emongo:find(cerlan, "days", [{"username", Username}], [])].

current_streak_users() ->
    Users = transform_user(emongo:find(cerlan, "users", [], [{orderby, [{"current_streak", desc}]}, {limit, 25}])),
    SortedUsers = lists:sort(fun(A, B) -> A#user.current_streak > B#user.current_streak end, Users),
    {Top, _} = lists:split(10, SortedUsers),
    Top.

longest_streak_users() ->
    Users = transform_user(emongo:find(cerlan, "users", [], [{orderby, [{"longest_streak", desc}]}, {limit, 25}])),
    SortedUsers = lists:sort(fun(A, B) -> A#user.longest_streak > B#user.longest_streak end, Users),
    {Top, _} = lists:split(10, SortedUsers),
    Top.

important_users() ->
    Users = transform_user(emongo:find(cerlan, "users", [], [{orderby, [{"importance", desc}]}, {limit, 25}])),
    SortedUsers = lists:sort(fun(A, B) -> A#user.importance > B#user.importance end, Users),
    {Top, _} = lists:split(10, SortedUsers),
    Top.

