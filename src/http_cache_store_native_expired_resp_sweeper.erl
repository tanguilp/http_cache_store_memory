-module(http_cache_store_native_expired_resp_sweeper).

-include("http_cache_store_native.hrl").

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(DEFAULT_INTERVAL, 7 * 1000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    Interval =
        application:get_env(
            application:get_application(), expired_resp_sweep_interval, ?DEFAULT_INTERVAL),
    schedule_sweep(Interval),
    {ok, Interval}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(sweep, Interval) ->
    sweep(),
    schedule_sweep(Interval),
    {noreply, Interval}.

sweep() ->
    do_sweep(ets:first(?OBJECT_TABLE)).

do_sweep('$end_of_table') ->
    ok;
do_sweep(ObjectKey) ->
    Now = unix_now(),
    case ets:lookup(?OBJECT_TABLE, ObjectKey) of
        [{_, _, _, _, _, Expires, _, _}] when Expires =< Now ->
            http_cache_store_native:delete_object(ObjectKey, expired);
        _ ->
            ok
    end,
    do_sweep(ets:next(?OBJECT_TABLE, ObjectKey)).

schedule_sweep(Interval) ->
    erlang:send_after(Interval, self(), sweep).

unix_now() ->
    os:system_time(second).
