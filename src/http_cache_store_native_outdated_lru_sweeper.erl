-module(http_cache_store_native_outdated_lru_sweeper).

-include("http_cache_store_native.hrl").

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(DEFAULT_INTERVAL, 5 * 1000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    Interval =
        application:get_env(
            http_cache_store_native, outdated_lru_sweep_interval, ?DEFAULT_INTERVAL),
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
    do_sweep(ets:first(?LRU_TABLE)).

do_sweep('$end_of_table') ->
    ok;
do_sweep({_, ObjectKey, SeqNumber} = LRUKey) ->
    case ets:lookup(?OBJECT_TABLE, ObjectKey) of
        [{_, _, _, _, _, _, _, SeqNumber}] ->
            ok;
        _ ->
            ets:delete(?LRU_TABLE, LRUKey)
    end,
    do_sweep(ets:next(?LRU_TABLE, LRUKey)).

schedule_sweep(Interval) ->
    erlang:send_after(Interval, self(), sweep).
