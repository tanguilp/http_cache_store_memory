%% @private
-module(http_cache_store_native_lru_nuker).

-include("http_cache_store_native.hrl").

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(LIMIT_CHECK_INTERVAL, 200).
-define(START_NUKE_NB_OBJECTS, 200).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    schedule_check(),
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(check, State) ->
    telemetry:span([http_cache_store_native, lru_nuker],
                   #{},
                   fun() ->
                      nuke(?START_NUKE_NB_OBJECTS),
                      {ok, #{}}
                   end),
    schedule_check(),
    {noreply, State}.

nuke(NbObjects) ->
    AllocatedMemoryUsed = http_cache_store_native_stats:allocated_memory_used(),
    if AllocatedMemoryUsed < 0.99 ->
           http_cache_store_native_stats:set_limit_reached(false),
           ok;
       AllocatedMemoryUsed < 1 ->
           case nuke_objects(NbObjects) of
               table_empty ->
                   ok;
               ok ->
                   nuke(NbObjects * 2)
           end;
       true ->
           http_cache_store_native_stats:set_limit_reached(true),
           case nuke_objects(NbObjects) of
               table_empty ->
                   ok;
               ok ->
                   nuke(NbObjects * 2)
           end
    end.

nuke_objects(0) ->
    ok;
nuke_objects(NbObjects) ->
    case ets:first(?LRU_TABLE) of
        '$end_of_table' ->
            table_empty;
        {_, ObjectKey, SeqNumber} = LRUKey ->
            case ets:lookup(?OBJECT_TABLE, ObjectKey) of
                [{_, _, _, _, _, SeqNumber}] ->
                    ets:delete(?LRU_TABLE, LRUKey),
                    http_cache_store_native:delete_object(ObjectKey, lru_nuked),
                    nuke_objects(NbObjects - 1);
                % either the object exists, but with another sequence number which means the
                % current LRU key is outdated (a more recent one exists), or the object doesn't
                % exist anymore
                _ ->
                    ets:delete(?LRU_TABLE, LRUKey),
                    nuke_objects(NbObjects)
            end
    end.

schedule_check() ->
    erlang:send_after(limit_check_interval(), self(), check).

limit_check_interval() ->
    application:get_env(http_cache_store_native, limit_check_interval, ?LIMIT_CHECK_INTERVAL).
