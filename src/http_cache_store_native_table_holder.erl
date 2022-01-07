-module(http_cache_store_native_table_holder).

-include("http_cache_store_native.hrl").

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    ets:new(?CONFIG_TABLE, [named_table, public]),
    ets:new(?OBJECT_TABLE, [ordered_set, named_table, public, {write_concurrency, true}]),
    ets:new(?LRU_TABLE, [ordered_set, named_table, public]),
    http_cache_store_native_stats:set_limit_reached(false),
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.
