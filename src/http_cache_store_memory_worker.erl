%% @private
-module(http_cache_store_memory_worker).

-include("http_cache_store_memory.hrl").

-export([start_link/1]).

start_link({cache_object, Object}) ->
    Pid = erlang:spawn_link(fun() -> cache_object(Object) end),
    {ok, Pid};
start_link({invalidate_url, UrlDigest}) ->
    Pid = erlang:spawn_link(fun() -> invalidate_url(UrlDigest) end),
    {ok, Pid};
start_link({invalidate_by_alternate_key, AltKeys}) ->
    Pid = erlang:spawn_link(fun() -> invalidate_by_alternate_key(AltKeys) end),
    {ok, Pid};
start_link({warm_me_up, {Node, NbObjects}}) ->
    Pid = erlang:spawn_link(fun() -> warmup_node(Node, NbObjects) end),
    {ok, Pid};
start_link({remote_object_available, {Node, {ObjectKey, Expires}}}) ->
    Pid = erlang:spawn_link(fun() -> maybe_request_cached_object(Node, ObjectKey, Expires) end),
    {ok, Pid};
start_link({remote_object_request, {Node, ObjectKey}}) ->
    Pid = erlang:spawn_link(fun() -> send_requested_object(Node, ObjectKey) end),
    {ok, Pid};
start_link({remote_object_response, Object}) ->
    Pid = erlang:spawn_link(fun() -> cache_object(Object) end),
    {ok, Pid}.

cache_object({RequestKey, UrlDigest, VaryHeaders, Response, RespMetadata}) ->
    ObjectKey = http_cache_store_memory:object_key(RequestKey, VaryHeaders, RespMetadata),
    SeqNumber = erlang:unique_integer(),
    ets:insert(
        ?OBJECT_TABLE,
        {ObjectKey, VaryHeaders, UrlDigest, Response, RespMetadata, SeqNumber}
    ),
    http_cache_store_memory:lru(ObjectKey, SeqNumber).

invalidate_url(UrlDigest) ->
    MatchSpec = [{{'_', '_', '$1', '_', '_', '_'}, [{'==', UrlDigest, '$1'}], [true]}],
    NbDeleted = ets:select_delete(?OBJECT_TABLE, MatchSpec),
    [
        telemetry:execute(
            [http_cache_store_memory, object_deleted],
            #{},
            #{reason => url_invalidation}
        )
     || _ <- lists:seq(0, NbDeleted - 1)
    ].

invalidate_by_alternate_key(AltKeys) ->
    MatchSpec =
        [
            {{'_', '_', '_', '_', #{alternate_keys => '$1'}, '_'}, [{is_map_key, AltKey, '$1'}], [
                true
            ]}
         || AltKey <- AltKeys
        ],
    NbDeleted = ets:select_delete(?OBJECT_TABLE, MatchSpec),
    [
        telemetry:execute(
            [http_cache_store_memory, object_deleted],
            #{},
            #{reason => alternate_key_invalidation}
        )
     || _ <- lists:seq(0, NbDeleted - 1)
    ].

warmup_node(Node, NbObjects) ->
    warmup_node(Node, ets:last(?LRU_TABLE), NbObjects).

warmup_node(_Node, '$end_of_table', _NbObjects) ->
    ok;
warmup_node(_Node, _Key, 0) ->
    ok;
warmup_node(Node, {_, ObjectKey, SeqNumber} = LRUKey, NbObjects) ->
    case ets:lookup(?OBJECT_TABLE, ObjectKey) of
        [{{RequestKey, _, _}, VaryHeaders, UrlDigest, Response, RespMetadata, SeqNumber}] ->
            CachedObject = {RequestKey, UrlDigest, VaryHeaders, Response, RespMetadata},
            http_cache_store_memory_cluster_mon:send_cached_object(Node, CachedObject),
            warmup_node(Node, ets:prev(?LRU_TABLE, LRUKey), NbObjects - 1);
        _ ->
            warmup_node(Node, ets:prev(?LRU_TABLE, LRUKey), NbObjects)
    end.

send_requested_object(Node, ObjectKey) ->
    case ets:lookup(?OBJECT_TABLE, ObjectKey) of
        [{{RequestKey, _, _}, VaryHeaders, UrlDigest, Response, RespMetadata, _}] ->
            CachedObject = {RequestKey, UrlDigest, VaryHeaders, Response, RespMetadata},
            http_cache_store_memory_cluster_mon:send_cached_object(Node, CachedObject);
        _ ->
            ok
    end.

maybe_request_cached_object(Node, ObjectKey, RemoteExpires) ->
    case ets:lookup(?OBJECT_TABLE, ObjectKey) of
        [] ->
            http_cache_store_memory_cluster_mon:request_cached_object(Node, ObjectKey);
        [{_, _, _, _, #{grace := Expires}, _}] when Expires < RemoteExpires ->
            http_cache_store_memory_cluster_mon:request_cached_object(Node, ObjectKey);
        _ ->
            ok
    end.
