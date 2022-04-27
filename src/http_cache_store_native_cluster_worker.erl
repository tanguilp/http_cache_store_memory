-module(http_cache_store_native_cluster_worker).

-include("http_cache_store_native.hrl").

-export([start_link/1]).

start_link({invalidate_url, UrlDigest}) ->
    Pid = erlang:spawn_link(http_cache_store_native,
                            invalidate_url_no_broadcast,
                            [UrlDigest]),
    {ok, Pid};
start_link({invalidate_by_alternate_key, AltKeys}) ->
    Pid = erlang:spawn_link(http_cache_store_native,
                            invalidate_by_alternate_key_no_broadcast,
                            [AltKeys]),
    {ok, Pid};
start_link({warm_me_up, {Node, NbObjects}}) ->
    Pid = erlang:spawn_link(fun() -> warmup_node(Node, NbObjects) end),
    {ok, Pid};
start_link({object_cached, {Node, {ObjectKey, Expires}}}) ->
    Pid = erlang:spawn_link(fun() -> maybe_request_cached_object(Node, ObjectKey, Expires)
                            end),
    {ok, Pid};
start_link({get_object, {Node, ObjectKey}}) ->
    Pid = erlang:spawn_link(fun() -> send_requested_object(Node, ObjectKey) end),
    {ok, Pid};
start_link({cached_object, CachedObject}) ->
    Pid = erlang:spawn_link(http_cache_store_native, put, tuple_to_list(CachedObject)),
    {ok, Pid}.

warmup_node(Node, NbObjects) ->
    warmup_node(Node, ets:last(?LRU_TABLE), NbObjects).

warmup_node(_Node, '$end_of_table', _NbObjects) ->
    ok;
warmup_node(_Node, _Key, 0) ->
    ok;
warmup_node(Node, {_, ObjectKey, SeqNumber} = LRUKey, NbObjects) ->
    case ets:lookup(?OBJECT_TABLE, ObjectKey) of
        [{{RequestKey, _},
          VaryHeaders,
          UrlDigest,
          Response,
          RespMetadata,
          _,
          SeqNumber}] ->
            CachedObject = {RequestKey, UrlDigest, VaryHeaders, Response, RespMetadata},
            http_cache_store_native_cluster_mon:send_cached_object(Node, CachedObject),
            warmup_node(Node, ets:prev(?LRU_TABLE, LRUKey), NbObjects - 1);
        _ ->
            warmup_node(Node, ets:prev(?LRU_TABLE, LRUKey), NbObjects)
    end.

send_requested_object(Node, ObjectKey) ->
    case ets:lookup(?OBJECT_TABLE, ObjectKey) of
        [{{RequestKey, _}, VaryHeaders, UrlDigest, Response, RespMetadata, _, _}] ->
            CachedObject = {RequestKey, UrlDigest, VaryHeaders, Response, RespMetadata},
            http_cache_store_native_cluster_mon:send_cached_object(Node, CachedObject);
        _ ->
            ok
    end.

maybe_request_cached_object(Node, ObjectKey, RemoteExpires) ->
    case ets:lookup(?OBJECT_TABLE, ObjectKey) of
        [] ->
            http_cache_store_native_cluster_mon:request_cached_object(Node, ObjectKey);
        [{_, _, _, _, _, Expires, _}] when Expires < RemoteExpires ->
            http_cache_store_native_cluster_mon:request_cached_object(Node, ObjectKey);
        _ ->
            ok
    end.
