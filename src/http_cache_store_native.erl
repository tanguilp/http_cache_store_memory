-module(http_cache_store_native).

-include("http_cache_store_native.hrl").

-behaviour(http_cache_store).

-export([list_candidates/1, get_response/1, put/6, put_no_broadcast/6, notify_resp_used/2,
         invalidate_url/1, invalidate_url_no_broadcast/1, invalidate_by_alternate_key/1,
         invalidate_by_alternate_key_no_broadcast/1]).
-export([delete_object/2]).

list_candidates(RequestKey) ->
    Spec =
        [{{{RequestKey, '$1'}, '$2', '_', {'$3', '$4', '_'}, '$5', '$6', '_', '_'},
          [],
          [['$1', '$2', '$3', '$4', '$5', '$6']]}],
    Now = unix_now(),
    [{{RequestKey, SecondKeyPart}, Status, RespHeaders, VaryHeaders, RespMetadata}
     || [SecondKeyPart, VaryHeaders, Status, RespHeaders, RespMetadata, Expires]
            <- ets:select(?OBJECT_TABLE, Spec),
        Now < Expires].

get_response(ObjectKey) ->
    case ets:lookup(?OBJECT_TABLE, ObjectKey) of
        [{_ObjectKey,
          _VaryHeaders,
          _UrlDigest,
          {Status, RespHeaders, RespBody},
          RespMetadata,
          _Expires,
          _AlternateKeys,
          _SeqNumber}] ->
            {Status, RespHeaders, RespBody, RespMetadata};
        [] ->
            undefined
    end.

put(RequestKey, UrlDigest, VaryHeaders, Response, RespMetadata, AlternateKeys) ->
    put(RequestKey, UrlDigest, VaryHeaders, Response, RespMetadata, AlternateKeys, true).

put_no_broadcast(RequestKey,
                 UrlDigest,
                 VaryHeaders,
                 Response,
                 RespMetadata,
                 AlternateKeys) ->
    put(RequestKey, UrlDigest, VaryHeaders, Response, RespMetadata, AlternateKeys, false).

put(RequestKey,
    UrlDigest,
    VaryHeaders,
    Response,
    RespMetadata,
    AlternateKeys,
    DoBroadcast) ->
    case http_cache_store_native_stats:is_limit_reached() of
        true ->
            {error, store_overloaded};
        false ->
            do_put(RequestKey,
                   UrlDigest,
                   VaryHeaders,
                   Response,
                   RespMetadata,
                   AlternateKeys,
                   DoBroadcast)
    end.

do_put(RequestKey,
       UrlDigest,
       VaryHeaders,
       Response,
       RespMetadata,
       AlternateKeys,
       DoBroadcast) ->
    ObjectKey = object_key(RequestKey, VaryHeaders),
    SeqNumber = new_seq_number(),
    Expires = map_get(grace, RespMetadata),
    ets:insert(?OBJECT_TABLE,
               {ObjectKey,
                VaryHeaders,
                UrlDigest,
                Response,
                RespMetadata,
                Expires,
                AlternateKeys,
                SeqNumber}),
    lru(ObjectKey, unix_now(), SeqNumber),
    case DoBroadcast of
        true ->
            http_cache_store_native_cluster_mon:broadcast_cached_object(ObjectKey, Expires);
        false ->
            ok
    end,
    ok.

delete_object(ObjectKey, Reason) ->
    telemetry:execute([http_cache_store_native, object_deleted], #{}, #{reason => Reason}),
    ets:delete(?OBJECT_TABLE, ObjectKey),
    ok.

invalidate_url(UrlDigest) ->
    http_cache_store_native_cluster_mon:broadcast_invalidate_url(UrlDigest),
    do_invalidate_url(UrlDigest, ets:first(?OBJECT_TABLE), 0).

invalidate_url_no_broadcast(UrlDigest) ->
    do_invalidate_url(UrlDigest, ets:first(?OBJECT_TABLE), 0).

do_invalidate_url(_UrlDigest, '$end_of_table', NbDeleted) ->
    {ok, NbDeleted};
do_invalidate_url(UrlDigest, ObjectKey, NbDeleted) ->
    case ets:lookup(?OBJECT_TABLE, ObjectKey) of
        [{_, _, UrlDigest, _, _, _, _, _}] ->
            delete_object(ObjectKey, url_invalidation),
            do_invalidate_url(UrlDigest, ets:next(?OBJECT_TABLE, ObjectKey), NbDeleted + 1);
        _ ->
            do_invalidate_url(UrlDigest, ets:next(?OBJECT_TABLE, ObjectKey), NbDeleted)
    end.

notify_resp_used(ObjectKey, When) ->
    SeqNumber = new_seq_number(),
    case ets:update_element(?OBJECT_TABLE, ObjectKey, {8, SeqNumber}) of
        true ->
            lru(ObjectKey, When, SeqNumber),
            ok;
        false ->
            ok
    end.

invalidate_by_alternate_key(AltKeys) ->
    http_cache_store_native_cluster_mon:broadcast_invalidate_by_alternate_key(AltKeys),
    do_invalidate_by_alternate_key(AltKeys, ets:first(?OBJECT_TABLE), 0).

invalidate_by_alternate_key_no_broadcast(AltKeys) ->
    do_invalidate_by_alternate_key(AltKeys, ets:first(?OBJECT_TABLE), 0).

do_invalidate_by_alternate_key(_AltKeys, '$end_of_table', NbDeleted) ->
    {ok, NbDeleted};
do_invalidate_by_alternate_key(AltKeys, ObjectKey, NbDeleted) ->
    case ets:lookup(?OBJECT_TABLE, ObjectKey) of
        [{_, _, _, _, _, _, ObjectAltKeys, _}] ->
            case lists:any(fun(AltKey) -> lists:member(AltKey, ObjectAltKeys) end, AltKeys) of
                true ->
                    delete_object(ObjectKey, alternate_key_invalidation),
                    do_invalidate_by_alternate_key(AltKeys,
                                                   ets:next(?OBJECT_TABLE, ObjectKey),
                                                   NbDeleted + 1);
                false ->
                    do_invalidate_by_alternate_key(AltKeys,
                                                   ets:next(?OBJECT_TABLE, ObjectKey),
                                                   NbDeleted)
            end;
        _ ->
            do_invalidate_by_alternate_key(AltKeys, ets:next(?OBJECT_TABLE, ObjectKey), NbDeleted)
    end.

lru(ObjectKey, When, SeqNumber) ->
    ets:insert(?LRU_TABLE, {{When, ObjectKey, SeqNumber}}).

new_seq_number() ->
    erlang:unique_integer().

object_key(RequestKey, VaryHeaders) ->
    {RequestKey, crypto:hash(sha, erlang:term_to_binary(VaryHeaders))}.

unix_now() ->
    os:system_time(second).
