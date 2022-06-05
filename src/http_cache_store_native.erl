%% @private
-module(http_cache_store_native).

-include("http_cache_store_native.hrl").

-behaviour(http_cache_store).

-export([list_candidates/1, get_response/1, put/5, notify_response_used/2, invalidate_url/1, invalidate_by_alternate_key/1, delete_object/2, object_key/2, lru/2]).

list_candidates(RequestKey) ->
    Spec =
        [{{{RequestKey, '$1'}, '$2', '_', {'$3', '$4', '_'}, '$5', '$6', '_'},
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
          _SeqNumber}] ->
            {Status, RespHeaders, RespBody, RespMetadata};
        [] ->
            undefined
    end.

put(RequestKey,
    UrlDigest,
    VaryHeaders,
    Response,
    RespMetadata
    ) ->
  case http_cache_store_native_worker_sup:start_worker({cache_object, {RequestKey, UrlDigest, VaryHeaders, Response, RespMetadata}}) of
    ok ->
      ObjectKey = object_key(RequestKey, VaryHeaders),
      Expires = map_get(grace, RespMetadata),
      http_cache_store_native_cluster_mon:broadcast_object_available(ObjectKey, Expires),
      ok;

    {error, _} = Error ->
      Error
  end.

invalidate_url(UrlDigest) ->
    http_cache_store_native_cluster_mon:broadcast_invalidate_url(UrlDigest),
    case http_cache_store_native_worker_sup:start_worker({invalidate_url, UrlDigest}) of
      ok ->
        {ok, undefined};

      {error, _} = Error ->
        Error
    end.

invalidate_by_alternate_key(AltKeys) ->
    http_cache_store_native_cluster_mon:broadcast_invalidate_by_alternate_key(AltKeys),
    case http_cache_store_native_worker_sup:start_worker({invalidate_by_alternate_key, AltKeys}) of
      ok ->
        {ok, undefined};

      {error, _} = Error ->
        Error
    end.

notify_response_used(ObjectKey, _When) ->
    SeqNumber = erlang:unique_integer(),
    case ets:update_element(?OBJECT_TABLE, ObjectKey, {7, SeqNumber}) of
        true ->
            lru(ObjectKey, SeqNumber),
            ok;
        false ->
            ok
    end.

delete_object(ObjectKey, Reason) ->
    telemetry:execute([http_cache_store_native, object_deleted], #{}, #{reason => Reason}),
    ets:delete(?OBJECT_TABLE, ObjectKey),
    ok.

object_key(RequestKey, VaryHeaders) ->
  {RequestKey, crypto:hash(sha256, erlang:term_to_binary(VaryHeaders))}.

lru(ObjectKey, SeqNumber) ->
    ets:insert(?LRU_TABLE, {{unix_now(), ObjectKey, SeqNumber}}).

unix_now() ->
    os:system_time(second).
