%% @private
-module(http_cache_store_memory).

-behaviour(http_cache_store_behaviour).

-include("http_cache_store_memory.hrl").

-export([list_candidates/2, get_response/2, put/6, notify_response_used/2,
         invalidate_url/2, invalidate_by_alternate_key/2, delete_object/2, object_key/3, lru/2]).

list_candidates(RequestKey, _Opts) ->
    Spec =
        [{{{RequestKey, '$1', '$2'}, '$3', '_', {'$4', '$5', '_'}, '$6', '_'},
          [],
          [['$1', '$2', '$3', '$4', '$5', '$6']]}],
    Now = unix_now(),
    [{{RequestKey, SecondKeyPart, ThirdKeyPart},
      Status,
      RespHeaders,
      VaryHeaders,
      RespMetadata}
     || [SecondKeyPart, ThirdKeyPart, VaryHeaders, Status, RespHeaders, RespMetadata]
            <- ets:select(?OBJECT_TABLE, Spec),
        Now < map_get(grace, RespMetadata)].

get_response(ObjectKey, _Opts) ->
    case ets:lookup(?OBJECT_TABLE, ObjectKey) of
        [{_ObjectKey,
          _VaryHeaders,
          _UrlDigest,
          {Status, RespHeaders, RespBody},
          RespMetadata,
          _SeqNumber}] ->
            {Status, RespHeaders, RespBody, RespMetadata};
        [] ->
            undefined
    end.

put(RequestKey, UrlDigest, VaryHeaders, Response, #{grace := _} = RespMetadata, _Opts) ->
    case http_cache_store_memory_worker_sup:start_worker({cache_object,
                                                          {RequestKey,
                                                           UrlDigest,
                                                           VaryHeaders,
                                                           Response,
                                                           RespMetadata}})
    of
        ok ->
            ObjectKey = object_key(RequestKey, VaryHeaders, RespMetadata),
            Expires = map_get(grace, RespMetadata),
            http_cache_store_memory_cluster_mon:broadcast_object_available(ObjectKey, Expires),
            ok;
        {error, _} = Error ->
            Error
    end.

invalidate_url(UrlDigest, _Opts) ->
    http_cache_store_memory_cluster_mon:broadcast_invalidate_url(UrlDigest),
    case http_cache_store_memory_worker_sup:start_worker({invalidate_url, UrlDigest}) of
        ok ->
            {ok, undefined};
        {error, _} = Error ->
            Error
    end.

invalidate_by_alternate_key(AltKeys, _Opts) ->
    http_cache_store_memory_cluster_mon:broadcast_invalidate_by_alternate_key(AltKeys),
    case http_cache_store_memory_worker_sup:start_worker({invalidate_by_alternate_key,
                                                          AltKeys})
    of
        ok ->
            {ok, undefined};
        {error, _} = Error ->
            Error
    end.

notify_response_used(ObjectKey, _Opts) ->
    SeqNumber = erlang:unique_integer(),
    case ets:update_element(?OBJECT_TABLE, ObjectKey, {6, SeqNumber}) of
        true ->
            lru(ObjectKey, SeqNumber),
            ok;
        false ->
            ok
    end.

delete_object(ObjectKey, Reason) ->
    telemetry:execute([http_cache_store_memory, object_deleted], #{}, #{reason => Reason}),
    ets:delete(?OBJECT_TABLE, ObjectKey),
    ok.

object_key(RequestKey, VaryHeaders, RespMetadata) ->
    {RequestKey,
     crypto:hash(sha256, erlang:term_to_binary(VaryHeaders)),
     chunk_id(RespMetadata)}.

chunk_id(#{parsed_headers := #{<<"content-range">> := {Unit, Start, End, Len}}}) ->
    UnitBin =
        if is_atom(Unit) ->
               atom_to_binary(Unit);
           true ->
               Unit
        end,
    StartBin = integer_to_binary(Start),
    EndBin = integer_to_binary(End),
    LenBin =
        case Len of
            '*' ->
                <<"*">>;
            _ ->
                integer_to_binary(Len)
        end,
    <<UnitBin/binary, " ", StartBin/binary, "-", EndBin/binary, "/", LenBin/binary>>;
chunk_id(_RespMetadata) ->
    <<>>.

lru(ObjectKey, SeqNumber) ->
    ets:insert(?LRU_TABLE, {{unix_now(), ObjectKey, SeqNumber}}).

unix_now() ->
    os:system_time(second).
