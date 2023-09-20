-module(base_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([cache_file/1, lru_deletion_memory_limit/1, invalidate_by_url/1,
         invalidate_by_alternate_key/1, cache_chunks/1]).

-define(LIMIT_CHECK_INTERVAL, 100).
-define(TEST_REQUEST_KEY, <<"some request key">>).
-define(TEST_URL_DIGEST, <<"request URI digest">>).
-define(TEST_VARY_HEADERS, #{}).
-define(TEST_STATUS, 200).
-define(TEST_RESP_HEADERS, [{<<"content-length">>, <<"21">>}]).
-define(TEST_RESP_BODY, <<"Some response content">>).
-define(TEST_RESPONSE, {?TEST_STATUS, ?TEST_RESP_HEADERS, ?TEST_RESP_BODY}).
-define(TEST_RESP_METADATA,
        #{alternate_keys => [some, alternate, keys], grace => erlang:system_time(second) + 120}).
-define(TEST_OPTS, []).

all() ->
    [cache_file,
     lru_deletion_memory_limit,
     invalidate_by_url,
     invalidate_by_alternate_key,
     cache_chunks].

init_per_testcase(_TestName, Config) ->
    application:set_env(http_cache_store_memory, limit_check_interval, ?LIMIT_CHECK_INTERVAL),
    application:set_env(http_cache_store_memory, memory_limit, 1.0),
    {ok, _} = application:ensure_all_started(http_cache_store_memory),
    Config.

end_per_testcase(_, _Config) ->
    ok = application:stop(http_cache_store_memory).

cache_file(_Config) ->
    http_cache_store_memory:put(?TEST_REQUEST_KEY,
                                ?TEST_URL_DIGEST,
                                ?TEST_VARY_HEADERS,
                                ?TEST_RESPONSE,
                                ?TEST_RESP_METADATA,
                                ?TEST_OPTS),
    [{ObjectKey, _, _, _, _}] =
        http_cache_store_memory:list_candidates(?TEST_REQUEST_KEY, ?TEST_OPTS),
    {?TEST_STATUS, ?TEST_RESP_HEADERS, ?TEST_RESP_BODY, _} =
        http_cache_store_memory:get_response(ObjectKey, ?TEST_OPTS).

lru_deletion_memory_limit(_Config) ->
    TelemetryRef =
        telemetry_test:attach_event_handlers(self(), [[http_cache_store_memory, object_deleted]]),
    http_cache_store_memory:put(?TEST_REQUEST_KEY,
                                ?TEST_URL_DIGEST,
                                ?TEST_VARY_HEADERS,
                                ?TEST_RESPONSE,
                                ?TEST_RESP_METADATA,
                                ?TEST_OPTS),
    application:set_env(http_cache_store_memory, memory_limit, 0),
    timer:sleep(?LIMIT_CHECK_INTERVAL * 2),
    [] = http_cache_store_memory:list_candidates(?TEST_REQUEST_KEY, ?TEST_OPTS),
    receive
        {[http_cache_store_memory, object_deleted], TelemetryRef, #{}, #{reason := lru_nuked}} ->
            telemetry:detach(TelemetryRef)
    after 1000 ->
        ct:fail(timeout_receive_telemetry_event)
    end.

invalidate_by_url(_Config) ->
    TelemetryRef =
        telemetry_test:attach_event_handlers(self(), [[http_cache_store_memory, object_deleted]]),
    http_cache_store_memory:put(?TEST_REQUEST_KEY,
                                ?TEST_URL_DIGEST,
                                ?TEST_VARY_HEADERS,
                                ?TEST_RESPONSE,
                                ?TEST_RESP_METADATA,
                                ?TEST_OPTS),
    http_cache_store_memory:invalidate_url(?TEST_URL_DIGEST, ?TEST_OPTS),
    timer:sleep(1000),
    [] = http_cache_store_memory:list_candidates(?TEST_REQUEST_KEY, ?TEST_OPTS),
    receive
        {[http_cache_store_memory, object_deleted],
         TelemetryRef,
         #{},
         #{reason := url_invalidation}} ->
            telemetry:detach(TelemetryRef)
    after 1000 ->
        ct:fail(timeout_receive_telemetry_event)
    end.

invalidate_by_alternate_key(_Config) ->
    TelemetryRef =
        telemetry_test:attach_event_handlers(self(), [[http_cache_store_memory, object_deleted]]),
    http_cache_store_memory:put(?TEST_REQUEST_KEY,
                                ?TEST_URL_DIGEST,
                                ?TEST_VARY_HEADERS,
                                ?TEST_RESPONSE,
                                ?TEST_RESP_METADATA,
                                ?TEST_OPTS),
    http_cache_store_memory:invalidate_by_alternate_key([alternate], ?TEST_OPTS),
    timer:sleep(1000),
    [] = http_cache_store_memory:list_candidates(?TEST_REQUEST_KEY, ?TEST_OPTS),
    receive
        {[http_cache_store_memory, object_deleted],
         TelemetryRef,
         #{},
         #{reason := alternate_key_invalidation}} ->
            telemetry:detach(TelemetryRef)
    after 1000 ->
        ct:fail(timeout_receive_telemetry_event)
    end.

cache_chunks(_Config) ->
    ChunkMetadata1 =
        #{grace => erlang:system_time(second) + 120,
          parsed_headers => #{<<"content-range">> => {bytes, 0, 5, 11}}},
    ChunkMetadata2 =
        #{grace => erlang:system_time(second) + 120,
          parsed_headers => #{<<"content-range">> => {bytes, 6, 10, 11}}},
    http_cache_store_memory:put(?TEST_REQUEST_KEY,
                                ?TEST_URL_DIGEST,
                                ?TEST_VARY_HEADERS,
                                {206, [], <<"Hello">>},
                                ChunkMetadata1,
                                ?TEST_OPTS),
    http_cache_store_memory:put(?TEST_REQUEST_KEY,
                                ?TEST_URL_DIGEST,
                                ?TEST_VARY_HEADERS,
                                {206, [], <<"World">>},
                                ChunkMetadata2,
                                ?TEST_OPTS),
    timer:sleep(100),
    [_, _] = http_cache_store_memory:list_candidates(?TEST_REQUEST_KEY, ?TEST_OPTS).
