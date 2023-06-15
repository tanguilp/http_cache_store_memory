%% @private
-module(http_cache_store_memory_worker_sup).

-behaviour(supervisor).

-export([start_link/0, start_worker/1, count_children/0]).
-export([init/1]).

-define(MAX_CONCURRENCY, 32).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_worker({Type, _} = Cmd)
    when Type == cache_object;
         Type == remote_object_available;
         Type == remote_object_request;
         Type == remote_object_response ->
    case http_cache_store_memory_stats:is_limit_reached() of
        false ->
            do_start_worker(Cmd);
        true ->
            {error, storage_limit_reached}
    end;
start_worker(Cmd) ->
    do_start_worker(Cmd).

do_start_worker(Cmd) ->
    case is_priority_command(Cmd) of
        true ->
            supervisor:start_child(?MODULE, [Cmd]),
            ok;
        false ->
            case count_children() < max_concurrency() of
                true ->
                    supervisor:start_child(?MODULE, [Cmd]),
                    ok;
                false ->
                    {error, concurrency_limit_reached}
            end
    end.

count_children() ->
    proplists:get_value(active, supervisor:count_children(?MODULE)).

init(_) ->
    ChildSpec =
        #{id => http_cache_store_memory_worker,
          start => {http_cache_store_memory_worker, start_link, []},
          restart => temporary,
          shutdown => brutal_kill,
          modules => [http_cache_store_memory_worker]},
    {ok, {{simple_one_for_one, 0, 1}, [ChildSpec]}}.

is_priority_command({invalidate_url, _}) ->
    true;
is_priority_command({invalidate_by_alternate_key, _}) ->
    true;
is_priority_command({warm_me_up, _}) ->
    true;
is_priority_command(_) ->
    false.

max_concurrency() ->
    application:get_env(http_cache_store_memory, max_concurrency, ?MAX_CONCURRENCY).
