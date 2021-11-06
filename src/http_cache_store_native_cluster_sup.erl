-module(http_cache_store_native_cluster_sup).

-behaviour(supervisor).

-export([start_link/0, start_worker/1, count_children/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_worker(Cmd) ->
  supervisor:start_child(?MODULE, [Cmd]).

count_children() ->
  proplists:get_value(active, supervisor:count_children(?MODULE)).

init(_) ->
  ChildSpec = #{
                id => http_cache_store_native_cluster_worker,
                start => {http_cache_store_native_cluster_worker, start_link, []},
                restart => temporary,
                shutdown => brutal_kill,
                modules => [http_cache_store_native_cluster_worker]
               },
  {ok, {{simple_one_for_one, 0, 1}, [ChildSpec]}}.
