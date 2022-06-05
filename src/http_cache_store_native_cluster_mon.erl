-module(http_cache_store_native_cluster_mon).

-behaviour(gen_server).

-define(WARMUP_NB_OBJECTS, 5000).

-export([broadcast_invalidate_url/1, broadcast_invalidate_by_alternate_key/1,
         broadcast_object_available/2, request_cached_object/2, send_cached_object/2]).
-export([start_link/0, init/1, handle_call/3, handle_continue/2, handle_cast/2,
         handle_info/2]).

broadcast_invalidate_url(UrlDigest) ->
    gen_server:abcast(nodes(), ?MODULE, {invalidate_url, UrlDigest}).

broadcast_invalidate_by_alternate_key(AltKeys) ->
    gen_server:abcast(nodes(), ?MODULE, {invalidate_by_alternate_key, AltKeys}).

broadcast_object_available(ObjectKey, Expires) ->
    gen_server:abcast(nodes(), ?MODULE, {remote_object_available, {node(), {ObjectKey, Expires}}}).

request_cached_object(Node, ObjectKey) ->
    gen_server:cast({?MODULE, Node}, {remote_object_request, {node(), ObjectKey}}).

send_cached_object(Node, Object) ->
    gen_server:cast({?MODULE, Node}, {remote_object_response, Object}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, [], {continue, []}}.

handle_continue(_, State) ->
    NbObjects =
        application:get_env(http_cache_store_native, warmup_nb_objects, ?WARMUP_NB_OBJECTS),
    gen_server:abcast(nodes(), ?MODULE, {warm_me_up, {node(), NbObjects}}),
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({_, _} = Cmd, State) ->
  http_cache_store_native_worker_sup:start_worker(Cmd),
  {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

