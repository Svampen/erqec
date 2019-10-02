%%%-------------------------------------------------------------------
%% @doc erqec public API
%% @end
%%%-------------------------------------------------------------------

-module(erqec_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    application:load(grpcbox),
    {ok, #{channel := Channel,
           protocol := Protocol,
           ip := IP,
           port := Port}} = application:get_env(erqec, grpc),
    application:set_env(grpcbox, client,
                        #{channels => [{Channel,
                                        [{Protocol, IP, Port, []}],
                                        #{}}
                                      ]}),
    {ok, _} = application:ensure_all_started(grpcbox),
    erqec_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
