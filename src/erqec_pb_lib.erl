%%%-------------------------------------------------------------------
%%% @author Stefan Hagdahl <stefan.hagdahl>
%%% @copyright (C) 2019, Stefan Hagdahl
%%% @doc
%%%
%%% @end
%%% Created : 27 Sep 2019 by Stefan Hagdahl <stefan.hagdahl>
%%%-------------------------------------------------------------------
-module(erqec_pb_lib).

%% API
-export([build_rq_request/2,
         build_delete_rq_request/1,
         build_match_entry_request/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Build Add RQ request
%% @end
%%--------------------------------------------------------------------
-spec build_rq_request(RQ :: [rqe_pb:rq_Item()],
                       Labels :: [rqe_pb:rq_Label()]) ->
                              rqe_pb:request().
build_rq_request(RQ, Labels) ->
    #{msg => {add_rq_request,
              #{rq => #{rq_items => RQ},
                labels => #{labels => Labels}}}}.

%%--------------------------------------------------------------------
%% @doc
%% Build Delete RQ Request
%% @end
%%--------------------------------------------------------------------
-spec build_delete_rq_request(RQId :: uuid:uuid()) -> rqe_pb:request().
build_delete_rq_request(RQId) ->
    #{msg => {delete_rq_request,
              #{uuid => RQId}}}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-spec build_match_entry_request(
        Entry :: #{iodata() := rqe_pb:entry_value()},
        Timeout :: integer()) -> rqe_pb:request().
build_match_entry_request(Entry, Timeout) ->
    #{msg => {match_entry_request,
              #{entry => Entry,
                timeout => Timeout}}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
