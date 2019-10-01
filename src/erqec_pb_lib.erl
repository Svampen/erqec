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
         build_match_entry_request/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Build RQ request
%% @end
%%--------------------------------------------------------------------
-spec build_rq_request(RQ :: [rqe_pb:'RQItem'()],
                       Labels :: [rqe_pb:'RQLabel'()]) ->
                              rqe_pb:'Request'().
build_rq_request(RQ, Labels) ->
    #{msg => {add_rq_request,
              #{rq => #{rq_items => RQ},
                labels => #{labels => Labels}}}}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-spec build_match_entry_request(
        Entry :: #{iodata() := rqe_pb:'MatchEntryRequest.EntryValue'()},
        Timeout :: integer()) ->
                                       rqe_pb:'Request'().
build_match_entry_request(Entry, Timeout) ->
    #{msg => {match_entry_request,
              #{entry => Entry,
                timeout => Timeout}}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
