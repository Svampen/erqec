%%%-------------------------------------------------------------------
%%% @author Stefan Hagdahl <stefan.hagdahl>
%%% @copyright (C) 2019, Stefan Hagdahl
%%% @doc
%%%
%%% @end
%%% Created : 27 Sep 2019 by Stefan Hagdahl <stefan.hagdahl>
%%%-------------------------------------------------------------------
-module(erqec_api).

%% API
-export([add_rq/1, add_rq/2, add_rq/3,
         delete_rq/1, delete_rq/2,
         match_entry/1, match_entry/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Add RQ
%% @end
%%--------------------------------------------------------------------
-spec add_rq(RQ :: [rqe_pb:rq_item()]) ->
                    {ok, uuid:uuid()} | {nok, Error :: term()}.
add_rq(RQ) ->
    add_rq(RQ, []).

-spec add_rq(RQ :: [rqe_pb:rq_item()], Labels :: [rqe_pb:rq_label()]) ->
                    {ok, uuid:uuid()} | {nok, Error :: term()}.
add_rq(RQ, Labels) ->
    add_rq(RQ, Labels, default_channel).

-spec add_rq(RQ :: [rqe_pb:rq_item()],
             Labels :: [rqe_pb:rq_label()],
             Channel :: atom()) ->
                    {ok, uuid:uuid()} | {nok, Error :: term()}.
add_rq(RQ, Labels, Channel) ->
    AddRQRequest = erqec_pb_lib:build_rq_request(RQ, Labels),
    grpc(AddRQRequest, Channel).

%%--------------------------------------------------------------------
%% @doc
%% Delete RQ
%% @end
%%--------------------------------------------------------------------
-spec delete_rq(RQId :: uuid:uuid()) -> ok | {nok, Error :: term()}.
delete_rq(RQId) ->
    delete_rq(RQId, default_channel).

-spec delete_rq(RQId :: uuid:uuid(),
                Channel :: atom()) ->
                       ok | {nok, Error :: term()}.
delete_rq(RQId, Channel) ->
    DeleteRQRequest = ereqc_pb_lib:build_delete_rq_request(RQId),
    grpc(DeleteRQRequest, Channel).

%%--------------------------------------------------------------------
%% @doc
%% Match Entry
%% @end
%%--------------------------------------------------------------------
-spec match_entry(Entry :: #{iodata() := rqe_pb:entry_value()}) ->
                         {ok, [rqe_pb:rq()]} | nok.
match_entry(Entry) when is_map(Entry) ->
    match_entry(Entry, default_channel).

-spec match_entry(Entry :: #{iodata() := rqe_pb:entry_value()},
                  Channel :: atom()) ->
                         {ok, [rqe_pb:rq()]} | nok.
match_entry(Entry, Channel) ->
    MatchEntryRequest = erqec_pb_lib:build_match_entry_request(Entry, 1000),
    grpc(MatchEntryRequest, Channel).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Handle grpc request and the response
%% @end
%%--------------------------------------------------------------------
grpc(Request, Channel) ->
    ChannelOption = #{channel => Channel},
    case rqe_service_client:rqe_message(ctx:new(), Request, ChannelOption) of
        {ok, ResponseMessage, _Headers} ->
             parse_response_message(ResponseMessage);
        {error, Error} ->
            lager:error("Received error in grpc:~p",
                        [Error]),
            {nok, error};
        {grpc_error, {ErrorCode, ErrorMessage}} ->
            lager:error("Recieved grpc error ~p:~p",
                        [ErrorCode, ErrorMessage]),
            {nok, grpc_error}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Parse Response Message
%% @end
%%--------------------------------------------------------------------
-spec parse_response_message(ResponseMessage :: rqe_pb:resposne()) -> term().
parse_response_message(#{msg := {add_rq_response, Response}}) ->
    parse_add_rq_response(Response);

parse_response_message(#{msg := {match_entry_response, Response}}) ->
    parse_match_entry_response(Response);

parse_response_message(#{msg := {delete_req_response, Response}}) ->
    parse_delete_rq_response(Response);

parse_response_message(ResponseMessage) ->
    lager:warning("Unsupported Response Message received "
                  "for parsing: ~p",
                  [ResponseMessage]),
    {nok, unsupported_response}.

%%--------------------------------------------------------------------
%% @doc
%% Parse add rq response
%% @end
%%--------------------------------------------------------------------
-spec parse_add_rq_response(#{uuid => uuid:uuid()}) -> {ok, uuid:uuid()}.
parse_add_rq_response(#{uuid := UUID}) ->
    {ok, UUID}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

-spec parse_match_entry_response(#{rqs => RQs :: [rqe_pb:rq()]}) ->
                                        {ok, [rqe_pb:rq()]}.
parse_match_entry_response(#{rqs := RQs}) ->
    {ok, RQs}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-spec parse_delete_rq_response(#{}) -> ok.
parse_delete_rq_response(_) ->
    ok.
