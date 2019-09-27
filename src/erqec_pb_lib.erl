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
-export([encode_request_message/1,
         decode_response_message/1,
         build_rq_request/2,
         build_match_entry_request/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Encode Request msg
%% @end
%%--------------------------------------------------------------------
-spec encode_request_message(Request :: rqe_pb:'Request'()) ->
                                    {ok, binary()} | nok.
encode_request_message(Request) ->
    try rqe_pb:encode_msg(Request, 'Request') of
        Data when is_binary(Data) ->
            {ok, Data};
        Data ->
            lager:warning("Encoded proto message isn't binary...~n~p", [Data]),
            nok
    catch
        TypeOfError:Exception ->
            lager:error("Exception ~p:~p when protobuf encoding request msg:~p",
                        [TypeOfError, Exception, Request]),
            nok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Decode response message
%% @end
%%--------------------------------------------------------------------
-spec decode_response_message(
        Response :: binary()) -> {ok, rqe_pb:'Response'()} | nok.
decode_response_message(Response) ->
    try rqe_pb:decode_msg(Response, 'Response') of
        #{msg := _}=ResponseMessage ->
            {ok, ResponseMessage};
        Unknown ->
            lager:error("Unsupported decoded msg ~p",
                        [Unknown]),
            nok
    catch
        TypeOfError:Exception ->
            lager:error("Exception ~p:~p when protobuf decoding "
                        "response msg:~p",
                        [TypeOfError, Exception, Response]),
            nok
    end.

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
