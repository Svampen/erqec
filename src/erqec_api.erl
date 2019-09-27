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
-export([add_rq/1, add_rq/2,
         match_entry/1]).

-type connectoptions() :: #{ip => iodata(), port => integer(),
                            options => ssl:tls_option(), timeout => integer()}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Add RQ
%% @end
%%--------------------------------------------------------------------
-spec add_rq(RQ :: [rqe_pb:'RQItem'()]) -> {ok, uuid:uuid()} | nok.
add_rq(RQ) ->
    add_rq(RQ, []).

-spec add_rq(RQ :: [rqe_pb:'RQItem'()], Labels :: [rqe_pb:'RQLabel'()]) ->
                    {ok, uuid:uuid()} | nok.
add_rq(RQ, Labels) ->
    AddRQRequest = erqec_pb_lib:build_rq_request(RQ, Labels),
    case erqec_pb_lib:encode_request_message(AddRQRequest) of
        {ok, RequestMessage} ->
            ConnectOptions = #{ip => "localhost", port => 8322,
                               options => [{mode, binary}],
                               timeout => 2000},
            send_receive_message(RequestMessage, add_rq_response,
                                 ConnectOptions);
        nok ->
            nok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Match Entry
%% @end
%%--------------------------------------------------------------------
-spec match_entry(Entry ::
                    #{iodata() := rqe_pb:'MatchEntryRequest.EntryValue'()}) ->
                         {ok, [rqe_pb:'RQ'()]} | nok.
match_entry(Entry) when is_map(Entry) ->
    MatchEntryRequest = erqec_pb_lib:build_match_entry_request(Entry, 1000),
    case erqec_pb_lib:encode_request_message(MatchEntryRequest) of
        {ok, RequestMessage} ->
            ConnectOptions = #{ip => "localhost", port => 8322,
                               options => [{mode, binary}],
                               timeout => 2000},
            send_receive_message(RequestMessage, match_entry_response,
                                 ConnectOptions);
        nok ->
            nok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Send and receive message
%% @end
%%--------------------------------------------------------------------
-spec send_receive_message(RequestMessage :: binary(),
                           ResponseMessageType :: atom(),
                           ConnectOptions :: connectoptions()) ->
                                  nok | term().
send_receive_message(RequestMessage, ResponseMessageType, ConnectOptions) ->
    case send_message(RequestMessage, ConnectOptions) of
        {ok, Data} ->
            case erqec_pb_lib:decode_response_message(Data) of
                {ok, #{msg := {ResponseMessageType, _}}=Response} ->
                    parse_response_message(Response);
                {ok, ResponseMessage} ->
                    lager:warning("Unsupported response message "
                                  "received: ~p",
                                  [ResponseMessage]),
                    nok;
                nok ->
                    nok
            end;
        nok ->
            nok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Send Message
%% @end
%%--------------------------------------------------------------------
-spec send_message(Message :: binary(), ConnectOptions :: connectoptions()) ->
                          {ok, binary() | list()} | nok.
send_message(Message, #{timeout := Timeout}=ConnectOptions)
  when is_binary(Message), is_map(ConnectOptions) ->
    case connect(ConnectOptions) of
        {ok, Socket} ->
            send(Socket, Message, Timeout);
        nok ->
            nok
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Send Data through Socket
%% @end
%%--------------------------------------------------------------------
-spec send(Socket :: ssl:sslsocket(), Data :: binary(), Timeout :: integer()) ->
                  {ok, binary() | list()} | nok.
send(Socket, Data, Timeout) ->
    case ssl:send(Socket, Data) of
        ok ->
            receive_loop(Socket, Timeout);
        {error, Reason} ->
            lager:error("Connection error when sending ~p with reason:~p",
                        [Data, Reason]),
            nok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Receive data from Socket until Timeout
%% @end
%%--------------------------------------------------------------------
-spec receive_loop(Socket :: ssl:sslsocket(), Timeout :: integer()) ->
                          {ok, binary() | list()} | nok.
receive_loop(Socket, Timeout) ->
    receive
        {ssl, Socket, Data} ->
            ssl:close(Socket),
            {ok, Data};
        {ssl_close, Socket} ->
            lager:warning("Socket ~p closed unexpectedly",
                          [Socket]),
            nok;
        {ssl_error, Socket, Reason} ->
            lager:error("Socket ~p closed with error:~p",
                        [Socket, Reason]),
            nok;
        Unknown ->
            lager:error("Received unknown message:~p", [Unknown]),
            ssl:close(Socket),
            nok
    after Timeout ->
            lager:warning("Socket ~p timedout after ~p",
                          [Socket, Timeout]),
            ssl:close(Socket),
            nok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Connect to RQE
%% @end
%%--------------------------------------------------------------------
-spec connect(ConnectOptions :: connectoptions()) ->
                     {ok, ssl:sslsocket()} | nok.
connect(#{ip := IP, port := Port,
          options := Options, timeout := Timeout}) ->
    case ssl:connect(IP, Port, Options, Timeout) of
        {ok, Socket} ->
            {ok, Socket};
        {ok, Socket, _Ext} ->
            {ok, Socket};
        {error, Reason} ->
            lager:error("Failed to connect to ~p:~p (~p) with reason:~p",
                        [IP, Port, Options, Reason]),
            nok;
        {option_not_a_key_value_tuple, _} ->
            nok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Parse Response Message
%% @end
%%--------------------------------------------------------------------
-spec parse_response_message(ResponseMessage :: rqe_pb:'Response'()) ->
                                    term().
parse_response_message(
  #{msg := {_, #{response_status := #{status := 'NOK',
                                      reason := Reason}}}}=ResponseMessage) ->
    lager:warning("NOK status in ~p with reason:~p", [ResponseMessage,
                                                      Reason]),
    nok;
parse_response_message(
  #{msg := {ResponseMessageType,
            #{response_status :=
                  #{status := 'OK'}}=Response}}=ResponseMessage) ->
    case ResponseMessageType of
        add_rq_response ->
            parse_add_rq_response(Response);
        match_entry_response ->
            parse_match_entry_response(Response);
        ResponseMessageType ->
            lager:warning("Unsupported Response Message received "
                          "for parsing: ~p",
                          [ResponseMessage]),
            nok
    end.

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

-spec parse_match_entry_response(#{rqs => RQs :: [rqe_pb:'RQ'()]}) ->
                                        {ok, [rqe_pb:'RQ'()]}.
parse_match_entry_response(#{rqs := RQs}) ->
    {ok, RQs}.
