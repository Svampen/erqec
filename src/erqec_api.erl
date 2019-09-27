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
-export([add_rq/1, add_rq/2]).

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
            %% Send to server
            ConnectOptions = #{ip => "localhost", port => 8322,
                               options => [{mode, binary}],
                               timeout => 1000},
            case send_message(RequestMessage, ConnectOptions) of
                {ok, Data} ->
                    case erqec_pb_lib:decode_response_message(Data) of
                        {ok, ResponseMessage} ->
                            parse_response_message(ResponseMessage,
                                                   add_rq_response);
                        nok ->
                            nok
                    end;
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
            nok
    after Timeout ->
            lager:warning("Socket ~p timedout after ~p",
                          [Socket, Timeout]),
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
-spec parse_response_message(ResponseMessage :: rqe_pb:'Response'(),
                             ResponseMessageType :: atom()) ->
                                    term().
parse_response_message(#{msg := {ResponseMessageType, _}}=ResponseMessage,
                       ResponseMessageType) ->
    parse_response_message(ResponseMessage);
parse_response_message({ResponseMessageType1, _},
                       ResponseMessageType2) ->
    lager:warning("Mismatch with received response message type ~p and "
                  "expected response message type ~p",
                  [ResponseMessageType1, ResponseMessageType2]),
    nok.


-spec parse_response_message(rqe_pb:'Response'()) -> term().
parse_response_message(#{msg := {add_rq_response,
                                 #{response_status :=
                                       #{status := 'NOK',
                                         reason := Reason}}}}) ->
    lager:warning("NOK status in add_rq_response with reason:~p", [Reason]),
    {nok, Reason};
parse_response_message(#{msg := {add_rq_response,
                                 #{response_status :=
                                       #{status := 'OK'},
                                   uuid := UUID}}}) ->
    {ok, UUID};
parse_response_message(ResponseMessage) ->
    lager:warning("Unsupported Response Message received for parsing: ~p",
                  [ResponseMessage]),
    nok.
