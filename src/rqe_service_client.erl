%%%-------------------------------------------------------------------
%% @doc Client module for grpc service rqeService.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2019-10-01T13:30:53+00:00 and should not be modified manually

-module(rqe_service_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox/include/grpcbox.hrl").

-define(is_ctx(Ctx), is_tuple(Ctx) andalso element(1, Ctx) =:= ctx).

-define(SERVICE, 'rqeService').
-define(PROTO_MODULE, 'rqe_pb').
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output, MessageType), #grpcbox_def{service=?SERVICE,
                                                      message_type=MessageType,
                                                      marshal_fun=?MARSHAL_FUN(Input),
                                                      unmarshal_fun=?UNMARSHAL_FUN(Output)}).

%% @doc Unary RPC
-spec rqe_message(rqe_pb:request()) ->
    {ok, rqe_pb:response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
rqe_message(Input) ->
    rqe_message(ctx:new(), Input, #{}).

-spec rqe_message(ctx:t() | rqe_pb:request(), rqe_pb:request() | grpcbox_client:options()) ->
    {ok, rqe_pb:response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
rqe_message(Ctx, Input) when ?is_ctx(Ctx) ->
    rqe_message(Ctx, Input, #{});
rqe_message(Input, Options) ->
    rqe_message(ctx:new(), Input, Options).

-spec rqe_message(ctx:t(), rqe_pb:request(), grpcbox_client:options()) ->
    {ok, rqe_pb:response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
rqe_message(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/rqeService/rqeMessage">>, Input, ?DEF(request, response, <<"Request">>), Options).

