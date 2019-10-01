%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service rqeService.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2019-10-01T13:30:53+00:00 and should not be modified manually

-module(rqe_service_bhvr).

%% @doc Unary RPC
-callback rqe_message(ctx:ctx(), rqe_pb:request()) ->
    {ok, rqe_pb:response(), ctx:ctx()} | grpcbox_stream:grpc_error_response().

