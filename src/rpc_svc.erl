%%
%% rpc_svc was generated by erpcgen (do not edit)
%% date: Jun 8 10:43:20 2015
%%
-module(rpc_svc).
-include("rpc.hrl").
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2]).

init(_) ->
    {ok,[]}.

handle_call(_, _, _) ->
    {reply,ok,[]}.

handle_cast(_, _) ->
    {noreply,[]}.

handle_info({tcp_new,Sock}, _) ->
    erlang:send(rpc_server, {tcp_new,Sock}),
    {noreply,[]};
handle_info({tcp_closed,Sock}, _) ->
    erlang:send(rpc_server, {tcp_closed,Sock}),
    {noreply,[]};
handle_info(_, _) ->
    {noreply,[]}.

terminate(_, _) ->
    [].