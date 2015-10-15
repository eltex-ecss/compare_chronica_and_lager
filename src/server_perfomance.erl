%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Nikita Roshchupkin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%% Created : Jul 2015
%%%-------------------------------------------------------------------

-module(server_perfomance).
-behaviour(gen_server).

%%gen_server callback
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3, start_link/0]).

-compile([{parse_transform, lager_transform}]).

-record(init_cmd, {}). %% {init_cmd}

%%gen_server callback
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    io:format("~p starting server_perfomance \n",[?MODULE]),
    self() ! #init_cmd{},
    {ok, 0}.

handle_call(Name_test, _From, N) ->
  io:format("header call of the test: ~p ~n", [Name_test]),
  {reply, ok, N+1}.

handle_cast(_Msg, N)  -> {noreply, N}.

handle_info(#init_cmd{}, N)  ->
    execution_tests:benchmark_test(),
    {noreply, N};

handle_info(_Info, N)  -> {noreply, N}.

terminate(_Reason, _N) ->
    io:format("stopping server_perfomance ~n"),
    ok.

code_change(_OldVsn, N, _Extra) -> {ok, N}.