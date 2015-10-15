%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Nikita Roshchupkin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%% Created : Jul 2015
%%%-------------------------------------------------------------------
-module(testing_app).
-behaviour(application).
-export([start/2, stop/1]).

-compile([{parse_transform, lager_transform}]).

start(_Type, StartArgs) ->
    testing_supervisor:start_link(StartArgs).

stop(_State) ->
    ok.