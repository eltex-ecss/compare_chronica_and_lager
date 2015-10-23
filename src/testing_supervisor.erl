%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Nikita Roshchupkin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%% Created : Jul 2015
%%%-------------------------------------------------------------------
-module(testing_supervisor).
-behaviour(supervisor).

-export([start/0, start_link/1, init/1]).

start() ->
    spawn(fun() ->
          supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = [])
      end).

start_link(Args) ->
    supervisor:start_link({local,?MODULE}, ?MODULE, Args).

init([]) ->
    {ok, {{one_for_all, 3, 10},
        [{tag1,
            {testing_lager_logs, start_link, []},
            temporary,
            10000,
            worker,
            [testing_lager_logs]
        },

        {tag2,
            {testing_chronica_logs, start_link, []},
            temporary,
            10000,
            worker,
            [testing_chronica_logs]
        },

        {tag3,
            {server_perfomance, start_link, []},
            temporary,
            10000,
            worker,
            [server_perfomance]
        }]
    }}.