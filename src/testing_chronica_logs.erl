%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Nikita Roshchupkin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%% Created : Jul 2015
%%%-------------------------------------------------------------------
-module(testing_chronica_logs).
-behaviour(gen_server).

%%test
-export([start_test_ONE_thread_ONE_log_file/0, start_test_MANY_threads_ONE_log_file/0,
    start_test_ONE_thread_MANY_log_files/0, start_test_MANY_threads_MANY_log_files/0]).

-export([testing_short_warning_file/1]).

-export([testing_long_warning_file/2]).

-export([testing_formater_warning_file/1]).

%%gen_server callback
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3, start_link/0, clear/0]).

-include_lib("chronica/include/chronica.hrl").
-include_lib("pt_scripts/include/pt_recompilable.hrl").

-record(config_state,
    {
        number = 0,
        buffertime_r1 = [],
        buffertime_r2 = [],
        buffertime_r3 = [],
        maxtime_r1 = 0,
        maxtime_r2 = 0,
        maxtime_r3 = 0
    }).

clear() ->
    gen_server:call(?MODULE, clear, infinity).

minmax(Arg1, Arg2) ->
    case Arg1 < Arg2 of
        true ->
            Arg2;
        false ->
            Arg1
    end.


%%LOG_FILES
%%%#########################################################################%%%
testing_short_warning_file(0) -> ok;
testing_short_warning_file(Number_test) ->
    log:warning([warning_tag1], "test chronica: ~p ~n", [Number_test]),
    testing_short_warning_file(Number_test - 1).

testing_long_warning_file(0, _) -> ok;
testing_long_warning_file(Number_test, List) ->
    log:warning([warning_tag1], "test chronica: ~p ~n", [List]),
    testing_long_warning_file(Number_test - 1, List).

testing_formater_warning_file(0) -> ok;
testing_formater_warning_file(Number_test) ->
    log:warning([warning_tag1], "test chronica: ~p ~n ~n ~n ~n ~n ~n ~n ~n ~n ~n", [Number_test]),
    testing_formater_warning_file(Number_test - 1).


%%%#########################################################################%%%

%% однопоточное логгирование (логгирование из одного потока)
%%%#########################################################################%%%
%%Для одного файла, один поток, короткие сообщения, одна маска,
start_test_ONE_thread_ONE_log_file() ->
    %?MODULE:print_current_source(),
    {R1, R2, R3, R4, R5, R6} = gen_server:call(?MODULE, one_thread_one_log, infinity),

    file:write_file("Benchmark", io_lib:fwrite("testing_short_message_file Reductions for one thread and many threads ~p\n", [R2-R1]), [append]),
    file:write_file("Benchmark", "-------------------------------------------------------------------------------------\n", [append]),

    file:write_file("Benchmark", io_lib:fwrite("testing_long_message_file Reductions for one thread and many threads ~p\n", [R4-R3]), [append]),
    file:write_file("Benchmark", "-------------------------------------------------------------------------------------\n", [append]),

    file:write_file("Benchmark", io_lib:fwrite("testing_formater_message_file Reductions for one thread and many threads ~p\n", [R6-R5]), [append]).

reductions_test_ONE_thread_ONE_log_file() ->
    erlang:garbage_collect(self()),
    [{_, R1}] = erlang:process_info(self(), [reductions]),
    testing_short_warning_file(1000),
    [{_, R2}] = erlang:process_info(self(), [reductions]),

    [{_, R3}] = erlang:process_info(self(), [reductions]),
    testing_long_warning_file(1000, lists:seq(1, 1000)),
    [{_, R4}] = erlang:process_info(self(), [reductions]),

    [{_, R5}] = erlang:process_info(self(), [reductions]),
    testing_formater_warning_file(1000),
    [{_, R6}] = erlang:process_info(self(), [reductions]),

    {R1, R2, R3, R4, R5, R6}.


%%Для разных файлов, один поток, короткие сообщения, одна маска
start_test_ONE_thread_MANY_log_files() ->
    {R1, R2, R3, R4, R5, R6} = gen_server:call(?MODULE, one_thread_many_log, infinity),

    file:write_file("Benchmark", io_lib:fwrite("testing_short_message_file Reductions for one thread and many threads ~p\n", [R2-R1]), [append]),
    file:write_file("Benchmark", "-------------------------------------------------------------------------------------\n", [append]),

    file:write_file("Benchmark", io_lib:fwrite("testing_long_message_file Reductions for one thread and many threads ~p\n", [R4-R3]), [append]),
    file:write_file("Benchmark", "-------------------------------------------------------------------------------------\n", [append]),

    file:write_file("Benchmark", io_lib:fwrite("testing_formater_message_file Reductions for one thread and many threads ~p\n", [R6-R5]), [append]).

reductions_test_ONE_thread_MANY_log_files() ->
    erlang:garbage_collect(self()),
    [{_, R1}] = erlang:process_info(self(), [reductions]),
    testing_short_warning_file(250),
    [{_, R2}] = erlang:process_info(self(), [reductions]),

    [{_, R3}] = erlang:process_info(self(), [reductions]),
    testing_long_warning_file(250, lists:seq(1, 1000)),
    [{_, R4}] = erlang:process_info(self(), [reductions]),

    [{_, R5}] = erlang:process_info(self(), [reductions]),
    testing_formater_warning_file(250),
    [{_, R6}] = erlang:process_info(self(), [reductions]),

    {R1, R2, R3, R4, R5, R6}.

%%%#########################################################################%%%

%% многопоточное логгирование (логгирование из нескольких потоков)
%%%#########################################################################%%%
%%Для одного файла, много потоков, короткие сообщения, одна маска.
start_test_MANY_threads_ONE_log_file() ->
    {HeadTime_R1, HeadTime_R2, HeadTime_R3} = gen_server:call(?MODULE, many_thread_one_log, infinity),
    gen_server:call(?MODULE, {result, HeadTime_R1, HeadTime_R2, HeadTime_R3}, infinity).

time_test_MANY_threads_ONE_log_file() ->
    {R1, ok} = timer:tc(testing_chronica_logs, testing_short_warning_file, [1000]),
    {R2, ok} = timer:tc(testing_chronica_logs, testing_long_warning_file, [1000, lists:seq(1, 1000)]),
    {R3, ok} = timer:tc(testing_chronica_logs, testing_formater_warning_file, [1000]),
    {R1, R2, R3}.

%%Для разных файлов, много потоков, короткие сообщения, одна маска
start_test_MANY_threads_MANY_log_files() ->
    {HeadTime_R1, HeadTime_R2, HeadTime_R3} = gen_server:call(?MODULE, many_thread_many_log, infinity),
    gen_server:call(?MODULE, {result, HeadTime_R1, HeadTime_R2, HeadTime_R3}, infinity).

time_test_MANY_threads_MANY_log_files() ->
    {R1, ok} = timer:tc(testing_chronica_logs, testing_short_warning_file, [250]),
    {R2, ok} = timer:tc(testing_chronica_logs, testing_long_warning_file, [250, lists:seq(1, 1000)]),
    {R3, ok} = timer:tc(testing_chronica_logs, testing_formater_warning_file, [250]),
    {R1, R2, R3}.
%%%#########################################################################%%%

%%%#########################################################################%%%
%%gen_server callback
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("~p starting tests chronica ~n",[?MODULE]),
    erlang:process_flag(trap_exit, true),
    State = #config_state{},
    {ok, State}.

handle_call(one_thread_one_log, _From, State) ->
    {reply, reductions_test_ONE_thread_ONE_log_file(), State};

handle_call(one_thread_many_log, _From, State) ->
    {reply, reductions_test_ONE_thread_MANY_log_files(), State};

handle_call(many_thread_one_log, _From, State) ->
    {reply, time_test_MANY_threads_ONE_log_file(), State};

handle_call(many_thread_many_log, _From, State) ->
    {reply, time_test_MANY_threads_MANY_log_files(), State};

handle_call(clear, _From, _State) ->
    {reply, ok, #config_state{number = 0, buffertime_r1 = [], buffertime_r2 = [], buffertime_r3 = [],
                                          maxtime_r1 = 0, maxtime_r2 = 0, maxtime_r3 = 0}};

handle_call({result, HeadTime_R1, HeadTime_R2, HeadTime_R3}, _From,
    _State = #config_state{number = N, buffertime_r1 = TailTime_R1, buffertime_r2 = TailTime_R2, buffertime_r3 = TailTime_R3,
                                       maxtime_r1 = Max_R1, maxtime_r2 = Max_R2, maxtime_r3 = Max_R3}) ->
    NewState =
        #config_state{number = N + 1, buffertime_r1 = [HeadTime_R1 | TailTime_R1],
            buffertime_r2 = [HeadTime_R2 | TailTime_R2], buffertime_r3 = [HeadTime_R3 | TailTime_R3],
            maxtime_r1 = minmax(Max_R1, HeadTime_R1), maxtime_r2 = minmax(Max_R2, HeadTime_R2),
            maxtime_r3 = minmax(Max_R3, HeadTime_R3) },

    R1 = 101,

    case NewState#config_state.number =:= R1 of
        true ->
            SumTime_R1 = lists:sum(NewState#config_state.buffertime_r1),
            file:write_file("Benchmark", io_lib:fwrite("testing_short_message_file AverageTime ~p ms\n", [SumTime_R1/101/1000]), [append]),
            file:write_file("Benchmark", "-------------------------------------------------------------------------------------\n", [append]),

            SumTime_R2 = lists:sum(NewState#config_state.buffertime_r2),
            file:write_file("Benchmark", io_lib:fwrite("testing_long_message_file AverageTime ~p ms\n", [SumTime_R2/101/1000]), [append]),
            file:write_file("Benchmark", "-------------------------------------------------------------------------------------\n", [append]),

            SumTime_R3 = lists:sum(NewState#config_state.buffertime_r3),
            file:write_file("Benchmark", io_lib:fwrite("testing_formater_message_file AverageTime ~p ms\n", [SumTime_R3/101/1000]), [append]);
        false ->
            ok
    end,
    {reply, ok, NewState}.

handle_cast(_Msg, N)  -> {noreply, N}.

handle_info(_Info, State)  -> {noreply, State}.

terminate(_Reason, _State) ->
    io:format("stopping tests chronica ~n"),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
%%%#########################################################################%%%