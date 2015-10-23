%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Nikita Roshchupkin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%% Created : Jul 2015
%%%-------------------------------------------------------------------

-module(execution_tests).

-export([benchmark_test/0]).

%%%#########################################################################%%%
%%Chronica
chronica_one_thread_one_log_file() ->
    testing_chronica_logs:start_test_ONE_thread_ONE_log_file().

chronica_one_thread_many_log_files() ->
    testing_chronica_logs:start_test_ONE_thread_MANY_log_files().

chronica_many_threads_one_log_file(0) ->
    timer:sleep(1000),
    testing_chronica_logs:start_test_MANY_threads_ONE_log_file();
chronica_many_threads_one_log_file(Number_thread) ->
    spawn(testing_chronica_logs, start_test_MANY_threads_ONE_log_file, []),
    chronica_many_threads_one_log_file(Number_thread - 1).

chronica_many_threads_many_log_files(0) ->
    timer:sleep(1000),
    testing_chronica_logs:start_test_MANY_threads_MANY_log_files();
chronica_many_threads_many_log_files(Number_thread) ->
    spawn(testing_chronica_logs, start_test_MANY_threads_MANY_log_files, []),
    chronica_many_threads_many_log_files(Number_thread - 1).

chronica_test1() ->
    chronica_manager:update_rule_inwork(warning_file1, true),

    file:write_file("Benchmark", "Testing one_thread_one_log_file() 1000 requests for Chronica\n", [append]),
    chronica_one_thread_one_log_file(),

    file:write_file("Benchmark", "\n                             | |\n", [append]),
    file:write_file("Benchmark", "                             | |\n", [append]),

    file:write_file("Benchmark", "\nTesting many_threads_one_log_file 1000 requests for Chronica. Threads 101.\n", [append]),
    chronica_many_threads_one_log_file(100),
    timer:sleep(1000),
    chronica_manager:update_rule_inwork(warning_file1, false),
    file:write_file("Benchmark", "\n######################################################################################\n", [append]).

chronica_test2() ->
    chronica_manager:update_rule_inwork(warning_file1, true),
    chronica_manager:update_rule_inwork(debug_file1, true),
    chronica_manager:update_rule_inwork(trace_file1, true),
    chronica_manager:update_rule_inwork(info_file1, true),

    file:write_file("Benchmark", "\nTesting one_thread_many_log_files 4 x 250 requests for Chronica. Восходящие логирование до четвертого уровня.\n", [append]),
    chronica_one_thread_many_log_files(),

    file:write_file("Benchmark", "\n                             | |\n", [append]),
    file:write_file("Benchmark", "                             | |\n", [append]),
    testing_chronica_logs:clear(),

    file:write_file("Benchmark", "\nTesting many_threads_many_log_files 4 x 250 requests for Chronica. Threads 101. Восходящие логирование до четвертого уровня.\n", [append]),
    chronica_many_threads_many_log_files(100),
    chronica_manager:update_rule_inwork(warning_file1, false),
    chronica_manager:update_rule_inwork(debug_file1, false),
    chronica_manager:update_rule_inwork(trace_file1, false),
    chronica_manager:update_rule_inwork(info_file1, false).


%%%#########################################################################%%%
%%Lager
lager_one_thread_one_log_file() ->
    testing_lager_logs:start_test_ONE_thread_ONE_log_file().

lager_one_thread_many_log_files() ->
    testing_lager_logs:start_test_ONE_thread_MANY_log_files().

lager_many_threads_one_log_file(0) ->
    timer:sleep(1000),
    testing_lager_logs:start_test_MANY_threads_ONE_log_file();
lager_many_threads_one_log_file(Number_thread) ->
    spawn(testing_lager_logs, start_test_MANY_threads_ONE_log_file, []),
    lager_many_threads_one_log_file(Number_thread - 1).

lager_many_threads_many_log_files(0) ->
    timer:sleep(1000),
    testing_lager_logs:start_test_MANY_threads_MANY_log_files();
lager_many_threads_many_log_files(Number_thread) ->
    spawn(testing_lager_logs, start_test_MANY_threads_MANY_log_files, []),
    lager_many_threads_many_log_files(Number_thread - 1).

lager_test1() ->
    file:write_file("Benchmark", "\nTesting one_thread_one_log_file() 1000 requests for Lager \n", [append]),
    lager_one_thread_one_log_file(),

    file:write_file("Benchmark", "\n                             | |\n", [append]),
    file:write_file("Benchmark", "                             | |\n", [append]),

    file:write_file("Benchmark", "\nTesting many_threads_one_log_file 1000 requests for Lager. Threads 101. \n", [append]),
    lager_many_threads_one_log_file(100),
    timer:sleep(1000),

    file:write_file("Benchmark", "\n######################################################################################\n", [append]).

lager_test2() ->
    file:write_file("Benchmark", "\nTesting one_thread_many_log_files 4 x 250 requests for Lager. Восходящие логирование до четвертого уровня.\n", [append]),
    lager_one_thread_many_log_files(),

    file:write_file("Benchmark", "\n                             | |\n", [append]),
    file:write_file("Benchmark", "                             | |\n", [append]),
    testing_lager_logs:clear(),

    file:write_file("Benchmark", "\nTesting many_threads_many_log_files 4 x 250 requests for Lager. Threads 101. Восходящие логирование до четвертого уровня.\n", [append]),
    lager_many_threads_many_log_files(100).

benchmark_test() ->
    file:write_file("Benchmark", "header call of the test: (ON_record_file) \n", [write]),

    chronica_test1(),
    lager_test1(),
    chronica_test2(),
    file:write_file("Benchmark", "\n######################################################################################\n", [append]),
    lager_test2(),
    io:format("test ok ~n"),
    init:stop().