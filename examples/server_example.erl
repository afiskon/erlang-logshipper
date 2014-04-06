-module(server_example).

-behaviour(logshipper_server).

-export([
        start_link/3
    ]).

% logshipper_server callbacks
-export([
        init/1,
        handle_data/3,
        handle_corrupted_data/3,
        terminate/2,
        code_change/3
    ]).

-record(state, {}).

start_link(Ip, Port, Workers) ->
    logshipper_server:start_link(?MODULE, [test], Port, Workers, [{ip, Ip}]).

init([test]) ->
    error_logger:info_msg("server_example - init~n"),
    {ok, #state{}}.

handle_data(Pos, Data, State) ->
    error_logger:info_msg("server_example, handle_data: ~p, pos: ~p~n", [Data, Pos]),
    {ok, State}.

handle_corrupted_data(Pos, Data, State) ->
    error_logger:info_msg("server_example, handle_corrupted_data: ~p, pos: ~p~n", [Data, Pos]),
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
