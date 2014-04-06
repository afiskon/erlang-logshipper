-module(logshipper_server).

-export([
        behaviour_info/1,
        start_link/4,
        start_link/5,
        acknowledge/2,
        suspend_writing/1,
        resume_writing/1
    ]).

behaviour_info(callbacks) ->
    [
     {init, 1},
     {handle_data, 3},
     {handle_corrupted_data, 3},
     {terminate, 2},
     {code_change, 3}
     % TODO: handle_info, handle_call, handle_cast ?
    ];

behaviour_info(_Other) ->
    undefined.

start_link(Module, Args, Port, Workers) ->
    start_link(Module, Args, Port, Workers, []).

start_link(Module, Args, Port, Workers, ListenOptions) ->
    logshipper_server_workers_sup:start_link(
        [
            {module, Module},
            {args, Args},
            {port, Port},
            {workers, Workers},
            {listen_options, ListenOptions}
        ]).

acknowledge(Pid, Pos) when is_integer(Pos) ->
    gen_server:cast(Pid, {acknowledge, Pos}).

suspend_writing(Pid) ->
    gen_server:cast(Pid, suspend_writing).

resume_writing(Pid) ->
    gen_server:cast(Pid, resume_writing).
