-module(logshipper_server_worker).

-behaviour(gen_server).

%% API
-export([
        start_link/3
    ]).

%% gen_server callbacks
-export([
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3
    ]).

-record(state, { socket, timer, timeout, module, state }).

-define(DEFAULT_TIMEOUT, 10000).

-define(TYPE_CLIENT_HEARTBEAT, 1).
-define(TYPE_DATA, 2).
-define(TYPE_CORRUPTED_DATA, 3).
-define(TYPE_SERVER_HEARTBEAT, 4).
-define(TYPE_ACKNOWLEDGEMENT, 5).
-define(TYPE_SUSPEND_WRITING, 6).
-define(TYPE_RESUME_WRITING, 7).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Sock, I, Opts) ->
    Port = proplists:get_value(port, Opts),
    Name = list_to_atom(atom_to_list(?MODULE) ++ "_port" ++ integer_to_list(Port) ++ "_" ++ integer_to_list(I)),
    gen_server:start_link({local, Name}, ?MODULE, [Sock, Opts], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ListenSocket, Opts]) ->
    self() ! {accept, ListenSocket},
    Timeout = proplists:get_value(timeout, Opts, ?DEFAULT_TIMEOUT),
    Module = proplists:get_value(module, Opts),
    Args = proplists:get_value(args, Opts),
    {ok, St} = Module:init(Args),
    {ok, #state{ timeout = Timeout, module = Module, state = St }}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({acknowledge, Pos}, #state{ socket = Socket } = State) ->
    ok = gen_tcp:send(Socket, <<?TYPE_ACKNOWLEDGEMENT:8, Pos:64>>),
    {noreply, State};

handle_cast(suspend_writing, #state{ socket = Socket} = State) ->
    ok = gen_tcp:send(Socket, <<?TYPE_SUSPEND_WRITING:8>>),
    {noreply, State};

handle_cast(resume_writing, #state{ socket = Socket } = State) ->
    ok = gen_tcp:send(Socket, <<?TYPE_RESUME_WRITING:8>>),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({accept, ListenSocket}, #state{ timeout = Timeout } = State) ->
    Socket =
        case gen_tcp:accept(ListenSocket) of
            {ok, S} -> S;
            {error, Reason} ->
                error_logger:error_msg("gen_tcp:accept failed, reason: ~p~n", [Reason]),
                throw({accept_failed, Reason})
        end,

    {ok, <<"EXL2">>} = gen_tcp:recv(Socket, 4, Timeout),
    ok = gen_tcp:send(Socket, <<"EXL2">>),

    inet:setopts(Socket,[{active, once}, {packet, 4}]),
    Timer = erlang:send_after(Timeout, self(), timeout),
    {noreply, State#state{ timer = Timer, socket = Socket }};

handle_info({tcp, S, <<Type:8, Payload/binary>>}, #state{ module = Module, state = St, timeout = Timeout, timer = Timer } = State) ->
    erlang:cancel_timer(Timer),

    {ok, NewSt} = process_message(S, Type, Payload, Module, St),

    inet:setopts(S, [{active, once}]),
    NewTimer = erlang:send_after(Timeout, self(), timeout),
    {noreply, State#state{ timer = NewTimer, state = NewSt } };

handle_info({tcp_closed, _}, State) ->
    error_logger:error_msg("Logshipper server worker - tcp connection closed~n"),
    {stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
    error_logger:error_msg("Logshipper server worker - TCP error: ~p~n", [Reason]),
    {stop, normal, State};

handle_info(timeout, State) ->
    error_logger:error_msg("Logshipper server worker - timeout~n"),
    {stop, normal, State};

handle_info(Msg, State) ->
    error_logger:error_msg("Logshipper server worker - unexpected message: ~w~n", [Msg]),
    {noreply, State}.

terminate(Reason, #state{ module = Module, state = St }) ->
    Module:terminate(Reason, St).

code_change(OldVsn, #state{ module = Module, state = St } = State, Extra) ->
    {ok, NewSt} = Module:code_change(OldVsn, St, Extra),
    {ok, State#state{ state = NewSt} }.

%%%===================================================================
%%% Internal functions
%%%===================================================================

process_message(S, ?TYPE_CLIENT_HEARTBEAT, <<Payload:8/binary>>, _Module, St) ->
    ok = gen_tcp:send(S, <<?TYPE_SERVER_HEARTBEAT:8, Payload:8/binary>>),
    {ok, St};

process_message(_S, ?TYPE_DATA, <<Pos:64, Data/binary>>, Module, St) ->
    Module:handle_data(Pos, Data, St);
        
process_message(_S, ?TYPE_CORRUPTED_DATA, <<Pos:64, Data/binary>>, Module, St) ->
    Module:handle_corrupted_data(Pos, Data, St);

process_message(_S, Type, Payload, _Module, _St) ->
    error_logger:error_msg("Logshipper server worker - unexpected data: Type = ~w, Payload = ~w~n", [Type, Payload]),
    throw({unexpected_data, Type, Payload}).
