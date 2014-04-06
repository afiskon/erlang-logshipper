-module(logshipper_server_workers_sup).

-behaviour(supervisor).

%% API
-export([
        start_link/1
    ]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Opts) ->
    Port = proplists:get_value(port, Opts),
    Name = list_to_atom(atom_to_list(?MODULE) ++ "_port" ++ integer_to_list(Port)),
    supervisor:start_link({local, Name}, ?MODULE, Opts).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init(Opts) ->
    Workers = proplists:get_value(workers, Opts),
    Port = proplists:get_value(port, Opts),
    ListenOpts = proplists:get_value(listen_options, Opts),
    {ok, Sock} = gen_tcp:listen(Port, [binary, {packet, raw}, {active, false}, {send_timeout, 5000}, {send_timeout_close, true}] ++ ListenOpts),
    {ok, {
            {one_for_one, 1000, 10}, % maximum 1000 restarts in 10 seconds
            [
                ?CHILD( list_to_atom("worker" ++ integer_to_list(I)), logshipper_server_worker, worker, [Sock, I, Opts])
                || I <- lists:seq(1, Workers)
            ]
         }
    }.

