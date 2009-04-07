%%%-------------------------------------------------------------------
%%% @author Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% @copyright 2008-2009 Torbjorn Tornkvist
%%% @since Created 27 Mar 2008 
%%% @reference Licensed under the 'MIT License'
%%% @title An Erlang-Yaws 'rev-proxy'/dispatcher ting.
%%%
%%% @doc mbrain - An Erlang 'rev-proxy'/dispatcher ting.
%%%
%%%    Run your Yaws web server as a front-end to your various
%%%    Erlang applications. The server should be named with
%%%    '-sname mbrain' Each Yaws-server spec. should point to 
%%%    the corresponding docroot, where the applications yaws files
%%%    resides. Each Yaws file should only contain a call to
%%%    mbrain:call/4, which will make an Erlang RPC to that
%%%    node. Each application should at startup time run 
%%%    mbrain:ping/0 to connect to the mbrain node.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(mbrain).

-export([call/4
         ,ping/0
         ,ping/1
        ]).

-define(MBRAIN, ?MODULE).

-define(minute,    60*1000).        % milli seconds
-define(hour,      60 * ?minute).


%%% --------------------------------------------------------------------
%%% @spec ping() -> pid()
%%%
%%% @doc Will setup a connection to the 'mbrain' frontend node on the same Host.
%%%      A watcher process will be started that will ensure that the 
%%%      connection always is maintained.
%%% @end
%%% --------------------------------------------------------------------
ping() ->
    [_,Host] = nn_split(node()),
    ping(Host).

%%% --------------------------------------------------------------------
%%% @spec ping(Host::string()) -> pong | pang
%%%
%%% @doc Will setup a connection to the 'mbrain' frontend node on the Host.
%%%      A watcher process will be started that will ensure that the 
%%%      connection always is maintained.
%%% @end
%%% --------------------------------------------------------------------
ping(Host) when is_list(Host) ->
    [_,Host] = nn_split(node()),
    Mbrain = nn_join([a2l(?MBRAIN),Host]),
    case net_adm:ping(Mbrain) of
        pong -> start_lazy_watcher(Mbrain);
        pang -> start_eager_watcher(Mbrain)
    end.

start_lazy_watcher(Node) ->
    start_watcher(Node, ?hour).

start_eager_watcher(Node) ->
    start_watcher(Node, ?minute).

%% Start a watcher that will try to ensure that the
%% backend node always is connected to the 'mbrain' node.
start_watcher(Node, Secs) ->
    spawn(fun() -> init_watcher(Node, Secs) end).

init_watcher(Node, Secs) ->
    %% Avoid multiple watchers of the same Mbrain node.
    try register(list_to_atom(lists:concat(["mbrain_watching",Node])),self()) of
        true  -> loop_watcher(Node, Secs)
    catch _:_ -> false
    end.

loop_watcher(Node, Secs) ->            
    timer:sleep(Secs),
    case net_adm:ping(Node) of
        pong -> loop_watcher(Node, ?hour);
        pang -> loop_watcher(Node, ?minute)
    end.
    

%%% --------------------------------------------------------------------
%%% @spec call(Node::atom(), Mod::atom(), Fun::atom(), Args::list()) -> term()
%%%
%%% @doc Will dispatch the call to the specified Node.
%%%      This function should be called as the only function
%%%      within a .yaws file. Typically this will call the out/1 
%%%      function of the corresponding Erlang module. Note that
%%%      it is important to return data that the Yaws server in the
%%%      'mbrain' node can consume. Example:
%%%
%%% <pre>
%%% % Inside example.yaws
%%% out(Arg) -> mbrain:call(mynode, example_yaws, out, [Arg]).
%%% </pre>
%%% @end
%%% --------------------------------------------------------------------
call(Node, Mod, Fun, Args) when is_atom(Node) ->
    call([Node], Mod, Fun, Args);

%%% --------------------------------------------------------------------
%%% @spec call(Nodes::list(), Mod::atom(), Fun::atom(), Args::list()) -> term()
%%%
%%% @doc Works as {@link call/4} but takes a list of nodes as
%%%      the first argument. This makes it possible to dispatch
%%%      the traffic to several backend, possibly on different machines.
%%%      In case of several Nodes, a random Node will be choosen.
%%% @end
%%% --------------------------------------------------------------------
call(Nodes, Mod, Fun, Args) when is_list(Nodes) ->
    SNodes = [a2l(S) || S <- Nodes],
    Ls = [nn_split(X) || X <- nodes()],
    Ns = [nn_join([N,H]) || [N,H] <- Ls,
                            lists:member(N,SNodes) == true],
    if (Ns /= []) -> rpc:call(random_pick(Ns), Mod, Fun, Args);
       true       -> exit({no_node_found, Nodes})
    end.
    


random_pick([N]) -> N;
random_pick(Ns)  ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    lists:nth(random:uniform(length(Ns)), Ns).


nn_split(NodeName) when is_atom(NodeName) ->
    string:tokens(atom_to_list(NodeName), "@").

nn_join([Node,Host]) when is_list(Node),is_list(Host) ->
    list_to_atom(Node++"@"++Host).
     
a2l(A) when is_atom(A) -> atom_to_list(A).

    
        
