%%%-------------------------------------------------------------------
%%% Created : 27 Mar 2008 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Desc.   : An Erlang 'rev-proxy'/dispatcher ting.
%%%
%%%    Run your Yaws web server as a front-end to your various
%%%    Erlang applications. The server should be named with
%%%    '-sname mbrain' Each Yaws-server spec. should point to 
%%%    the corresponding docroot, where the applications yaws files
%%%    resides. Each Yaws file should only contain a call to
%%%    mbrain:call/4, which will make an Erlang rpc to that
%%%    node. Each application should at startup time run 
%%%    mbrain:ping/0 to connect to the mbrain node.
%%%-------------------------------------------------------------------
-module(mbrain).

-export([call/4, ping/0]).

-define(MBRAIN, ?MODULE).

ping() ->
    [_,Host] = nn_split(node()),
    net_adm:ping(nn_join([a2l(?MBRAIN),Host])).


call(Node, Mod, Fun, Args) when is_atom(Node) ->
    SNode = a2l(Node),
    Ls = [nn_split(X) || X <- nodes()],
    Ns = [nn_join([N,H]) || [N,H] <- Ls,
                            N == SNode],
    if (Ns /= []) -> rpc:call(random_pick(Ns), Mod, Fun, Args);
       true       -> exit({no_node_found, Node})
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

    
        
