%%%-------------------------------------------------------------------
%%% Created : 27 Mar 2008 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Desc.   : An Erlang 'rev-proxy'/dispatcher ting.
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

    
        
