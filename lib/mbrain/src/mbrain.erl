%%%-------------------------------------------------------------------
%%% Created : 27 Mar 2008 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Desc.   : An Erlang 'rev-proxy'/dispatcher ting.
%%%-------------------------------------------------------------------
-module(mbrain).

-export([call/4, ping/0]).

-define(MBRAIN, ?MODULE).

ping() ->
    [_,Host] = nn_split(node()),
    net_adm:ping(nn_join([?MBRAIN,Host])).


call(Node, Mod, Fun, Args) ->
    Ls = [nn_split(X) || X <- nodes()],
    Ns = [nn_join([N,H]) || [N,H] <- Ls,
                            N == Node],
    NodeName = random_pick(Ns),
    rpc:call(NodeName, Mod, Fun, Args).


random_pick([N]) -> N;
random_pick(Ns)  ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    lists:nth(random:uniform(length(Ns)), Ns).


nn_split(NodeName) when is_atom(NodeName) ->
    string:tokens(atom_to_list(NodeName), "@").

nn_join([Node,Host]) when is_list(Node),is_list(Host) ->
    list_to_atom(Node++"@"++Host).
            
        
