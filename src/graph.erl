-module(graph).
 
-compile([export_all]).

test_graph() ->
	[
		{0,1,1.0},
		{0,2,1.0},
		{1,3,0.1},
		{3,4,1.0},
		{4,5,1.0}
	].

connected(Graph) ->
	G = digraph:new(),
	lists:map(
		fun({VertexFrom, VertexTo, _Cost}) ->
				digraph:add_vertex(G, VertexFrom),
				digraph:add_vertex(G, VertexTo),
				digraph:add_edge(G, VertexFrom, VertexTo)
		end, Graph),
	Components = lists:map(
		fun(Component) ->
				%Given this list of vertices, which edges are in the graph?
				%Get it working, then optimise
				lists:filter(fun({V1, V2, _Cost}) ->
							case lists:member(V1, Component) of
								true ->
									true;
								false ->
									lists:member(V2, Component)
							end 
					end, Graph)
		end,
		digraph_utils:components(G)),
	digraph:delete(G),
	Components.

reduce_graph(Graph) ->
	[_|NewGraph] = lists:keysort(3,Graph),
	NewGraph.

valid_graph(Graph) ->
	length(Graph) < 4.

split_graph(Graph) ->
	case valid_graph(Graph) of
		true ->
			Graph;
		false ->
			split_graph([], [Graph], fun valid_graph/1)
	end.
split_graph(Valid, [], _) ->
	Valid;
split_graph(Valid, Invalid, ValidationFunction) ->
	{NewValid, NewInvalid} = lists:partition(ValidationFunction, lists:flatmap(fun(G) -> connected(reduce_graph(G)) end, Invalid)),
	split_graph(NewValid ++ Valid, NewInvalid, ValidationFunction).


start() ->
	split_graph(test_graph()).
