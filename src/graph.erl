-module(graph).
 
-compile([export_all]).

test_graph() ->
	[
		{{0,0},{1,0},1.0},
		{{0,0},{1,1},1.0},
		{{0,0},{1,2},0.1},
		{{0,1},{1,2},1.0},
		{{0,1},{1,3},1.0}
	].

generate_random_graph(Vertices, Density, Frames) ->
	[{{Frame, random:uniform(Vertices)}, {Frame + 1, random:uniform(Vertices)}, random:uniform()} || Frame <- lists:seq(1,Frames), _ <- lists:seq(1,round(Vertices * Density))].


reduce_graph(Graph) ->
	[_|NewGraph] = lists:keysort(3,Graph),
	NewGraph.

digraph_components(Graph) ->
	G = digraph:new(),
	lists:map(
		fun({VertexFrom, VertexTo, _Cost}) ->
				digraph:add_vertex(G, VertexFrom),
				digraph:add_vertex(G, VertexTo),
				digraph:add_edge(G, VertexFrom, VertexTo)
		end, Graph),
	GraphComponents = digraph_utils:components(G),
	digraph:delete(G),
	GraphComponents.

connected(Graph) ->
	GraphComponents = digraph_components(Graph),
	lists:map(
		fun(Component) ->
				lists:filter(fun({V1, V2, _Cost}) ->
							case lists:member(V1, Component) of
								true ->
									true;
								false ->
									lists:member(V2, Component)
							end 
					end,
					Graph)
		end,
		GraphComponents).

valid_graph(Graph) ->
	length(Graph) < 20.

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
