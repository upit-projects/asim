%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(asim_ext_sim_status).
-author("madalin").

-include("../include/asim.hrl").

%% API
-export([handle/1]).
-export([get_html/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Display simulations status
handle(State = #asim_state{ob = Ob}) ->

	%% Get simulation status from simulation manager server
	{ok, Status} = asim_srv_sim_manager:simulation_status(),

	%% Order status
	OrderedStatus = order_status(Status),

	%% Build html
	build_html(OrderedStatus, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% build_html
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Output some message when there are no simulations running
build_html([], State = #asim_state{req = Req, opts = Opts}) ->

	Body = <<"<h2>No simulations are curently running!</h2><p>Please create new simulations in order to be able to see simulations progress.</p>">>,
	{ok, asim_lib_http_reply:respond_html(200, Req, [], Body), Opts};

%% We have simulation data, iterate the data and output the data into
%% a readable manner.
build_html(Status, State) -> build_html(Status, <<>>, State).

%% Iterate status of the simulations
build_html([{_, V}|T], Acum, State = #asim_state{}) ->

	%% Continue iteration
	build_html(T, iolist_to_binary([Acum, V]), State);

%% Handle iteration termination.
build_html([], Acum, State = #asim_state{req = Req, opts = Opts}) ->

	{ok, asim_lib_http_reply:respond_html(200, Req, [], Acum), Opts}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get_html
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Convert simulations to html
get_html(Status) ->	get_html_iterate(get_html_order_status(Status)).

%% We have simulation data, iterate the data and output the data into
%% a readable manner.
get_html_iterate(Status) -> get_html_iterate(Status, <<>>).

%% Iterate status of the simulations
get_html_iterate([{_, V}|T], Acum) ->

	%% Continue iteration
	get_html_iterate(T, iolist_to_binary([Acum, V]));

%% Output some message when there are no simulations
get_html_iterate(_, <<>>) -> <<"<h2>There are no results in database!</h2><p>Please create new simulations in order to be able to see simulations results...</p>">>;

%% Handle iteration termination.
get_html_iterate([], Acum) -> Acum.

%% Status come out in various order and it is hard to watch
%% when simulations exchange positions. Reorder status by simulation id.
get_html_order_status([]) -> [];
get_html_order_status(List) -> get_html_order_status(List, orddict:new()).
get_html_order_status([Simulation = #asim_simulation{id = SimId} | T], Dict) ->
	Html = convert_simulation_to_html(Simulation),
	NewDict = orddict:append(SimId, Html, Dict),
	get_html_order_status(T, NewDict);
get_html_order_status([_ | T], Dict) -> get_html_order_status(T, Dict);
get_html_order_status(_, Dict) -> orddict:to_list(Dict).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% order_status
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Status come out in various order and it is hard to watch
%% when simulations exchange positions. Reorder status by simulation id.
order_status([]) -> [];
order_status(List) -> order_status(List, orddict:new()).
order_status([{_, Simulation = #asim_simulation{id = SimId}} | T], Dict) ->
	Html = convert_simulation_to_html(Simulation),
	NewDict = orddict:append(SimId, Html, Dict),
	order_status(T, NewDict);
order_status([_ | T], Dict) -> order_status(T, Dict);
order_status(_, Dict) -> orddict:to_list(Dict).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% convert simulation to html
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Convert simulation to html
convert_simulation_to_html(Simulation = #asim_simulation{

	%% Simulation unique ID
	id = SimId

}) ->

	iolist_to_binary([
		"<div class=\"simulation\">",
		"<div class=\"simulation-id\">Simulation ID: <span><input type=\"text\" class=\"input-text\" value=\"", SimId, "\" readonly=\"readonly\" /></span></div>",
		"</div>"
	]);
convert_simulation_to_html(_Simulation) -> <<>>.
