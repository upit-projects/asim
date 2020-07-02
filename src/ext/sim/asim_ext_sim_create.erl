%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(asim_ext_sim_create).
-author("madalin").

-include("../include/asim.hrl").

%% API
-export([handle/1]).

handle(State = #asim_state{}) ->

	%% Error msg and route
	ErrorMsg = <<"Invalid id! The specified record no longer exist in our database (maybe deleted?)">>,
	ErrorRoute = #asim_route{
		extension = rules
	},

	%% Get id
	Id = asim_lib_web_request:get_selected_key(ErrorMsg, ErrorRoute, <<"id">>, State),

	%% Load simulation rules
	Result = mnesia:dirty_read(asim_simulation_rules, Id),
	case Result of
		[Model] ->

			%% Ask simulation manager server to create the simulation for us
			asim_srv_sim_manager:simulation_create(Model),

			%% Redirect
			SuccessUrl = asim_lib_web_url:get(<<"sim">>, <<"index">>),
			asim_lib_web_redirect:url(<<"The specified simulation was started! Redirecting you to the running simulations navigator...">>, SuccessUrl, State);

		_ ->

			%% Redirect
			ErrorUrl = asim_lib_web_url:get(<<"rules">>),
			asim_lib_web_redirect:url(<<"The specified simulation rules no longer exist (deleted?)! Redirecting you to the simulations rules navigator...">>, ErrorUrl, State)

	end.


