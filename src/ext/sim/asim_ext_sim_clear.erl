%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(asim_ext_sim_clear).
-author("madalin").

-include("../include/asim.hrl").

%% API
-export([handle/1]).

%% @doc Display simulation create form
handle(State = #asim_state{extension_module_operation = Operation}) ->

	%% Clear all ended simulations from simulation server
	asim_srv_sim_manager:simulation_clear_status(),

	%% Redirect
	SuccessUrl = asim_lib_web_url:get_current_extension(<<"index">>, State),
	asim_lib_web_redirect:url(<<"All ended simulation where removed from simulation server! You can still find the result of the simulations in the Results section...">>, SuccessUrl, State).