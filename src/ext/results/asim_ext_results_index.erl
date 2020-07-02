%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(asim_ext_results_index).
-author("madalin").

-include("../include/asim.hrl").

%% API
-export([handle/1]).


%% @doc Handle home
handle(State = #asim_state{ob = Ob}) ->

	%% Create table navigator
	{TableNavigator, NewState} = asim_lib_web_table_nav:create([
		{table, asim_simulation_history},
		{record, asim_simulation_history},
		{columns, [
			id,
			cycle,
			population,
			max_player_taxes,
			g_energy_produced,
			b_taxes_collected,
			time_created]},
		{key, id},
		{operations, [
			{delete_many, []
			}
		]}
	], State),

	%% Render navigator to output buffer
	{NewOb, _} = asim_lib_web_table_nav:render(Ob, TableNavigator),


	%% Render output buffer and reply
	asim_lib_web_ob:render_and_reply(NewState#asim_state{ob = NewOb}).


