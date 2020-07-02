%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(asim_ext_sim_index).
-author("madalin").


-include("../include/asim.hrl").

%% API
-export([handle/1]).

%% @doc Handle home
handle(State = #asim_state{ob = Ob}) ->

	%% Create table navigator
	{TableNavigator, NewState} = asim_lib_web_table_nav:create([
		{table, asim_simulation},
		{record, asim_simulation},
		{columns, [
			id,
			cycle,
			rules,
			population,
			max_player_taxes,
			g_energy_produced,
			b_taxes_collected,
			time_started,
			time_ended]},
		{key, id},
		{operations, [
			{custom, [
				{button, #asim_ob_toolbar_button{
					img                 = asim_lib_web_url:get_asset_url(<<"/images/oxygen/64x64/apps/utilities-system-monitor.png">>),
					name                = <<"View simulation graphs">>
				}},
				{selection, 1},
				{action_extension, <<"charts">>},
				{action_module, <<"index">>},
				{action_operation, <<"default">>}
			]},
			{delete_many, []}
		]}
	], State),

	%% Render navigator to output buffer
	{NewOb, _} = asim_lib_web_table_nav:render(Ob, TableNavigator),


	%% Render output buffer and reply
	asim_lib_web_ob:render_and_reply(NewState#asim_state{ob = NewOb}).
