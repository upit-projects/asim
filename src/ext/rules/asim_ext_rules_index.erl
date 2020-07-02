%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(asim_ext_rules_index).
-author("madalin").


-include("../include/asim.hrl").

%% API
-export([handle/1]).

%% @doc Handle home
handle(State = #asim_state{ob = Ob}) ->

	%% Create table navigator
	{TableNavigator, NewState} = asim_lib_web_table_nav:create([
		{table, asim_simulation_rules},
		{record, asim_simulation_rules},
		{columns, [
			id,
			name,
			stop_cycle,
			population_count,
			genome_size,
			selection_count,
			fitness_function,
			profit_redistribution_function,
			c_cost_of_the_simulation,
			p_capital_multiplier,
			x_taxation_percent,
			m_gene_miner_energy,
			t_gene_trader_energy,
			h_gene_hoarder_ability,
			z_gene_thief_ability,
			time_created]},
		{key, id},
		{operations, [
			{custom, [
				{button, #asim_ob_toolbar_button{
					img                 = asim_lib_web_url:get_asset_url(<<"/images/oxygen/64x64/actions/system-run.png">>),
					name                = <<"Start simulation">>
				}},
				{selection, 2},
				{action_extension, <<"sim">>},
				{action_module, <<"create">>},
				{action_operation, <<"default">>}
			]},
			{create, []},
			{update, []},
			{delete_many, []}
		]}
	], State),

	%% Render navigator to output buffer
	{NewOb, _} = asim_lib_web_table_nav:render(Ob, TableNavigator),

	%% Render output buffer and reply
	asim_lib_web_ob:render_and_reply(NewState#asim_state{ob = NewOb}).
