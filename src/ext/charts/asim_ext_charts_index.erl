%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(asim_ext_charts_index).
-author("madalin").

-include("../include/asim.hrl").

%% API
-export([handle/1]).

-record(asim_charts_data, {
	max_player_taxes = <<"">>,
	g_energy_produced = <<"">>,
	b_taxes_collected = <<"">>,
	cycles_labels = <<"">>,
	sum_energy_produced = 0,
	sum_taxes_collected = 0
}).


handle(State = #asim_state{ob = Ob}) ->

	%% Error msg and route
	ErrorMsg = <<"Invalid id! The specified simulation no longer exist in our database (maybe deleted?)">>,
	ErrorRoute = #asim_route{
		extension = <<"sim">>
	},

	%% Get id
	Id = asim_lib_web_request:get_selected_key(ErrorMsg, ErrorRoute, <<"id">>, State),

	%% Load from database
	SimulationRead = mnesia:dirty_read(asim_simulation, Id),
	case SimulationRead of
		[Simulation] ->

			Result = mnesia:dirty_read(asim_simulation_history, Id),

			%% Build graphs
			ChartsData = build_charts_data(Result),

			%% Add variables to the template
			FinalOb = asim_lib_web_ob:add_variables([
				{simulation_id, Id},
				{simulation_name, Simulation#asim_simulation.rules#asim_simulation_rules.name},
				{cycles_labels, ChartsData#asim_charts_data.cycles_labels},
				{max_player_taxes, ChartsData#asim_charts_data.max_player_taxes},
				{g_energy_produced, ChartsData#asim_charts_data.g_energy_produced},
				{b_taxes_collected, ChartsData#asim_charts_data.b_taxes_collected},
				{sum_energy_produced, ChartsData#asim_charts_data.sum_energy_produced},
				{sum_taxes_collected, ChartsData#asim_charts_data.sum_taxes_collected},
				{sum_taxes_collected, ChartsData#asim_charts_data.sum_taxes_collected}
			], Ob),

			%% Render and reply
			asim_lib_web_ob:render_and_reply(State#asim_state{ob = FinalOb})

	end.


%% Build graph
build_charts_data(Result) -> build_charts_data(Result, <<"">>, #asim_charts_data{}).
build_charts_data([#asim_simulation_history{
	max_player_taxes = MaxPlayerTaxes,
	g_energy_produced = GEnergyProduced,
	b_taxes_collected = BTaxesCollected,
	cycle = Cycle
}|T],
	Sep,
	#asim_charts_data{
	max_player_taxes = AcumMaxPlayerTaxes,
	g_energy_produced = AcumGEnergyProduced,
	b_taxes_collected = AcumBTaxesCollected,
		cycles_labels = AcumCyclesLabels,
	sum_energy_produced = SumEnergyProduced,
	sum_taxes_collected = SumTaxesCollected
}) ->

	FinalAcum = #asim_charts_data{
		max_player_taxes = erlang:iolist_to_binary([AcumMaxPlayerTaxes, Sep, asim_lib_utils_type_conv:to_binary(MaxPlayerTaxes)]),
		g_energy_produced = erlang:iolist_to_binary([AcumGEnergyProduced, Sep, asim_lib_utils_type_conv:to_binary(GEnergyProduced)]),
		b_taxes_collected = erlang:iolist_to_binary([AcumBTaxesCollected, Sep, asim_lib_utils_type_conv:to_binary(BTaxesCollected)]),
		cycles_labels = erlang:iolist_to_binary([AcumCyclesLabels, Sep, asim_lib_utils_type_conv:to_binary(Cycle)]),
		sum_energy_produced = SumEnergyProduced + GEnergyProduced,
		sum_taxes_collected = SumTaxesCollected + BTaxesCollected
	},
	build_charts_data(T, <<", ">>, FinalAcum);
build_charts_data([], _Sep, Acum) -> Acum.
