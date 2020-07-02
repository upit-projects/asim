%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(asim_srv_sim).
-author("madalin").

-behaviour(gen_server).

-include("../include/asim.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->

	Name = asim_lib_utils_type_conv:to_atom(iolist_to_binary([asim_lib_utils_type_conv:to_binary(?MODULE),
		<<"_">>,
		asim_lib_utils_type_conv:to_binary(asim_lib_db_ets:get_node_counter())])),

	gen_server:start_link({local, Name}, ?MODULE, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INIT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->

	State = #asim_simulation{},

	%% Seed the number generator
	rand:seed(exsss, {asim_lib_utils_time:microseconds(), asim_lib_utils_time:seconds(), asim_lib_db_ets:get_node_counter()}),

	{ok, State, infinity}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Init this simulation, stop any previous one
handle_call({start, SimulationRules}, From, State) ->

	%% Creates a unique simulation ID
	{{Seconds, _Milliseconds, _Microseconds, _}, Sid} = asim_lib_utils_timeuuid:now(),

	%% Populate current simulation state with new one
	NewState = #asim_simulation{
		id = Sid,
		rules = SimulationRules
	},

	%% Makes us tick - trigger the simulation loop
	gen_server:cast(self(), simulation_loop),

	%% Simulation started... return
	{reply, ok, NewState};

%% @doc Stop any current running simulation
handle_call(stop, From, State) ->

	gen_server:reply(From, ok),
	{stop, normal, State};

%% Handle any other call
handle_call(_Msg, _From, State) -> {reply, error, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_cast
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Handle simulation loop cast
handle_cast(simulation_loop, State) ->

	NewState = simulation_loop(State),

	{noreply, NewState};

%% Handle any other cast
handle_cast(_Msg, State) -> {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info(simulation_loop, State) ->

	NewState = simulation_loop(State),

	{noreply, NewState};

handle_info(_Info, State) -> {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% terminate/code_change
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% simulation_loop
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Create initial population when we are running the first cycle
simulation_loop(State = #asim_simulation{population = undefined}) ->

	%% Keep time started
	TimeStarted = asim_lib_utils_time:milliseconds(),
	StartedState = State#asim_simulation{
		time_started = TimeStarted
	},

	%% Create initial population
	StartedPopulationState = asim_lib_genetic_algorithm:initial_population_create(StartedState),

	%% Insert the simulation result into the database
	mnesia:dirty_write(asim_simulation, StartedPopulationState),

	%% Run first cycle
	simulation_loop(StartedPopulationState);

%% @doc This is the simulation loop
simulation_loop(State = #asim_simulation{
	cycle = Cycle,
	rules = #asim_simulation_rules{
		stop_cycle = StopCycle
	}
}) ->

	%% Run a new simulation cycle
	CycleState = cycle(State),

	%% Increase cycle count
	CurrentCycle = Cycle + 1,
	EndCycleState = CycleState#asim_simulation{
		cycle = CurrentCycle
	},

	%% Insert the simulation cycle into the database
	mnesia:dirty_write(asim_simulation_history, EndCycleState),

	%% Check for simulation termination condition
	case erlang:is_integer(StopCycle) of
		true ->

			case StopCycle > CurrentCycle of
				true ->

					%% Continue simulation
					simulation_loop_continue(EndCycleState);

				_ ->

					%% Simulation ended
					simulation_loop_ended(EndCycleState)

			end;

		_ ->

			%% Continue simulation
			simulation_loop_continue(EndCycleState)

	end.


%% @doc Continue simulation loop when both tanks are still alive
simulation_loop_continue(State) ->

	%% Inform the simulation manager server about the new simulation status
	asim_srv_sim_manager:simulation_report(State),

	%% Makes us tick - trigger the simulation loop
	erlang:send_after(50, self(), simulation_loop),

	%% Return the final state
	State.

%% @doc Stop the simulation loop
simulation_loop_ended(State) ->

	TimeEnded = asim_lib_utils_time:seconds(),

	EndedState = State#asim_simulation{
		time_ended = TimeEnded
	},

	%% Insert the simulation result into the database
	mnesia:dirty_write(asim_simulation, EndedState),

	%% Inform the simulation manager server about the new simulation status
	asim_srv_sim_manager:simulation_report(EndedState),

	%% Return the final state
	EndedState.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cycle
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Execute a simulation cycle
cycle(State = #asim_simulation{
	population = Population,
		rules = #asim_simulation_rules{
	}
}) when erlang:is_list(Population) ->

	%% Redistribute taxes
	RedistributedTaxesState = cycle_redistribute_taxes(State),

	%% Iterate population and produce energy
	ProducedEnergyState = cycle_energy_production(RedistributedTaxesState),

	%% Pay taxes on energy produced
	PayTaxesState = cycle_pay_taxes(ProducedEnergyState),

	%% Perform selection, crossover and mutation
	asim_lib_genetic_algorithm:cycle(PayTaxesState).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cycle_redistribute_taxes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Skip redistribution of taxes on first cycle
cycle_redistribute_taxes(State = #asim_simulation{cycle = 0}) -> State;
%% Redistribute taxes from previous cycle at the beginning of the new cycle
%% (simulate state support)
cycle_redistribute_taxes(State = #asim_simulation{
	g_energy_produced = GEnergyProduced,
	b_taxes_collected = BTaxesCollected,
	population = Population,
	rules = #asim_simulation_rules{
		population_count = PopulationCount,
		c_cost_of_the_simulation = CCostOfTheSimulation,
		profit_redistribution_function = TaxesRedistributionType
	}
}) ->

	NewPopulation = case TaxesRedistributionType of

										<<"meritocracy">> ->

			cycle_redistribute_taxes_meritocracy(Population, CCostOfTheSimulation, BTaxesCollected, []);

		from_ability_to_needs ->

			EqualValue = (GEnergyProduced - (GEnergyProduced * CCostOfTheSimulation)) / PopulationCount,
			cycle_redistribute_taxes_meritocracy_fatn(Population, EqualValue, [])

	end,

	State#asim_simulation{population = NewPopulation}.

%% Redistribute taxes using meritocracy
cycle_redistribute_taxes_meritocracy([H = #asim_player{taxes = TPayedTaxes}|T], CCostOfTheSimulation, BTaxesCollected, Acum) ->
	NewPlayer = H#asim_player{capital = TPayedTaxes - TPayedTaxes * CCostOfTheSimulation},
	cycle_redistribute_taxes_meritocracy(T, CCostOfTheSimulation, BTaxesCollected, [NewPlayer | Acum]);
cycle_redistribute_taxes_meritocracy([], _CCostOfTheSimulation, _BTaxesCollected, Acum) -> Acum.

%% Redistribute taxes using from_ability_to_needs
cycle_redistribute_taxes_meritocracy_fatn([H|T], EqualValue, Acum) ->
	NewPlayer = H#asim_player{capital = EqualValue},
	cycle_redistribute_taxes_meritocracy_fatn(T, EqualValue, [NewPlayer | Acum]);
cycle_redistribute_taxes_meritocracy_fatn([], _EqualValue, Acum) -> Acum.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cycle_energy_production
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Perform energy production
cycle_energy_production(State = #asim_simulation{
	population = Population,
	rules = Rules
}) ->

	{GEnergyProduced, NewPopulation} = cycle_energy_production(Population, Rules, 0, []),
	State#asim_simulation{
		g_energy_produced = GEnergyProduced,
		population = NewPopulation
	}.

%% Cycle through each member of the population
cycle_energy_production([Player = #asim_player{capital = Capital}|TPopulation],
		Rules = #asim_simulation_rules{
			m_gene_miner_energy = MGeneMinerEnergy,
			t_gene_trader_energy = TGeneTraderEnergy,
			h_gene_hoarder_ability = HGeneHoarderAbility,
			p_capital_multiplier = PCapitalMultiplier,
			z_gene_thief_ability = ZGeneThiefAbility
		}, GEnergyProduced, NewPopulation) ->

	{_G0, G1, G2, G3, G4, G5} = cycle_energy_production_count_genes(Player),

	%% The Capital at the beginning of the cycle formula
	Cb = Capital - (Capital * G5 * HGeneHoarderAbility),

	%% Compute the remaining capital
	PlayerRemainingCapital = Capital - Cb,

	%% The raw energy formula production formula
	Er = Cb * PCapitalMultiplier * ((G1 * MGeneMinerEnergy + G2 * TGeneTraderEnergy)/1 + G3),

	%% The stolen capital formula
	Cs = Er * G5 * ZGeneThiefAbility,

	%% Add the stolen capital to the player remaining capital
	PlayerRemainingAndStolenCapital = PlayerRemainingCapital + Cs,

	%% the final legal/clean energy formula
	E = Er - Cs,

	NewPlayer = Player#asim_player{
		production = E,
		capital = PlayerRemainingAndStolenCapital
	},
	cycle_energy_production(TPopulation, Rules, GEnergyProduced + E, [NewPlayer | NewPopulation]);

cycle_energy_production([], _Rules, GEnergyProduced, NewPopulation) -> {GEnergyProduced, NewPopulation}.

%% Count genes
cycle_energy_production_count_genes(Genome) -> cycle_energy_production_count_genes(Genome, 0, 0, 0, 0, 0, 0).
%% archaic gene (has no effect on player behavior)
cycle_energy_production_count_genes([?ASIM_PLAYER_GENE_ARCHAIC|T], G0, G1, G2, G3, G4, G5) ->
	cycle_energy_production_count_genes(T, G0+1, G1, G2, G3, G4, G5);
%% miner gene (increase the mining skill)
cycle_energy_production_count_genes([?ASIM_PLAYER_GENE_MINER|T], G0, G1, G2, G3, G4, G5) ->
cycle_energy_production_count_genes(T, G0, G1+1, G2, G3, G4, G5);
%% trader gene (increase the trading skill)
cycle_energy_production_count_genes([?ASIM_PLAYER_GENE_TRADER|T], G0, G1, G2, G3, G4, G5) ->
	cycle_energy_production_count_genes(T, G0, G1, G2+1, G3, G4, G5);
%% lazy gene (increase the laziness of the player)
cycle_energy_production_count_genes([?ASIM_PLAYER_GENE_LAZY|T], G0, G1, G2, G3, G4, G5) ->
cycle_energy_production_count_genes(T, G0, G1, G2, G3+1, G4, G5);
%% thief gene (increase the thief skill)
cycle_energy_production_count_genes([?ASIM_PLAYER_GENE_THIEF|T], G0, G1, G2, G3, G4, G5) ->
	cycle_energy_production_count_genes(T, G0, G1, G2, G3, G4+1, G5);
%% hoarder gene (increase the predisposition of the player to store resources).
cycle_energy_production_count_genes([?ASIM_PLAYER_GENE_HOARDER|T], G0, G1, G2, G3, G4, G5) ->
	cycle_energy_production_count_genes(T, G0, G1, G2, G3, G4, G5+1);
cycle_energy_production_count_genes([], G0, G1, G2, G3, G4, G5) ->
	{G0, G1, G2, G3, G4, G5}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cycle_pay_taxes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cycle_pay_taxes(State = #asim_simulation{
	population = Population,
	rules = #asim_simulation_rules{
		x_taxation_percent = XTaxationPercent,
		c_cost_of_the_simulation = CCostOfTheSimulation,
		profit_redistribution_function = ProfitRedistributionType
	}
}) ->

	{NewBTaxesCollected, NewPopulation} = case ProfitRedistributionType of

																					<<"meritocracy">> ->

			cycle_pay_taxes_meritocracy(Population, CCostOfTheSimulation, XTaxationPercent, []);

		from_ability_to_needs ->

			cycle_pay_taxes_meritocracy_fatn(Population, 0, [])

	end,

	State#asim_simulation{b_taxes_collected = NewBTaxesCollected, population = NewPopulation}.

%% Pay taxes using meritocracy
cycle_pay_taxes_meritocracy([Player = #asim_player{
	capital = Capital,
	production = EProduction
}|T],
		CCostOfTheSimulation, XTaxationPercent, Acum) ->

	EProductionCosts = EProduction - EProduction * CCostOfTheSimulation,
	TPayedTaxes = EProductionCosts * XTaxationPercent,
	NewCapital = Capital + EProductionCosts - TPayedTaxes,
	NewPlayer = Player#asim_player{capital = NewCapital, taxes = TPayedTaxes},
	cycle_pay_taxes_meritocracy(T, CCostOfTheSimulation, XTaxationPercent, [NewPlayer | Acum]);
cycle_pay_taxes_meritocracy([], _CCostOfTheSimulation, _XTaxationPercent, Acum) -> Acum.

%% Pay taxes using from_ability_to_needs
cycle_pay_taxes_meritocracy_fatn([Player = #asim_player{production = EProduction}|T],
		BTaxesCollected, Acum) ->
	NewBTaxesCollected = BTaxesCollected + EProduction,
	NewPlayer = Player#asim_player{taxes = EProduction},
	cycle_pay_taxes_meritocracy_fatn(T, NewBTaxesCollected, [NewPlayer | Acum]);
cycle_pay_taxes_meritocracy_fatn([], BTaxesCollected, Acum) -> {BTaxesCollected, Acum}.

