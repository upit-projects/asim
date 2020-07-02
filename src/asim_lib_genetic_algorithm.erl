%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%% Implements the genetic algorithm used for creating and mantaining the simulation population
%%% @end
%%%-------------------------------------------------------------------
-module(asim_lib_genetic_algorithm).
-author("madalin").

-include("../include/asim.hrl").

%% API
-export([initial_population_create/1]).
-export([cycle/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% initial_population
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Creates initial population for the specified simulation
initial_population_create(State = #asim_simulation{rules = #asim_simulation_rules{
  population_count = PopulationCount,
  genome_size = GenomeSize
}}) ->

  State#asim_simulation{population = initial_population_create_members(PopulationCount, GenomeSize)}.

%% Create population
initial_population_create_members(PopulationCount, GenomeSize) ->
  initial_population_create_member(PopulationCount, GenomeSize, []).

%% Create each member of the population through recursive function
initial_population_create_member(PopulationCount, GenomeSize, Population) when PopulationCount > 0 ->
  Player = player_random(GenomeSize),
  initial_population_create_member(PopulationCount-1, GenomeSize, [Player | Population]);
initial_population_create_member(_PopulationCount, _GenomeSize, Population) -> Population.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cycle
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Cycle perform selection, crossover and mutation
cycle(State = #asim_simulation{rules = #asim_simulation_rules{
  population_count = PopulationCount,
  genome_size = GenomeSize,
  fitness_function = FitnessFunction,
  selection_count = SelectionCount
}, population = Population,
  max_player_taxes = MaxPlayerTaxes
  }) ->

  %% Compute fitness for the entire population
  PopulationFitness = cycle_population_fitness(FitnessFunction, Population, MaxPlayerTaxes),

  %% Order population by fitness
  PopulationOrdered = cycle_population_fitness_order(PopulationFitness),

  %% Create new population by selection and mate
  State#asim_simulation{
    population = cycle_create_new_population(PopulationCount, SelectionCount, PopulationOrdered, GenomeSize)
  }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cycle_population_fitness
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Compute population fitness
cycle_population_fitness(FitnessFunction, Population, MaxPlayerTaxes) ->
  cycle_population_fitness(FitnessFunction, Population, MaxPlayerTaxes, []).

%% Profit oriented fitness function
cycle_population_fitness(<<"profit_oriented">>, [Head = #asim_player{taxes = T}|Tail], M, Acum) when T < 1; M < 1 ->
  Fitness = 0.0,
  cycle_population_fitness(<<"profit_oriented">>, Tail, M, [Head#asim_player{fitness = Fitness}| Acum]);
cycle_population_fitness(<<"profit_oriented">>, [Head = #asim_player{taxes = T}|Tail], M, Acum) ->
  Fitness = T / M,
  cycle_population_fitness(<<"profit_oriented">>, Tail, M, [Head#asim_player{fitness = Fitness}| Acum]);

%% Unfair fitness function
cycle_population_fitness(<<"unfair">>, [Head = #asim_player{taxes = T}|Tail], M, Acum) when T < 1; M < 1 ->
  Fitness = 1.0,
  cycle_population_fitness(<<"unfair">>, Tail, M, [Head#asim_player{fitness = Fitness}| Acum]);
cycle_population_fitness(<<"unfair">>, [Head = #asim_player{taxes = T}|Tail], M, Acum) ->
  Fitness = (M - T) / M,
  cycle_population_fitness(<<"unfair">>, Tail, M, [Head#asim_player{fitness = Fitness}| Acum]);

%% Neutral fitness function
cycle_population_fitness(neutral, [Head|Tail], M, Acum) ->
  Fitness = 1.0,
  cycle_population_fitness(neutral, Tail, M, [Head#asim_player{fitness = Fitness}| Acum]);

%% Iteration end
cycle_population_fitness(_FitnessFunction, [], _M, Acum) -> Acum.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cycle_population_fitness_order
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cycle_population_fitness_order(Population) ->
  SortFun = fun(Player1, Player2) -> Player1#asim_player.fitness > Player2#asim_player.fitness end,
  lists:sort(SortFun, Population).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cycle_create_new_population
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cycle_create_new_population(PopulationCount, SelectionCount, Population, GenomeSize) ->
  cycle_create_new_population(PopulationCount, SelectionCount, Population, GenomeSize, []).

cycle_create_new_population(PopulationCount, SelectionCount, Population, GenomeSize, Acum) when
  PopulationCount > 0 ->
  %% Select random players from first SelectionCount
  Parent1Index = asim_lib_utils_random:integer_interval(1, SelectionCount),
  Parent2Index = asim_lib_utils_random:integer_interval(1, SelectionCount),
  Parent1 = lists:nth(Parent1Index, Population),
  Parent2 = lists:nth(Parent2Index, Population),
  Child = player_mate(Parent1, Parent2, GenomeSize),
  cycle_create_new_population(PopulationCount - 1, SelectionCount, Population, [Child | Acum]);
cycle_create_new_population(_PopulationCount, _SelectionCount, _Population, _GenomeSize, Acum) ->
  Acum.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% player
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Creates a random player
player_random(GenomeSize) ->
  #asim_player{
    genome = player_random_genome(GenomeSize, [])
  }.

%% Create a random player genome
player_random_genome(GenesCount, Genome) when GenesCount > 0 ->
  player_random_genome(GenesCount - 1, [ gene_random() | Genome]);
player_random_genome(_GenesCount, Genome) -> Genome.

%% Create child by mating the specified players
player_mate(#asim_player{
    genome = Genome1,
    capital = Capital1
    },
    #asim_player{
      genome = Genome2,
      capital = Capital2
    }, GenomeSize) ->

  %% Chose a random gene to mutate
  GeneToMutate = asim_lib_utils_random:integer_interval(1, GenomeSize),

  %% Create the child genome
  ChildGenome = player_mate_genomes(Genome1, Genome2, GeneToMutate),

  %% TODO: Lose capital in some random cases?
  #asim_player{
    genome = ChildGenome,
    capital = Capital1 + Capital2
  }.

%% The actual function that mate the players genomes
player_mate_genomes(Genome1, Genome2, GeneToMutate) ->
  player_mate_genomes(Genome1, Genome2, GeneToMutate, 1, []).
player_mate_genomes([_H1|T1], [_H2|T2], GeneToMutate, GeneToMutate, Acum) ->
  player_mate_genomes(T1, T2, GeneToMutate, GeneToMutate + 1, [ gene_random() | Acum]);
player_mate_genomes([H1|T1], [H2|T2], GeneToMutate, CurrentGeneIndex, Acum) ->
  player_mate_genomes(T1, T2, GeneToMutate, CurrentGeneIndex + 1, [ gene_random(H1, H2) | Acum]);
player_mate_genomes([], [], _GeneToMutate, _CurrentGeneIndex, Acum) -> Acum.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gene
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Creates a completely random gene
gene_random() -> asim_lib_utils_random:integer_interval(1, ?ASIM_PLAYER_GENES_POOL_SIZE).

%% Create a new gene by randomly choosing from the specified two genes
gene_random(Gene1, Gene2) when
  erlang:is_integer(Gene1),
  erlang:is_integer(Gene2) ->
  case asim_lib_utils_random:true_or_false() of
    true -> Gene1;
    _ -> Gene2
  end.