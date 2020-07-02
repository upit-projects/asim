%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(asim_ext_rules_create).
-author("madalin").

-include("../include/asim.hrl").

%% API
-export([handle/1]).
-export([create_form_story/1]).

handle(State = #asim_state{ob = Ob, extension_module_operation = Operation}) when Operation =/= <<"submit">> ->

	%% Create form
	{Form, NewState} = asim_lib_web_form:create([{action_operation, <<"submit">>}], State),

	%% Create a new timeuuid
	{_, Uuid} = asim_lib_utils_timeuuid:now(),

	%% Add story to the form
	StoryForm = asim_lib_web_form:add_story(create_form_story(#asim_simulation_rules{

		%% Simulation rule id
		id = Uuid,

		%% Simulation rule name
		name = <<"">>,

		%% Stop cycle (use undefined for running the simulation forever until stopped manually by the user)
		stop_cycle = 100,

		%% Population count
		population_count = 100,

		%% Genome size
		genome_size = 8,

		%% Selection count (from population count)
		selection_count = 50,

		%% Fitness function type
		fitness_function = <<"profit_oriented">>,

		%% Profit redistribution function
		profit_redistribution_function = <<"meritocracy">>,

		%% C – is the cost of the simulation cycle
		%% (a real number in between 0 and 1 interval)
		c_cost_of_the_simulation = 0.2,

		%% P – is the capital profit multiplier
		%% (a real number in between 0 and 1 interval)
		p_capital_multiplier = 0.1,

		%% X – is the taxation percent from the total amount of energy produced by a player into a simulation cycle
		%% (a real number in between 0 and 1 interval).,
		x_taxation_percent = 0.18,

		%% M – is a constant representing the simulation energy obtained by a miner gene
		m_gene_miner_energy = 16,

		%% T – is a constant representing the simulation energy obtained by a trader gene
		t_gene_trader_energy = 4,

		%% H – is a constant representing the ability to hoard of a hoarder gene
		h_gene_hoarder_ability = 0.1,

		%% Z – is a constant representing the ability to steal of a thief gene
		z_gene_thief_ability = 0.1

	}), Form),

	%% Render form
	{NewOb, _} = asim_lib_web_form:render(Ob, StoryForm),

	%% Render output buffer and reply
	asim_lib_web_ob:render_and_reply(NewState#asim_state{ob = NewOb});

handle(State = #asim_state{request = Request}) ->

	%% Create a new timeuuid
	{{Seconds, _Milliseconds, _Microseconds, _}, Uuid} = asim_lib_utils_timeuuid:now(),

	%% Read all tank information (TODO: more validation)
	Model = #asim_simulation_rules{
		id      = asim_lib_http_request:always_get_trimed_non_empty_binary(<<"id">>, Request, Uuid),
		name = asim_lib_http_request:always_get_trimed_non_empty_binary(<<"name">>, Request, <<"">>),
		stop_cycle = asim_lib_http_request:always_get_integer(<<"stop_cycle">>, Request, 100),
		population_count = asim_lib_http_request:always_get_integer(<<"population_count">>, Request, 100),
		genome_size = asim_lib_http_request:always_get_integer(<<"genome_size">>, Request, 8),
		selection_count = asim_lib_http_request:always_get_integer(<<"selection_count">>, Request, 50),
		fitness_function = asim_lib_http_request:always_get_trimed_non_empty_binary(<<"fitness_function">>, Request, <<"profit_oriented">>),
		profit_redistribution_function = asim_lib_http_request:always_get_trimed_non_empty_binary(<<"profit_redistribution_function">>, Request, <<"meritocracy">>),
		c_cost_of_the_simulation = asim_lib_http_request:always_get_float(<<"c_cost_of_the_simulation">>, Request, 0.2),
		p_capital_multiplier = asim_lib_http_request:always_get_float(<<"p_capital_multiplier">>, Request, 0.1),
		x_taxation_percent = asim_lib_http_request:always_get_float(<<"x_taxation_percent">>, Request, 0.18),
		m_gene_miner_energy = asim_lib_http_request:always_get_integer(<<"m_gene_miner_energy">>, Request, 16),
		t_gene_trader_energy = asim_lib_http_request:always_get_integer(<<"t_gene_trader_energy">>, Request, 4),
		h_gene_hoarder_ability = asim_lib_http_request:always_get_float(<<"h_gene_hoarder_ability">>, Request, 0.1),
		z_gene_thief_ability = asim_lib_http_request:always_get_float(<<"z_gene_thief_ability">>, Request, 0.1),
		time_created = Seconds
	},

	%% Insert record into database
	mnesia:dirty_write(asim_simulation_rules, Model),

	%% Redirect
	SuccessUrl = asim_lib_web_url:get_current_extension(<<"index">>, State),
	asim_lib_web_redirect:url(<<"The specified record was succesfully created!">>, SuccessUrl, State).


%% Create for story from the specified model
create_form_story(#asim_simulation_rules{
	id = Id,
	name = Name,
	stop_cycle = StopCycle,
	population_count = PopulationCount,
	genome_size = GenomeSize,
	selection_count = SelectionCount,
	fitness_function = FitnessFunction,
	profit_redistribution_function = ProfitRedistributionFunction,
	c_cost_of_the_simulation = CCostOfTheSimulation,
	p_capital_multiplier = PCapitalMultiplier,
	x_taxation_percent = XTaxationPercent,
	m_gene_miner_energy = MGeneMinerEnergy,
	t_gene_trader_energy = TGeneTraderEnergy,
	h_gene_hoarder_ability = HGeneHoarderAbility,
	z_gene_thief_ability = ZGeneThiefAbility
}) ->

	[{fieldset, [{legend,<<"Simulation rules form">>},
		{children,
			[

				{field,
					[{children,
						[{element,
							[{label,<<"Id:">>},
								{for,<<"id">>},
								{size,4},
								{children,
									[{readonly_text, [{name, <<"id">>}, {value, Id}]}]
								}]
						}]
					}]
				},

				{field,
					[{children,
						[{element,
							[{label,<<"Name (chose any name you want to identify this simulation rules):">>},
								{for,<<"name">>},
								{size,8},
								{children,
									[{text, [{name, <<"name">>}, {value, Name}]}]
								}]
						}]
					}]
				},

				{field,
					[{children,
						[{element,
							[{label,<<"Stop cycle:">>},
								{for,<<"stop_cycle">>},
								{size,4},
								{children,
									[{text, [{name, <<"stop_cycle">>}, {value, StopCycle}]}]
								}]
						}]
					}]
				},

				{field,
					[{children,
						[{element,
							[{label,<<"Population count:">>},
								{for,<<"population_count">>},
								{size,4},
								{children,
									[{text, [{name, <<"population_count">>}, {value, PopulationCount}]}]
								}]
						}]
					}]
				},

				{field,
					[{children,
						[{element,
							[{label,<<"Genome size:">>},
								{for,<<"genome_size">>},
								{size,4},
								{children,
									[{text, [{name, <<"genome_size">>}, {value, GenomeSize}]}]
								}]
						}]
					}]
				},

				{field,
					[{children,
						[{element,
							[{label,<<"Selection count:">>},
								{for,<<"selection_count">>},
								{size,4},
								{children,
									[{text, [{name, <<"selection_count">>}, {value, SelectionCount}]}]
								}]
						}]
					}]
				},

				{field,
					[{children,
						[{element,
							[{label,<<"Fitness function:">>},
								{for,<<"fitness_function">>},
								{size,8},
								{children,
									[{text, [{name, <<"fitness_function">>}, {value, FitnessFunction}]}]
								}]
						}]
					}]
				},

				{field,
					[{children,
						[{element,
							[{label,<<"Profit redistribution function:">>},
								{for,<<"profit_redistribution_function">>},
								{size,4},
								{children,
									[{text, [{name, <<"profit_redistribution_function">>}, {value, ProfitRedistributionFunction}]}]
								}]
						},
						{element,
							[{label,<<"Cost of the simulation (C):">>},
								{for,<<"c_cost_of_the_simulation">>},
								{size,4},
								{children,
									[{text, [{name, <<"c_cost_of_the_simulation">>}, {value, CCostOfTheSimulation}]}]
								}]
						}]
					}]
				},

				{field,
					[{children,
						[{element,
							[{label,<<"Capital multiplier (P):">>},
								{for,<<"p_capital_multiplier">>},
								{size,8},
								{children,
									[{text, [{name, <<"p_capital_multiplier">>}, {value, PCapitalMultiplier}]}]
								}]
						}]
					}]
				},

				{field,
					[{children,
						[{element,
							[{label,<<"Taxation percent (X):">>},
								{for,<<"x_taxation_percent">>},
								{size,4},
								{children,
									[{text, [{name, <<"x_taxation_percent">>}, {value, XTaxationPercent}]}]
								}]
						}]
					}]
				},

				{field,
					[{children,
						[{element,
							[{label,<<"Miner gene energy (M):">>},
								{for,<<"m_gene_miner_energy">>},
								{size,4},
								{children,
									[{text, [{name, <<"m_gene_miner_energy">>}, {value, MGeneMinerEnergy}]}]
								}]
						},
							{element,
								[{label,<<"Trader gene energy (T):">>},
									{for,<<"t_gene_trader_energy">>},
									{size,4},
									{children,
										[{text, [{name, <<"t_gene_trader_energy">>}, {value, TGeneTraderEnergy}]}]
									}]
							},
							{element,
								[{label,<<"Hoarder gene ability (H):">>},
									{for,<<"h_gene_hoarder_ability">>},
									{size,4},
									{children,
										[{text, [{name, <<"h_gene_hoarder_ability">>}, {value, HGeneHoarderAbility}]}]
									}]
							},
							{element,
								[{label,<<"Thief gene ability (Z):">>},
									{for,<<"z_gene_thief_ability">>},
									{size,4},
									{children,
										[{text, [{name, <<"z_gene_thief_ability">>}, {value, ZGeneThiefAbility}]}]
									}]
							}]
					}]
				}

			]}]}].



