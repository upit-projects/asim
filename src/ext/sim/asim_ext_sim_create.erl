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

%% @doc Display simulation create form
handle(State = #asim_state{ob = Ob, extension_module_operation = Operation}) when Operation =/= <<"submit">> ->

	%% Create form
	{Form, NewState} = asim_lib_web_form:create([{action_operation, <<"submit">>}], State),

	%% Create a new timeuuid
	{_, Uuid} = asim_lib_utils_timeuuid:now(),

	%% Add story to the form
	StoryForm = asim_lib_web_form:add_story(create_form_story(#asim_simulation_specs{
		how_many    = 2,
		sim_id = <<>>,
		sim_template_id = <<>>
	}), Form),

	%% Render form
	{NewOb, _} = asim_lib_web_form:render(Ob, StoryForm),

	%% Render output buffer and reply
	asim_lib_web_ob:render_and_reply(NewState#asim_state{ob = NewOb});

%% @doc Creates the simulation
handle(State = #asim_state{request = Request}) ->

	HowMany  = asim_lib_http_request:always_get_integer(<<"how_many">>, Request, 2),
	SimId    = asim_lib_http_request:always_get_binary(<<"sim_id">>, Request, undefined),
	SimTemplateId  = asim_lib_http_request:always_get_binary(<<"sim_template_id">>, Request, undefined),

	%% Ask simulation manager server to create the simulation for us
	asim_srv_sim_manager:simulation_create(#asim_simulation_specs{
		how_many    = HowMany,
		sim_id = SimId,
		sim_template_id = SimTemplateId
	}),

	%% Redirect
	SuccessUrl = asim_lib_web_url:get_current_extension(<<"index">>, State),
	asim_lib_web_redirect:url(<<"The specified simulations where started! Redirecting you to display running simulations...">>, SuccessUrl, State).


%% Create for story from the specified model
create_form_story(#asim_simulation_specs{
	how_many    = HowMany,
	sim_id = MapId,
	sim_template_id = SimTemplateId
}) ->

	SimTemplates   = get_sim_template_options(),

	[{fieldset, [{legend,<<"Create new simulations form">>},
		{children,
			[

				{field,
					[{children,
						[{element,
							[{label,<<"How Many new simulation to create concurently:">>},
								{for,<<"how_many">>},
								{size,4},
								{children,
									[{text, [{name, <<"how_many">>}, {value, HowMany}]}]
								}]
						}]
					}]
				},

				{field,
					[{children,
						[{element,
							[{label,<<"Simulation template (create maps if empty):">>},
								{for,<<"sim_template_id">>},
								{size,4},
								{children,
									[{select, [{name, <<"sim_template_id">>}, {value, SimTemplateId}, {options, SimTemplates}]}]
								}]
						}]
					}]
				}

			]}]}].

%% @doc Returns available simulation templates options
get_sim_template_options() ->

	%% Load data from database
	Transaction = fun() -> mnesia:select(asim_map,[{'_',[],['$_']}]) end,
	DataRows = mnesia:activity(transaction, Transaction),
	[].
	%%get_map_options(DataRows, []).