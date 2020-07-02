%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(asim_ext_rules_update).
-author("madalin").

-include("../include/asim.hrl").

%% API
-export([handle/1]).

handle(State = #asim_state{extension = Extension, extension_module_operation = Operation}) ->

	%% Error msg and route
	ErrorMsg = <<"Invalid id! The specified record no longer exist in our database (maybe deleted?)">>,
	ErrorRoute = #asim_route{
		extension = Extension
	},

	%% Get id
	Id = asim_lib_web_request:get_selected_key(ErrorMsg, ErrorRoute, <<"id">>, State),

	%% Submit or display?
	Result = case Operation of
		<<"submit">> -> handle_submit(Id, State);
		_ ->  handle_display(Id, State)
	end,

	case Result of
		{ok, NewState} ->

			%% Render output buffer and reply
			asim_lib_web_ob:render_and_reply(NewState);

		_ ->

			%% Redirect
			asim_lib_web_redirect:route(ErrorMsg, ErrorRoute, State)

	end.

handle_display(Id, State = #asim_state{ob = Ob}) ->

	%% Load from database
	Result = mnesia:dirty_read(asim_simulation_rules, Id),
	case Result of
		[Model] ->

			%% Create form
			{Form, NewState} = asim_lib_web_form:create([{action_operation, <<"submit">>}], State),

			%% Add story to the form
			StoryForm = asim_lib_web_form:add_story(asim_ext_rules_create:create_form_story(Model), Form),

			%% Render form
			{NewOb, _} = asim_lib_web_form:render(Ob, StoryForm),

			%% OK
			{ok, NewState#asim_state{ob = NewOb}};

		_ -> error

	end.

handle_submit(Id, State = #asim_state{ob = Ob, request = Request}) ->

	%% Load tank from database
	Result = mnesia:dirty_read(asim_simulation_rules, Id),
	case Result of

		[Model] ->

			%% Read all information
			NewModel = Model#asim_simulation_rules{
				id      = Id,
				name = asim_lib_http_request:always_get_trimed_non_empty_binary(<<"name">>, Request, Model#asim_simulation_rules.name),
				stop_cycle = asim_lib_http_request:always_get_integer(<<"stop_cycle">>, Request, Model#asim_simulation_rules.stop_cycle),
				population_count = asim_lib_http_request:always_get_integer(<<"population_count">>, Request, Model#asim_simulation_rules.population_count),
				genome_size = asim_lib_http_request:always_get_integer(<<"genome_size">>, Request, Model#asim_simulation_rules.genome_size),
				selection_count = asim_lib_http_request:always_get_integer(<<"selection_count">>, Request, Model#asim_simulation_rules.selection_count),
				fitness_function = asim_lib_http_request:always_get_trimed_non_empty_binary(<<"fitness_function">>, Request, Model#asim_simulation_rules.fitness_function),
				profit_redistribution_function = asim_lib_http_request:always_get_trimed_non_empty_binary(<<"profit_redistribution_function">>, Request, Model#asim_simulation_rules.profit_redistribution_function),
				c_cost_of_the_simulation = asim_lib_http_request:always_get_float(<<"c_cost_of_the_simulation">>, Request, Model#asim_simulation_rules.c_cost_of_the_simulation),
				p_capital_multiplier = asim_lib_http_request:always_get_float(<<"p_capital_multiplier">>, Request, Model#asim_simulation_rules.p_capital_multiplier),
				x_taxation_percent = asim_lib_http_request:always_get_float(<<"x_taxation_percent">>, Request, Model#asim_simulation_rules.x_taxation_percent),
				m_gene_miner_energy = asim_lib_http_request:always_get_integer(<<"m_gene_miner_energy">>, Request, Model#asim_simulation_rules.m_gene_miner_energy),
				t_gene_trader_energy = asim_lib_http_request:always_get_integer(<<"t_gene_trader_energy">>, Request, Model#asim_simulation_rules.t_gene_trader_energy),
				h_gene_hoarder_ability = asim_lib_http_request:always_get_float(<<"h_gene_hoarder_ability">>, Request, Model#asim_simulation_rules.h_gene_hoarder_ability),
				z_gene_thief_ability = asim_lib_http_request:always_get_float(<<"z_gene_thief_ability">>, Request, Model#asim_simulation_rules.z_gene_thief_ability)
			},

			%% Insert record into database
			mnesia:dirty_write(asim_simulation_rules, NewModel),

			%% Redirect
			SuccessUrl = asim_lib_web_url:get_current_extension(<<"index">>, State),
			asim_lib_web_redirect:url(<<"The specified record was succesfully updated!">>, SuccessUrl, State);

		_ -> error

	end.