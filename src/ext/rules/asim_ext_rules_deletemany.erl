%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(asim_ext_rules_deletemany).
-author("madalin").

-include("../include/asim.hrl").

%% API
-export([handle/1]).

handle(State = #asim_state{ob = Ob}) ->

	%% Get selected keys
	SelectedKeys = asim_lib_web_table_nav:request_get_selected_keys(State),

	%% Delete
	delete(SelectedKeys, State).

%% @doc Iterate and delete selected keys
delete([], State) ->

	CurrentExtensionUrl = asim_lib_web_url:get_current_extension(<<"index">>, State),
	asim_lib_web_redirect:url(<<"All selected records where succesfully deleted!">>, CurrentExtensionUrl, State);

delete([H|T], State) ->

	mnesia:dirty_delete(asim_simulation_rules, H),
	delete(T, State).
