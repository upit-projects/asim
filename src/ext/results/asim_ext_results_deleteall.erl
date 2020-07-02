%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(asim_ext_results_deleteall).
-author("madalin").

-include("../include/asim.hrl").

%% API
-export([handle/1]).

handle(State = #asim_state{}) ->

	%% Delete
	mnesia:clear_table(asim_simulation),

	CurrentExtensionUrl = asim_lib_web_url:get_current_extension(<<"index">>, State),
	asim_lib_web_redirect:url(<<"All records where succesfully deleted!">>, CurrentExtensionUrl, State).

