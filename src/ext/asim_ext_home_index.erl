%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(asim_ext_home_index).
-author("madalin").

-include("../include/asim.hrl").

%% API
-export([handle/1]).

%% @doc Handle home
handle(State = #asim_state{}) ->

	asim_lib_web_ob:render_and_reply(State).
