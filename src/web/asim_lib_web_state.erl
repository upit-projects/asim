%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(asim_lib_web_state).
-author("madalin").

-include("../include/asim.hrl").

%% API
-export([keep_params_add/2]).

%% @doc Add parameters to keep to the specified state
keep_params_add(Params, State = #asim_state{extension_keep_params = ExtensionKeepParams}) when
    erlang:is_list(Params),
    erlang:is_list(ExtensionKeepParams) ->

    State#asim_state{extension_keep_params = lists:append(ExtensionKeepParams, Params)};
keep_params_add(Params, State = #asim_state{}) when
    erlang:is_list(Params) ->

    State#asim_state{extension_keep_params = Params}.

