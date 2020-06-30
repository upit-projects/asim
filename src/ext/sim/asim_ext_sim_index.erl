%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(asim_ext_sim_index).
-author("madalin").

-include("../include/asim.hrl").

%% API
-export([handle/1]).

%% @doc Handle home
handle(State = #asim_state{ob = Ob}) ->

	%% Add custom operations to database navigator
	NewOb = asim_lib_web_ob:add_toolbar_buttons([

		#asim_ob_toolbar_button{
			img    = asim_lib_web_url:get_asset_url(<<"/images/oxygen/64x64/places/user-trash.png">>),
			href   = asim_lib_web_url:get(<<"sim">>, <<"clear">>),
			name   = <<"Removed finished">>
		},

		#asim_ob_toolbar_button{
				img    = asim_lib_web_url:get_asset_url(<<"/images/oxygen/64x64/places/server-database.png">>),
				href   = asim_lib_web_url:get(<<"results">>, <<"index">>),
				name   = <<"Results database">>
			}

		], Ob),

	asim_lib_web_ob:render_and_reply(State#asim_state{ob = NewOb}).
