%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(asim_lib_web_table_nav).
-author("madalin").

-include("../include/asim.hrl").

%% API
-export([create/2]).
-export([render/2]).
-export([request_get_selected_keys/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% create
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Create Cassandra database navigator
create(Options, State = #asim_state{request = Request}) ->

	%% Table and record must be present
	Table = asim_lib_utils_proplists:guard_get_atom(table, Options),
	Record= asim_lib_utils_proplists:guard_get_atom(record, Options),

	%% Get columns
	Columns   = proplists:get_value(columns, Options),
	OptionMsg = proplists:get_value(option_msg, Options, [
		{msg_empty_nav,"The table is empty..."},
		{msg_select_one,"Please select one element from the table!"},
		{msg_select_at_least_one,"Please select at least one element from the table!"},
		{msg_delete_one_confirm,"Are you sure you want to delete selected row?"},
		{msg_delete_many_confirm,"Are you sure you want to delete selected rows?"},
		{msg_delete_one_success,"The selected row was deleted!"},
		{msg_delete_many_success,"All selected rows were succesfully deleted!"},
		{msg_update_one_success,"The data you submited was succesfully updated!"},
		{msg_update_one_ttl_success,"The ttl of the selected row was succesfully updated!"}
	]),
	Key       = proplists:get_value(key, Options),

	%% Always get an id
	Id = asim_lib_web_html_id:is_valid_or_new(proplists:get_value(id, Options, undefined)),

	%% Always get data select type, keys
	Operations = proplists:get_value(operations, Options, [{create, []}, {update, []}, {delete_many, []}]),
	DataRows   = proplists:get_value(data_rows, Options, undefined),


	%% Get option autogeneration of filter
	OptionAutoKeyFilter = proplists:get_value(option_auto_key_filter, Options, false),

	%% Create navigator form if no custom form was setup
	{Form, FormState} = case proplists:get_value(form, Options, undefined) of
		                    undefined -> asim_lib_web_form:create([{method, <<"get">>}], State);
		                    CustomForm -> {CustomForm, State}
	                    end,

	%% Create a new navigator
	Nav = #asim_table_nav{
		id                      = Id,
		form                    = Form,
		table                   = Table,
		record                  = Record,
		columns                 = Columns,
		key                     = Key,
		option_auto_key_filter  = OptionAutoKeyFilter,
		operations              = Operations,
		data_rows               = DataRows,
		option_msg              = OptionMsg,
		request                 = Request
	},

	%% Nav is ready to use. Return.
	{Nav, FormState}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RENDER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%=============================================
%% render
%%=============================================

%% @doc Render database navigator
render(Ob, Nav = #asim_table_nav{
	form        = Form,
	data_rows   = DataRows}) when erlang:is_list(DataRows) ->

	%% Begin form
	{ObBeginForm, Form1}            = asim_lib_web_form:render_begin_form(Ob, Form),

	ObDbNav                         = asim_lib_web_ob:add_story(<<"\n<div class=\"dbnav\">">>, ObBeginForm),

	{Ob4, Nav1}                     = render_header(ObDbNav, Nav),
	{Ob5, Nav2}                     = render_data(Ob4, Nav1),
	{Ob6, Nav3}                     = render_footer(Ob5, Nav2),

	Ob7                             = asim_lib_web_ob:add_story(<<"\n</div>">>, Ob6),

	%% End form
	{ObPreToolbar, Form2}           = asim_lib_web_form:render_end_form(Ob7, Form1),

	%% Render toolbar buttons
	{ObToolbar, NavToolbar}         = render_toolbar_buttons(ObPreToolbar, Nav3),

	%% Return
	{ObToolbar, NavToolbar};

%% @doc Render database navigator
render(Ob, Nav = #asim_table_nav{
	table       = Table,
	data_rows   = undefined}) ->

	%% Load data from database
	Transaction = fun() -> mnesia:select(Table,[{'_',[],['$_']}]) end,
	DataRows = mnesia:activity(transaction, Transaction),

	%%io:format("~n~n~nData rows: ~p~n~n", [DataRows]),

	%% Call render again
	render(Ob, Nav#asim_table_nav{data_rows = DataRows}).

%%=============================================
%% render_header
%%=============================================

%% @doc Render navigator header
render_header(Ob, Nav) ->

	Ob1 = asim_lib_web_ob:add_story(<<"\n<table class=\"data\">">>, Ob),
	render_header_head(Ob1, Nav).

%% @doc Render navigator header head
render_header_head(Ob, Nav = #asim_table_nav{
	columns = []
	}) -> {Ob, Nav};

render_header_head(Ob, Nav  = #asim_table_nav{
	id        = Id,
	columns   = Columns
}) ->

	Ob1 = asim_lib_web_ob:add_story([
		<<"<tr class=\"head\">">>,
		<<"<th class=\"selectall\"><div class=\"fakelink\" id=\"">>,
		Id,
		<<"-selectall\">x</div><div class=\"fakelink\" id=\"">>,
		Id,
		<<"-deselectall\">-</div></th>">>
	], Ob),

	Ob2 = asim_lib_web_ob:add_jsready([
		<<"\n$('#">>, Id,
		<<"-selectall').click(function() { $('.">>,
		Id,
		<<"_srid').attr('checked','checked'); });">>,
		<<"\n$('#">>,
		Id,
		<<"-deselectall').click(function() { $('.">>,
		Id,
		<<"_srid').removeAttr('checked'); });">>
	], Ob1),

	%% Iterate
	{Ob3, Nav1} = render_header_head_iterate(Columns, Ob2, Nav),

	%% Close tag
	{asim_lib_web_ob:add_story(<<"</tr>">>, Ob3), Nav1}.

render_header_head_iterate([ColumnName|T], Ob, Nav) ->

	Ob1 = asim_lib_web_ob:add_story([<<"<th>">>, asim_lib_utils_type_conv:to_binary(ColumnName), <<"</th>">>], Ob),
	render_header_head_iterate(T, Ob1, Nav);

render_header_head_iterate([], Ob, Nav) -> {Ob, Nav}.

%%=============================================
%% render_footer
%%=============================================

%% @doc Render navigator footer
render_footer(Ob, Nav) ->

	{asim_lib_web_ob:add_story(<<"</table>">>, Ob), Nav}.

%%=============================================
%% render_data
%%=============================================

%% @doc Render database navigator data
render_data(Ob, Nav = #asim_table_nav{
	data_rows = DataRows
}) ->

	{render_data_row(DataRows, 1, Ob, Nav), Nav}.

render_data_row([H|T], RowNumber, Ob, Nav = #asim_table_nav{
	id = Id,
	record = Record,
	key = Key
}) ->

	ProplistH   = render_data_row_convert_to_proplist(H, Record),
	SridValue   = proplists:get_value(Key, ProplistH),
	FirstRowOb  = asim_lib_web_ob:add_story([
		<<"\n<tr class=\"row\" id=\"">>, Id, <<"_row_1\">">>,
		<<"<td><input type=\"checkbox\" class=\"xsrid\" id=\"xsrid_">>,
		erlang:integer_to_binary(RowNumber),
		<<"\" name=\"xsrid_">>,
		erlang:integer_to_binary(RowNumber),
		<<"\" value=\"">>, asim_lib_web_htmlentities:encode(SridValue), <<"\" /></td>">>
	], Ob),

	RowOb       = render_data_row_columns(ProplistH, ProplistH, RowNumber, FirstRowOb, Nav),
	EndRowOb    = asim_lib_web_ob:add_story(<<"\n</tr>">>, RowOb),

	render_data_row(T, RowNumber+1, EndRowOb, Nav);

render_data_row([], _RowNumber, Ob, _Nav) -> Ob.

render_data_row_columns([{Key, Value}|T], Row, RowNumber, Ob, Nav) ->

	ColumnOb = asim_lib_web_ob:add_story([
		<<"\n<td>">>,
		asim_lib_web_table_nav_renderer:to_html(Key, Value, Row),
		<<"</td>">>
	], Ob),

	render_data_row_columns(T, Row, RowNumber, ColumnOb, Nav);
render_data_row_columns([], _Row, _RowNumber, Ob, _Nav) -> Ob.

render_data_row_convert_to_proplist(Row, asim_simulation) -> lists:zip(record_info(fields, asim_simulation), tl(tuple_to_list(Row))).

%%=============================================
%% render_toolbar
%%=============================================

%% @doc Render navigator toolbar buttons
render_toolbar_buttons(Ob, Nav = #asim_table_nav{
	operations = Operations
}) ->

	{render_toolbar_buttons_iterate(Operations, Ob, Nav), Nav}.

render_toolbar_buttons_iterate([{create, Options}|T], Ob, Nav) ->

	Button = #asim_ob_toolbar_button{
		id                  = asim_lib_web_html_id:new(),
		href                = <<"#">>,
		img                 = asim_lib_web_url:get_asset_url(<<"/images/toolbar/create.png">>),
		name                = <<"Create new">>,
		target              = undefined,
		js_ready            = undefined,
		js_confirm          = false
	},

	FullOptions = [
		{action_module, <<"create">>},
		{selection, 0},
		{button, Button}
	],

	FinalButton = render_toolbar_button_build_js(FullOptions, Nav),
	NewOb       = asim_lib_web_ob:add_toolbar_button(FinalButton, Ob),

	render_toolbar_buttons_iterate(T, NewOb, Nav);

render_toolbar_buttons_iterate([{update, Options}|T], Ob, Nav) ->

	Button = #asim_ob_toolbar_button{
		id                  = asim_lib_web_html_id:new(),
		href                = <<"#">>,
		img                 = asim_lib_web_url:get_asset_url(<<"/images/toolbar/edit.png">>),
		name                = <<"Edit">>,
		target              = undefined,
		js_ready            = undefined,
		js_confirm          = false
	},

	FullOptions = [
		{action_module, <<"update">>},
		{selection, 1},
		{msg_select_one, proplists:get_value(msg_select_one, Options, utils_get_msg(msg_select_one, Nav))},
		{button, Button}
	],

	FinalButton = render_toolbar_button_build_js(FullOptions, Nav),
	NewOb       = asim_lib_web_ob:add_toolbar_button(FinalButton, Ob),

	render_toolbar_buttons_iterate(T, NewOb, Nav);

render_toolbar_buttons_iterate([{delete_many, Options}|T], Ob, Nav) ->

	Button = #asim_ob_toolbar_button{
		id                  = asim_lib_web_html_id:new(),
		href                = <<"#">>,
		img                 = asim_lib_web_url:get_asset_url(<<"/images/toolbar/delete.png">>),
		name                = <<"Delete">>,
		target              = undefined,
		js_ready            = undefined,
		js_confirm          = true,
		js_confirm_question = utils_get_msg(msg_delete_many_confirm, Nav)
	},

	FullOptions = [
		{action_module, <<"deletemany">>},
		{selection, 2},
		{msg_select_at_least_one, proplists:get_value(msg_select_at_least_one, Options, utils_get_msg(msg_select_at_least_one, Nav))},
		{button, Button}
	],

	FinalButton = render_toolbar_button_build_js(FullOptions, Nav),
	NewOb       = asim_lib_web_ob:add_toolbar_button(FinalButton, Ob),

	render_toolbar_buttons_iterate(T, NewOb, Nav);

render_toolbar_buttons_iterate([{_, Options}|T], Ob, Nav) ->

	FinalButton = render_toolbar_button_build_js(Options, Nav),
	NewOb       = asim_lib_web_ob:add_toolbar_button(FinalButton, Ob),

	render_toolbar_buttons_iterate(T, NewOb, Nav);

%% End iteration
render_toolbar_buttons_iterate([], Ob, Nav) -> Ob.

render_toolbar_button_build_js(Options, Nav = #asim_table_nav{
	id      = Id,
	form    = Form = #asim_html_form{
		hidden_fields = HiddenFields
	}}) ->

	Selection                = proplists:get_value(selection, Options, 0),
	Button                   = proplists:get_value(button, Options, undefined),
	Extension                = proplists:get_value(action_extension, Options,  proplists:get_value(<<"xae">>, HiddenFields, <<>>)),
	ExtensionModule          = proplists:get_value(action_module, Options,  proplists:get_value(<<"xaem">>, HiddenFields, <<>>)),
	ExtensionModuleOperation = proplists:get_value(action_operation, Options,  proplists:get_value(<<"xaemo">>, HiddenFields, <<>>)),


	%% Build java script for the button
	Js = case erlang:is_record(Button, asim_ob_toolbar_button) of

		     true ->


			     TempCount                   = erlang:iolist_to_binary([<<"\nvar tempAL = $(\"input.xsrid:checked\").length;">>]),
			     {TempFormSubmit, _NewForm}  = asim_lib_web_form:get_js_for_submit([
				     {<<"xae">>, Extension},
				     {<<"xaem">>, ExtensionModule},
				     {<<"xaemo">>, ExtensionModuleOperation}
			     ], Form),

			     case Selection of

				     0 -> erlang:iolist_to_binary([TempFormSubmit]);
				     1 ->

					     SelectOneMsgOption = proplists:get_value(msg_select_one, Options, utils_get_msg(msg_select_one, Nav)),
					     erlang:iolist_to_binary([
						     TempCount,
						     <<"\nif (tempAL != 1)\n{\ngem_alert(\"">>, SelectOneMsgOption, <<"\");\nreturn;\n}">>,
						     TempFormSubmit
					     ]);

				     _ ->

					     SelectAtLeastOneMsgOption = proplists:get_value(msg_select_at_least_one, Options, utils_get_msg(msg_select_at_least_one, Nav)),
					     erlang:iolist_to_binary([
						     TempCount,
						     <<"\nif (tempAL < 1)\n{\ngem_alert(\"">>, SelectAtLeastOneMsgOption, <<"\");\nreturn;\n}">>,
						     TempFormSubmit
					     ])

			     end

	     end,

	%% Returns the new button
	Button#asim_ob_toolbar_button{js_ready = Js}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% request_get...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns selected keys from any cassandra database navigator
request_get_selected_keys(#asim_state{request = Request}) ->

	Max = asim_lib_http_request:always_get_integer(<<"xsrid_max">>, Request, 10000),
	case Max > 10000 of
		true -> request_get_selected_keys_iterate(10000, Request);
		_ -> request_get_selected_keys_iterate(Max, Request)
	end.

request_get_selected_keys_iterate(Max, Request) -> request_get_selected_keys_iterate(1, Max, Request, []).
request_get_selected_keys_iterate(Count, Max, _Request, Acum) when Count > Max -> Acum;
request_get_selected_keys_iterate(Count, Max, Request, Acum) ->

	Srid        = erlang:iolist_to_binary([<<"xsrid_">>, erlang:integer_to_binary(Count)]),
	NewCount    = Count+1,
	Value       = asim_lib_http_request:always_get_binary(Srid, Request, invalid),
	case Value of
		invalid -> request_get_selected_keys_iterate(NewCount, Max, Request, Acum);
		_ ->
			NewAcum = lists:append([Acum, [Value]]),
			request_get_selected_keys_iterate(NewCount, Max, Request, NewAcum)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTILS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns the specified navigator message or <<"undefined">> if message can not be found.
utils_get_msg(MsgKey, #asim_table_nav{option_msg = Msgs}) ->
	proplists:get_value(MsgKey, Msgs, <<"undefined">>).
