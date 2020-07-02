%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(asim_lib_web_form).
-author("madalin").

-include("../include/asim.hrl").

%% API
-export([create/2]).

-export([render/2]).
-export([render_begin_form/2]).
-export([render_end_form/2]).

-export([get_js_for_submit/1]).
-export([get_js_for_submit/2]).

-export([add_story/2]).
-export([add_story_specs/3]).
-export([add_hidden_field/3]).
-export([add_hidden_fields/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% create
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create(Options, State = #asim_state{
	extension                   = CurrentExtension,
	extension_module            = CurrentExtensionModule,
	extension_module_operation  = CurrentExtensionModuleOperation,
	extension_keep_params       = ExtensionKeepParams
}) ->

	Id            = asim_lib_web_html_id:is_valid_or_new(proplists:get_value(id, Options, undefined)),
	CustomAction  = proplists:get_value(action, Options, undefined),
	HiddenFields  = proplists:get_value(hidden_fields, Options, []),
	{Action, FinalHiddenFields} = case CustomAction of

		                              undefined ->

			                              ActionExtension     = proplists:get_value(action_extension, Options, CurrentExtension),
			                              ActionModule        = proplists:get_value(action_module, Options, CurrentExtensionModule),
			                              ActionOperation     = proplists:get_value(action_operation, Options, CurrentExtensionModuleOperation),

			                              StartHiddenFields   = case ActionExtension of
				                                                    CurrentExtension -> lists:append(ExtensionKeepParams, HiddenFields);
				                                                    _ -> HiddenFields
			                                                    end,

			                              HiddenFields1       = proplists:delete(<<"xae">>, StartHiddenFields),
			                              HiddenFields2       = proplists:delete(<<"xaem">>, HiddenFields1),
			                              HiddenFields3       = proplists:delete(<<"xaemo">>, HiddenFields2),

			                              HiddenFields4       = lists:append([HiddenFields3, [{<<"xae">>, ActionExtension}], [{<<"xaem">>, ActionModule}], [{<<"xaemo">>, ActionOperation}]]),

			                              {asim_lib_web_url:get(ActionExtension, ActionModule, ActionOperation), HiddenFields4};

		                              _ -> {CustomAction, HiddenFields}

	                              end,

	{#asim_html_form{

		method              = proplists:get_value(method, Options, <<"post">>),
		id                  = Id,
		name                = proplists:get_value(name, Options, Id),
		class               = proplists:get_value(class, Options, <<"quickform">>),
		div_class           = proplists:get_value(div_class, Options, <<"form-container">>),
		encoding            = proplists:get_value(encoding, Options, <<"application/x-www-form-urlencoded">>),
		hidden_fields       = FinalHiddenFields,
		action              = Action,
		toolbar_buttons     = proplists:get_value(toolbar_buttons, Options, [{submit, []}])

	}, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% render
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Render common form
render(Ob, Form = #asim_html_form{}) ->

	{BeginOb, BeginForm}    = render_begin_form(Ob, Form),
	{ContentOb, ContentForm}= render_content(BeginOb, BeginForm),
	{EndOb, EndForm}        = render_end_form(ContentOb, ContentForm),

	render_toolbar_buttons(EndOb, EndForm).

%% @doc Render begin form
render_begin_form(Ob, Form = #asim_html_form{
	div_class   = DivClass,
	class       = Class,
	id          = Id,
	name        = Name,
	method      = Method,
	encoding    = Encoding,
	action      = Action
}) ->

	Ob1     = asim_lib_web_ob:add_story([
		"\n<div class=\"",
		DivClass,
		"\">",
		"\n<form class=\"",
		Class,
		"\" id=\"",
		Id,
		"\" name=\"",
		Name,
		"\" method=\"",
		Method,
		"\" enctype=\"",
		Encoding,
		"\"",
		" accept-charset=\"utf-8\"",
		" action=\"",
		Action,
		"\">"
	], Ob),

	{Ob1, Form}.

%%=============================================
%% render_content
%%=============================================

render_content(Ob, Form = #asim_html_form{
	content = Content
}) ->

	{asim_lib_web_ob:add_story(Content, Ob), Form}.

%%=============================================
%% render_end_form
%%=============================================

%% @doc Render end form
render_end_form(Ob, Form = #asim_html_form{
	hidden_fields = HiddenFields
}) ->

	Ob1 = render_end_form_hidden_fields(HiddenFields, Ob),
	Ob2 = asim_lib_web_ob:add_story("\n</form>\n</div>", Ob1),

	{Ob2, Form}.

%% @doc Render form hidden field
render_end_form_hidden_fields([{Key, Value}|T], Ob) ->

	Ob1 = asim_lib_web_ob:add_story([
		"\n<input type=\"hidden\" name=\"",
		Key,
		"\" value=\"",
		asim_lib_web_htmlentities:encode(Value),
		"\" />"], Ob),
	render_end_form_hidden_fields(T, Ob1);

render_end_form_hidden_fields([], Ob) -> Ob.

%%=============================================
%% render_toolbar_buttons
%%=============================================

render_toolbar_buttons(Ob, Form = #asim_html_form{
	toolbar_buttons = ToolbarButtons
}) ->

	{render_toolbar_buttons_iterate(ToolbarButtons, Ob, Form), Form}.

render_toolbar_buttons_iterate([{clear, _Options}|T], Ob, Form = #asim_html_form{id = Id}) ->

	Button = #asim_ob_toolbar_button{
		id                  = asim_lib_web_html_id:new(),
		href                = <<"#">>,
		img                 = asim_lib_web_url:get_asset_url(<<"/images/toolbar/clear.png">>),
		name                = <<"Clear">>,
		target              = undefined,
		js_ready            = erlang:iolist_to_binary([<<"$('#">>, Id, <<"')[0].reset();">>]),
		js_confirm          = false
	},

	NewOb       = asim_lib_web_ob:add_toolbar_button(Button, Ob),
	render_toolbar_buttons_iterate(T, NewOb, Form);

render_toolbar_buttons_iterate([{refresh, _Options}|T], Ob, Form) ->

	Button = #asim_ob_toolbar_button{
		id                  = asim_lib_web_html_id:new(),
		href                = <<"#">>,
		img                 = asim_lib_web_url:get_asset_url(<<"/images/toolbar/refresh.png">>),
		name                = <<"Refresh">>,
		target              = undefined,
		js_ready            = <<"location.reload(true);">>,
		js_confirm          = false
	},

	NewOb       = asim_lib_web_ob:add_toolbar_button(Button, Ob),
	render_toolbar_buttons_iterate(T, NewOb, Form);

render_toolbar_buttons_iterate([{submit, _Options}|T], Ob, Form = #asim_html_form{id = Id}) ->

	Button = #asim_ob_toolbar_button{
		id                  = asim_lib_web_html_id:new(),
		href                = <<"#">>,
		img                 = asim_lib_web_url:get_asset_url(<<"/images/toolbar/submit.png">>),
		name                = <<"Submit">>,
		target              = undefined,
		js_ready            = erlang:iolist_to_binary([<<"\n$.blockUI({ message: '<h1>We are processing your request. Please be patient.</h1>' });\n$('#">>, Id, <<"').submit();">>]),
		js_confirm          = false
	},

	NewOb       = asim_lib_web_ob:add_toolbar_button(Button, Ob),
	render_toolbar_buttons_iterate(T, NewOb, Form);

%% End iteration
render_toolbar_buttons_iterate([], Ob, _Form) -> Ob.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns javascript for submitting this form
get_js_for_submit(Form = #asim_html_form{id = Id}) ->
	{erlang:iolist_to_binary([<<"\n$(\"#">>, asim_lib_web_htmlentities:encode(Id), <<"\").submit();">>]), Form}.
get_js_for_submit(Params, Form) -> get_js_for_submit(Params, <<>>, Form).
get_js_for_submit([{Key, Value}|T], Acum, Form = #asim_html_form{id = Id}) ->
	NewAcum = erlang:iolist_to_binary([Acum, <<"\n$(\"#">>, Id, <<" input[name='">>, Key, <<"']\").val(\"">>, Value, <<"\");">>]),
	get_js_for_submit(T, NewAcum, Form);
get_js_for_submit([], Acum, Form = #asim_html_form{id = Id}) ->
	{erlang:iolist_to_binary([Acum, <<"\n$(\"#">>, Id, <<"\").submit();">>]), Form}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% add_story_...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%=============================================
%% add_story
%%=============================================

%% @doc Add story to the specified form
add_story([], Form) -> Form;
add_story(Story = [{fieldset, Data}|T], Form) ->

	Legend          = proplists:get_value(legend, Data, <<>>),
	Children        = proplists:get_value(children, Data, []),

	BeginForm       = add_story_binary([<<"\n<fieldset>\n<legend>">>, Legend, <<"</legend>">>], Form),
	ChildrenForm    = add_story_field(Children, BeginForm),
	EndForm         = add_story_binary(<<"\n</fieldset>">>, ChildrenForm),
	add_story(T, EndForm);

add_story([{custom, Data}|T], Form) ->

	NewForm = add_story_binary(Data, Form),
	add_story(T, NewForm).

%% @doc Add the specified binary to the specified form
add_story_binary(IoList, Form = #asim_html_form{content = Story}) ->
	Form#asim_html_form{content = erlang:iolist_to_binary([Story, IoList])}.

%%=============================================
%% add_story_field
%%=============================================

%% @doc Add story filedset children (field)
add_story_field([], Form) -> Form;
add_story_field([{field, Data}|T], Form) ->

	Children        = proplists:get_value(children, Data, []),

	BeginForm       = add_story_binary([<<"\n<div class=\"form-field\">">>], Form),
	ChildrenForm    = add_story_field_element(Children, BeginForm),
	EndForm         = add_story_binary(<<"\n</div>">>, ChildrenForm),
	add_story_field(T, EndForm);

add_story_field([{custom, Data}|T], Form) ->

	NewForm = add_story_binary(Data, Form),
	add_story_field(T, NewForm).

%%=============================================
%% add_story_field_element
%%=============================================

%% @doc Add story field element
add_story_field_element([], Form) -> Form;
add_story_field_element([{element, Data}|T], Form) ->

	Label           = proplists:get_value(label, Data, <<>>),
	For             = proplists:get_value(for, Data, undefined),
	Size            = proplists:get_value(size, Data, 4),
	Children        = proplists:get_value(children, Data, []),


	ForBin = case erlang:is_binary(For) of
		         true -> erlang:iolist_to_binary([<<" for=\"">>, For, <<"\"">>]);
		         _ -> <<>>
	         end,

	BeginForm       = add_story_binary([
		<<"\n<div class=\"element-">>,
		erlang:integer_to_binary(Size),
		<<"\">">>,
		<<"\n<label">>,
		ForBin,
		<<">">>,
		asim_lib_web_htmlentities:encode(Label),
		<<"</label>">>
	], Form),

	ChildrenForm    = add_story_inputs(Children, BeginForm),
	EndForm         = add_story_binary(<<"\n</div>">>, ChildrenForm),

	add_story_field_element(T, EndForm);

add_story_field_element([{custom, Data}|T], Form) ->

	NewForm = add_story_binary(Data, Form),
	add_story_field_element(T, NewForm).

%%=============================================
%% add_story_inputs
%%=============================================

%% @doc Iterate trough all inputs and add them
add_story_inputs([], Form) -> Form;
add_story_inputs([H|T], Form) ->
	NewForm = add_story_input(H, Form),
	add_story_inputs(T, NewForm).

%% @doc Add text
add_story_input({text, Data}, Form) ->

	Name        = proplists:get_value(name, Data, <<"">>),
	Value       = add_story_input_get_value(Data, <<"">>),
	MaxLength   = proplists:get_value(maxlength, Data, 255),
	ReadOnly    = case proplists:get_value(readonly, Data, undefined) of
		              true -> <<"readonly">>;
		              _ -> <<"">>
	              end,

	add_story_binary([
		<<"\n<span><input type=\"text\" class=\"input-text\" id=\"">>,
		Name,
		<<"\" name=\"">>,
		Name,
		<<"\" value=\"">>,
		asim_lib_web_htmlentities:encode(Value),
		<<"\" maxlength=\"">>,
		erlang:integer_to_binary(MaxLength),
		<<"\" ">>, ReadOnly, <<"/></span>">>
	], Form);

%% @doc Add readonly text
add_story_input({readonly_text, Data}, Form) ->

	Name        = proplists:get_value(name, Data, <<>>),
	Value       = add_story_input_get_value(Data, <<>>),
	MaxLength   = proplists:get_value(maxlength, Data, 255),

	add_story_binary([
		<<"\n<span><input type=\"text\" class=\"input-text\" id=\"">>,
		Name,
		<<"\" name=\"">>,
		Name,
		<<"\" value=\"">>,
		asim_lib_web_htmlentities:encode(Value),
		<<"\" maxlength=\"">>,
		erlang:integer_to_binary(MaxLength),
		<<"\" \" readonly=\"readonly\" /></span>">>
	], Form);

%% @doc Add file
add_story_input({file, Data}, Form) ->

	Name        = proplists:get_value(name, Data, <<>>),
	add_story_binary([
		<<"\n<span><input type=\"file\" class=\"input-file\" id=\"">>,
		Name,
		<<"\" name=\"">>,
		Name,
		<<"\"/></span>">>
	], Form);

%% @doc Add password
add_story_input({password, Data}, Form) ->

	Name        = proplists:get_value(name, Data, <<>>),
	Value       = add_story_input_get_value(Data, <<>>),
	MaxLength   = proplists:get_value(maxlength, Data, 255),

	ReadOnly    = case proplists:get_value(readonly, Data, undefined) of
		              true -> <<"readonly">>;
		              _ -> <<"">>
	              end,

	add_story_binary([
		<<"\n<span><input type=\"password\" class=\"input-text\" id=\"">>,
		Name,
		<<"\" name=\"">>,
		Name,
		<<"\" value=\"">>,
		asim_lib_web_htmlentities:encode(Value),
		<<"\" maxlength=\"">>,
		erlang:integer_to_binary(MaxLength),
		<<"\" ">>, ReadOnly, <<"/></span>">>
	], Form);

%% @doc Add textarea
add_story_input({textarea, Data}, Form) ->

	Name        = proplists:get_value(name, Data, <<"">>),
	Value       = add_story_input_get_value(Data, <<"">>),
	Rows        = proplists:get_value(rows, Data, 30),
	Cols        = proplists:get_value(cols, Data, 30),

	ReadOnly    = case proplists:get_value(readonly, Data, undefined) of
		              true -> <<" readonly">>;
		              _ -> <<"">>
	              end,

	%% Replace any \r\n with \r
	NewValue = asim_lib_utils_unicode:replace_string("\r\n", "\r", unicode:characters_to_list(asim_lib_utils_type_conv:to_list(Value), utf8)),

	%% Add story
	add_story_binary([
		<<"\n<span><textarea id=\"">>,
		Name,
		<<"\" name=\"">>,
		Name,
		<<"\" rows=\"">>,
		erlang:integer_to_binary(Rows),
		<<"\" cols=\"">>,
		erlang:integer_to_binary(Cols),
		<<"\"">>, ReadOnly, <<">">>,
		asim_lib_web_htmlentities:encode(NewValue),
		<<"</textarea></span>">>
	], Form);

%% @doc Add boolean
add_story_input({boolean, Data}, Form) ->

	Name        = proplists:get_value(name, Data, <<>>),
	Value       = add_story_input_get_value(Data, <<>>),
	YesMsg      = proplists:get_value(yesmsg, Data, <<"Yes">>),
	NoMsg       = proplists:get_value(nomsg, Data, <<"No">>),

	ReadOnly    = case proplists:get_value(readonly, Data, undefined) of
		              true -> <<" readonly">>;
		              _ -> <<"">>
	              end,

	case Value of

		<<"1">> ->

			add_story_binary([
				<<"\n<select id=\"">>,
				Name,
				<<"\" name=\"">>,
				Name,
				<<"\"">>, ReadOnly, <<">">>,
				<<"\n<option value=\"1\" selected=\"selected\">">>,
				YesMsg,
				<<"</option>\n<option value=\"0\">">>,
				NoMsg,
				<<"</option></select>">>
			], Form);

		_ ->

			add_story_binary([
				<<"\n<select id=\"">>,
				Name,
				<<"\" name=\"">>,
				Name,
				<<"\"">>, ReadOnly, <<">">>,
				<<"\n<option value=\"1\">">>,
				YesMsg,
				<<"</option>\n<option value=\"0\" selected=\"selected\">">>,
				NoMsg,
				<<"</option></select>">>
			], Form)

	end;

%% @doc Add select
add_story_input({select, Data}, Form) ->

	Name        = proplists:get_value(name, Data, <<>>),
	Value       = add_story_input_get_value(Data, <<>>),
	Options     = proplists:get_value(options, Data, []),

	ReadOnly    = case proplists:get_value(readonly, Data, undefined) of
		              true -> <<" readonly">>;
		              _ -> <<"">>
	              end,

	SelectOptions = add_story_input_select_iterate(Options, Value, <<>>),

	add_story_binary([
		<<"\n<span><select id=\"">>, Name, <<"\" name=\"">>, Name, <<"\"">>, ReadOnly, <<">">>,
		SelectOptions,
		<<"\n</select></span>">>
	], Form);

%% @doc Add map matrix
add_story_input({map_matrix, Data}, Form) ->

	{Width, Height, Matrix} = proplists:get_value(value, Data),
	MaxLength               = proplists:get_value(maxlength, Data, 3),

	Form1 = add_story_binary([
		<<"\n<span>Width (cols): <input type=\"text\" class=\"input-text\" id=\"map_matrix_width\" name=\"map_matrix_width\" value=\"">>,
		asim_lib_web_htmlentities:encode(asim_lib_utils_type_conv:to_list(Width)),
		<<"\" maxlength=\"">>,
		erlang:integer_to_binary(MaxLength),
		<<"\"/></span>">>
	], Form),

	Form2 = add_story_binary([
		<<"\n<span>Height (rows): <input type=\"text\" class=\"input-text\" id=\"map_matrix_height\" name=\"map_matrix_height\" value=\"">>,
		asim_lib_web_htmlentities:encode(asim_lib_utils_type_conv:to_list(Height)),
		<<"\" maxlength=\"">>,
		erlang:integer_to_binary(MaxLength),
		<<"\"/></span>">>
	], Form1),

	Form3 = add_story_binary([
		<<"\n<div id=\"map_matrix\"></div>">>
	], Form2),

	Form3;


%% @doc Add custom data
add_story_input({custom, Data}, Form) ->

	add_story_binary(Data, Form).

%% @doc Util for select
add_story_input_select_iterate([], _Value, Acum) -> Acum;
add_story_input_select_iterate([{OptionKey, OptionValue}|T], Value, Acum) when OptionKey =:= Value ->

	NewAcum = erlang:iolist_to_binary([Acum,
		<<"\n<option value=\"">>,
		asim_lib_utils_type_conv:to_binary(OptionKey),
		<<"\" selected=\"selected\">">>,
		asim_lib_utils_type_conv:to_binary(OptionValue),
		<<"</option>">>
	]),

	add_story_input_select_iterate(T, Value, NewAcum);

add_story_input_select_iterate([{OptionKey, OptionValue}|T], Value, Acum) ->

	NewAcum = erlang:iolist_to_binary([Acum,
		<<"\n<option value=\"">>,
		asim_lib_utils_type_conv:to_binary(OptionKey),
		<<"\">">>,
		asim_lib_utils_type_conv:to_binary(OptionValue),
		<<"</option>">>
	]),

	add_story_input_select_iterate(T, Value, NewAcum);
add_story_input_select_iterate([OptionValue|T], Value, Acum) when OptionValue =:= Value ->

	NewAcum = erlang:iolist_to_binary([Acum,
		<<"\n<option value=\"">>,
		asim_lib_utils_type_conv:to_binary(OptionValue),
		<<"\" selected=\"selected\">">>,
		asim_lib_utils_type_conv:to_binary(OptionValue),
		<<"</option>">>
	]),

	add_story_input_select_iterate(T, Value, NewAcum);

add_story_input_select_iterate([OptionValue|T], Value, Acum) ->

	NewAcum = erlang:iolist_to_binary([Acum,
		<<"\n<option value=\"">>,
		asim_lib_utils_type_conv:to_binary(OptionValue),
		<<"\">">>,
		asim_lib_utils_type_conv:to_binary(OptionValue),
		<<"</option>">>
	]),

	add_story_input_select_iterate(T, Value, NewAcum).

%% @doc Returns story input value always as binary
add_story_input_get_value(Data, Default) -> add_story_input_data_get_binary(value, Data, Default).

add_story_input_data_get_binary(Key, Data, Default) ->

	Value = proplists:get_value(Key, Data, Default),
	add_story_input_data_get_binary(Value).

add_story_input_data_get_binary(true) -> <<"1">>;
add_story_input_data_get_binary(false) -> <<"0">>;
add_story_input_data_get_binary(Value) when erlang:is_binary(Value) -> Value;
add_story_input_data_get_binary(Value) when erlang:is_list(Value) ->
	case add_story_input_data_get_binary_is_string(Value) of
		true -> erlang:list_to_binary(Value);
		_ -> erlang:list_to_binary(io_lib:format("~p", [Value]))
	end;
add_story_input_data_get_binary(Value) when erlang:is_integer(Value) -> erlang:integer_to_binary(Value);
add_story_input_data_get_binary(Value) when erlang:is_float(Value) -> erlang:float_to_binary(Value, [{decimals, 6}, compact]);
add_story_input_data_get_binary(Value) -> erlang:list_to_binary(io_lib:format("~p", [Value])).

add_story_input_data_get_binary_is_string([]) -> true;
add_story_input_data_get_binary_is_string([H|T]) when erlang:is_integer(H), H>=0 ->
	add_story_input_data_get_binary_is_string(T);
add_story_input_data_get_binary_is_string(_) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% add_hidden_field
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_hidden_field(Key, Value, Form = #asim_html_form{hidden_fields = HiddenFields}) ->

	NewHiddenFields = lists:append(HiddenFields, [{asim_lib_utils_type_conv:to_binary(Key), asim_lib_utils_type_conv:to_binary(Value)}]),
	Form#asim_html_form{hidden_fields = NewHiddenFields}.

add_hidden_fields([], Form) -> Form;
add_hidden_fields([{Key, Value}|T], Form) ->

	NewForm = add_hidden_field(Key, Value, Form),
	add_hidden_fields(T, NewForm).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% add_story_specs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%=============================================
%% add_story_specs
%%=============================================

add_story_specs(Template, ConfigList, Form) ->

	FormStory = add_story_specs_parse(Template, ConfigList, []),
	add_story(FormStory, Form).

%% @doc Parse story specs and transform them in form story
add_story_specs_parse([], _ConfigList, Acum) -> Acum;
add_story_specs_parse([{Category, Options}|T], ConfigList, Acum) when erlang:is_list(Options) ->

	Children = add_story_specs_options(Options, ConfigList, []),
	NewAcum  = lists:append(Acum, [{fieldset, [
		{legend, Category},
		{children, Children}
	]}]),
	add_story_specs_parse(T, ConfigList, NewAcum).

%% @doc Parse story specs option
add_story_specs_options([], _ConfigList, Acum) -> Acum;
add_story_specs_options([{Name, Data}|T], ConfigList, Acum) when erlang:is_list(Data) ->

	DataType        = asim_lib_utils_proplists:guard_get_value(type, Data),
	KnownType       = proplists:get_value(known_type, Data, undefined),
	DataLabel       = add_story_specs_option_data_label(Name, Data),
	DataDefault     = add_story_specs_option_data_default(Name, Data, ConfigList),

	Option          = add_story_specs_option(DataType, KnownType, DataLabel, DataDefault, Name, Data),
	NewAcum         = lists:append(Acum, [{field, [{children, [Option]}]}]),
	add_story_specs_options(T, ConfigList, NewAcum).

%%=============================================
%% add_story_specs_option
%%=============================================

%% integer
add_story_specs_option(integer, _KnownType, DataLabel, DataDefault, Name, Data) ->

	{element, [
		{label, DataLabel}, {for, Name}, {size, 3},
		{children, [
			{text, [{name, Name}, {value, DataDefault}]}
		]}
	]};

%% float
add_story_specs_option(float, _KnownType, DataLabel, DataDefault, Name, Data) ->

	{element, [
		{label, DataLabel}, {for, Name}, {size, 3},
		{children, [
			{text, [{name, Name}, {value, DataDefault}]}
		]}
	]};

%% binary
add_story_specs_option(binary, KnownType, DataLabel, DataDefault, Name, Data) -> add_story_specs_option_binary(KnownType, DataLabel, DataDefault, Name, Data);

%% list
add_story_specs_option(list, KnownType, DataLabel, DataDefault, Name, Data) -> add_story_specs_option(binary, KnownType, DataLabel, DataDefault, Name, Data);

%% atom
add_story_specs_option(atom, boolean, DataLabel, DataDefault, Name, Data) ->

	{element, [
		{label, DataLabel}, {for, Name}, {size, 3},
		{children, [
			{boolean, [{name, Name}, {value, DataDefault}]}
		]}
	]};

%% term
add_story_specs_option(term, _KnownType, DataLabel, DataDefault, Name, Data) ->

	BinaryDefaultData = lists:flatten(io_lib:format("~p", [DataDefault])),
	{element, [
		{label, DataLabel}, {for, Name}, {size, 8},
		{children, [
			{textarea, [{name, Name}, {value, BinaryDefaultData}]}
		]}
	]}.

%%=============================================
%% add_story_specs_option_binary
%%=============================================

add_story_specs_option_binary(location_continent, DataLabel, DataDefault, Name, Data) ->

	{element, [
		{label, DataLabel}, {for, Name}, {size, 4},
		{children, [
			{location_continent, [{name, Name}, {value, DataDefault}]}
		]}
	]};

add_story_specs_option_binary(location_country, DataLabel, DataDefault, Name, Data) ->

	{element, [
		{label, DataLabel}, {for, Name}, {size, 4},
		{children, [
			{location_country, [{name, Name}, {value, DataDefault}]}
		]}
	]};

%% handle anything else
add_story_specs_option_binary(_, DataLabel, DataDefault, Name, Data) ->

	Big = proplists:get_value(big, Data, false),
	case Big of
		true ->

			{element, [
				{label, DataLabel}, {for, Name}, {size, 8},
				{children, [
					{textarea, [{name, Name}, {value, DataDefault}]}
				]}
			]};

		_ ->

			{element, [
				{label, DataLabel}, {for, Name}, {size, 8},
				{children, [
					{text, [{name, Name}, {value, DataDefault}]}
				]}
			]}
	end.

%%=============================================
%% add_story_specs_option_data_label
%%=============================================

%% @doc Always return a label
add_story_specs_option_data_label(Name, Data) ->

	case proplists:get_value(label, Data, undefined) of
		undefined -> binary:replace(Name, <<"_">>, <<" ">>);
		Value -> Value
	end.

%%=============================================
%% add_story_specs_option_data_default
%%=============================================

%% @doc Returns default value
add_story_specs_option_data_default(Name, Data, ConfigList) ->

	DefaultValue = proplists:get_value(default, Data, <<"">>),
	add_story_specs_option_data_default(ConfigList, Name, Data, DefaultValue).

add_story_specs_option_data_default([H|T], Name, Data, LastValue) ->

	NewLastValue = proplists:get_value(Name, H, LastValue),
	add_story_specs_option_data_default(T, Name, Data, NewLastValue);

add_story_specs_option_data_default([], _Name, _Data, LastValue) -> LastValue.

