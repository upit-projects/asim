%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(asim_lib_web_request).
-author("madalin").

-include("../include/asim.hrl").

%% API
-export([get/3]).
-export([get_list/2]).
-export([always_get/4]).

-export([get_selected_key/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get_selected_key
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns the specified selected key if found either passed by database navigator or manually
%% This method only handle single selection from database navigator and returns failure on multiple selection.
get_selected_key(Msg, Route, Key, State) ->

    Result = get_selected_keys(Key, State),
    case Result of
        {ok, Value} -> Value;
        _ -> asim_lib_web_redirect:route(Msg, Route, State)
    end.

get_selected_keys(Key, State = #asim_state{request = Request}) ->

    SelectedKeys = asim_lib_web_table_nav:request_get_selected_keys(State),
    case SelectedKeys of
        [Element] ->

            %% Only handle single selection not multiple selection
            %% Multiple selection means something is wrong
            {ok, Element};

        _ ->

	        case proplists:get_value(Key, Request, undefined) of
		        undefined -> missing;
		        Value -> {ok, Value}
	        end

    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% always_get
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns the specified variable from request
always_get(Validator, Name, Default, State) ->

    case get(Validator, Name, State) of
        {ok, ProperTypeValue, NewState} -> {ProperTypeValue, NewState};
        {_, _, _, NewState} -> {Default, NewState}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns a list of variables from request
get_list(Validators, State) -> get_list(Validators, ok, [], State).
get_list([], LastError, Acum, State) -> {LastError, Acum, State};
get_list([{Name, Validator}|T], LastError, Acum, State) ->

    case get(Validator, Name, State) of
        {ok, OkValue, NewState} ->

            OkAcum = lists:append(Acum, [{Name, OkValue}]),
            get_list(T, LastError, OkAcum, NewState);

        {_, _, _, ErrorState} ->

            ErrorValue = proplists:get_value(default, Validator, <<>>),
            ErrorAcum  = lists:append(Acum, [{Name,ErrorValue}]),
            get_list(T, error, ErrorAcum, ErrorState)

    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns the specified variable from request
get(Validator, Name, State = #asim_state{ob = Ob, request = Request}) ->

    Value = proplists:get_value(Name, Request, undefined),
    case Value of
        undefined ->

                NewOb   = asim_lib_web_ob:add_message(#asim_ob_message{
                message = erlang:iolist_to_binary([Name, <<" is undefined!">>]),
                class   = <<"error">>}, Ob),
                NewState    = State#asim_state{ob = NewOb},
                {error, undefined, undefined, NewState};

        _ ->

            %% Get in encoding
            InEncoding = proplists:get_value(in_encoding, Validator, utf8),

            %% Make sure value is unicode character list
            UnicodeListValue = asim_lib_utils_unicode:characters_to_unicode_list(Value, InEncoding),
            case UnicodeListValue of
                error ->

                    NewOb       = asim_lib_web_ob:add_message(#asim_ob_message{
                        message = erlang:iolist_to_binary([<<"Invalid encoding for ">>, Name]),
                        class   = <<"error">>}, Ob),
                    NewState    = State#asim_state{ob = NewOb},
                    {error, invalid_encoding, undefined, NewState};

                _ ->

                    %% Trim input if necessary
                    Trim                    = proplists:get_value(trim, Validator, both),
                    TrimedUnicodeListValue  = asim_lib_utils_unicode:trim(UnicodeListValue, Trim),

                    %% get the type and validate it
                    DataType = asim_lib_utils_proplists:guard_get_value(type, Validator),
                    case get_type(DataType, Validator, TrimedUnicodeListValue) of
                        {ok, ProperTypeValue} -> {ok, ProperTypeValue, State};
                        _ ->

                            NewOb       = asim_lib_web_ob:add_message(#asim_ob_message{
                                message = erlang:iolist_to_binary([Name, <<" is invalid!">>]),
                                class   = <<"error">>}, Ob),
                            NewState    = State#asim_state{ob = NewOb},
                            {error, invalid_type, Value, NewState}

                    end

            end

    end.

%% @doc Returns the specified type with exception catch
get_type(Type, Validator, Value) ->

    try

        %% Alow empty?
        AllowEmpty = proplists:get_value(allow_empty, Validator, true),
        case {AllowEmpty, Value} of
            {false, []} -> error;
            _ -> get_type_always(Type, Validator, Value)
        end

    catch
        _:_ -> error
    end.

%% integer
get_type_always(integer, Validator, Value) -> get_type_integer(Validator, Value);

%% float
get_type_always(float, Validator, Value) -> get_type_float(Validator, Value);

%% binary
get_type_always(binary, Validator, Value) -> get_type_binary(Validator, Value);

%% list
get_type_always(list, Validator, Value) -> get_type_list(Validator, Value);

%% atom
get_type_always(atom, Validator, Value) -> get_type_atom(Validator, Value);

%% term
get_type_always(term, Validator, Value) -> get_type_term(Validator, Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get_type_integer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_type_integer(Validator, Value) ->

    case asim_lib_utils_unicode:is_ascii_numeric(Value) of
        true ->

            IntegerValue    = erlang:list_to_integer(Value),
            Unsigned        = proplists:get_value(unsigned, Validator, false),
            Size 	        = proplists:get_value(size, Validator, undefined),
            Min 	        = proplists:get_value(min, Validator, undefined),
            Max 	        = proplists:get_value(max, Validator, undefined),
            get_type_integer_check(Unsigned, Size, Min, Max, IntegerValue);

        false -> error
    end.

get_type_integer_check(true, _Size, _Min, _Max, Value) when Value < 0 -> error;
get_type_integer_check(true, Size, Min, Max, Value) -> get_type_integer_check_unsigned_size(Size, Min, Max, Value);
get_type_integer_check(false, Size, Min, Max, Value) -> get_type_integer_check_signed_size(Size, Min, Max, Value).

get_type_integer_check_unsigned_size(8, _Min, _Max, Value) when Value > 255 -> error;
get_type_integer_check_unsigned_size(16, _Min, _Max, Value) when Value > 65535 -> error;
get_type_integer_check_unsigned_size(32, _Min, _Max, Value) when Value > 4294967295 -> error;
get_type_integer_check_unsigned_size(64, _Min, _Max, Value) when Value > 18446744073709551615 -> error;
get_type_integer_check_unsigned_size(128, _Min, _Max, Value) when Value > 340282366920938463463374607431768211455 -> error;
get_type_integer_check_unsigned_size(_Size, Min, Max, Value) -> get_type_integer_or_float_check_min_max(Min, Max, Value).

get_type_integer_check_signed_size(8, _Min, _Max, Value) when Value < -128, Value > 127 -> error;
get_type_integer_check_signed_size(16, _Min, _Max, Value) when Value < -32768, Value > 32767 -> error;
get_type_integer_check_signed_size(32, _Min, _Max, Value) when Value < -2147483648, Value > 2147483647 -> error;
get_type_integer_check_signed_size(64, _Min, _Max, Value) when Value < -9223372036854775808, Value > 9223372036854775807 -> error;
get_type_integer_check_signed_size(128, _Min, _Max, Value) when Value < -170141183460469231731687303715884105728, Value > 170141183460469231731687303715884105727 -> error;
get_type_integer_check_signed_size(_Size, Min, Max, Value) -> get_type_integer_or_float_check_min_max(Min, Max, Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get_type_float
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_type_float(Validator, Value) ->

    FloatValue  = erlang:list_to_float(Value),

    Unsigned    = proplists:get_value(unsigned, Validator, false),
    Min 	    = proplists:get_value(min, Validator, undefined),
    Max 	    = proplists:get_value(max, Validator, undefined),
    get_type_float_check(Unsigned, Min, Max, FloatValue).

get_type_float_check(true, _Min, _Max, Value) when Value < 0.0 -> error;
get_type_float_check(_, Min, Max, Value) -> get_type_integer_or_float_check_min_max(Min, Max, Value).

%%=============================================
%% get_type_integer_or_float_check_min_max
%%=============================================

get_type_integer_or_float_check_min_max(Min, Max, Value) when Value < Min, Value > Max -> error;
get_type_integer_or_float_check_min_max(_Min, _Max, Value) -> {ok, Value}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get_type_list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns list type
get_type_list(Validator, Value) ->

    Subtype         = proplists:get_value(known_type, Validator, undefined),

    ValueRN1        = asim_lib_utils_unicode:replace_string("\r\n", "\n", Value),
    ValueRN2        = asim_lib_utils_unicode:replace("\r", "\n", ValueRN1),
    ValueRN3        = asim_lib_utils_unicode:replace("\n", "\r\n", ValueRN2),

    case get_type_list_known_type(Subtype, Validator, ValueRN3) of
        {ok, SubtypeValue} ->

            OutEncoding     = proplists:get_value(out_encoding, Validator, utf8),
            BinaryValue     = unicode:characters_to_binary(SubtypeValue, utf8, OutEncoding),
            {ok, BinaryValue};

        Error -> Error
    end.

get_type_list_known_type(unicode_letters, _Validator, Value) ->
    case asim_lib_utils_unicode:is_unicode_letters(Value) of
        true -> {ok, Value};
        false -> error
    end;

get_type_list_known_type(unicode_name, _Validator, Value) ->
    case asim_lib_utils_unicode:is_unicode_name(Value) of
        true -> {ok, Value};
        false -> error
    end;

get_type_list_known_type(ascii, _Validator, Value) ->
    case asim_lib_utils_unicode:is_ascii(Value) of
        true -> {ok, Value};
        false -> error
    end;

get_type_list_known_type(ascii_alpha, _Validator, Value) ->
    case asim_lib_utils_unicode:is_ascii_alpha(Value) of
        true -> {ok, Value};
        false -> error
    end;

get_type_list_known_type(ascii_alpha_lower, _Validator, Value) ->
    case asim_lib_utils_unicode:is_ascii_alpha_lower(Value) of
        true -> {ok, Value};
        false -> error
    end;

get_type_list_known_type(ascii_alpha_upper, _Validator, Value) ->
    case asim_lib_utils_unicode:is_ascii_alpha_upper(Value) of
        true -> {ok, Value};
        false -> error
    end;

get_type_list_known_type(ascii_numeric, _Validator, Value) ->
    case asim_lib_utils_unicode:is_ascii_numeric(Value) of
        true -> {ok, Value};
        false -> error
    end;

get_type_list_known_type(ascii_alpha_numeric, _Validator, Value) ->
    case asim_lib_utils_unicode:is_ascii_alpha_numeric(Value) of
        true -> {ok, Value};
        false -> error
    end;

get_type_list_known_type(ascii_alpha_numeric_lower, _Validator, Value) ->
    case asim_lib_utils_unicode:is_ascii_alpha_numeric_lower(Value) of
        true -> {ok, Value};
        false -> error
    end;

get_type_list_known_type(ascii_alpha_numeric_upper, _Validator, Value) ->
    case asim_lib_utils_unicode:is_ascii_alpha_numeric_upper(Value) of
        true -> {ok, Value};
        false -> error
    end;

get_type_list_known_type(_KnownType, _Validator, Value) -> {ok, Value}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get_type_binary
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_type_binary(Validator, Value) ->

    KnownType  = proplists:get_value(known_type, Validator, undefined),
    get_type_binary(KnownType, Validator, Value).

get_type_binary(_KnownType, Validator, Value) -> get_type_list(Validator, Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get_type_atom
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_type_atom(Validator, Value) ->

    KnownType  = proplists:get_value(known_type, Validator, undefined),
    get_type_atom_known_type(KnownType, Validator, Value).

get_type_atom_known_type(boolean, _Validator, "0") -> {ok, false};
get_type_atom_known_type(boolean, _Validator, "1") -> {ok, true};
get_type_atom_known_type(boolean, _Validator, "true") -> {ok, true};
get_type_atom_known_type(boolean, _Validator, "false") -> {ok, false}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get_type_term
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_type_term(Validator, Value) ->

    KnownType  = proplists:get_value(known_type, Validator, undefined),
    get_type_term_known_type(KnownType, Validator, Value).

get_type_term_known_type(erlang, _Validator, Value) ->

    StripedValue = asim_lib_utils_code:strip_add_dot(Value),
    case asim_lib_utils_code:eval(StripedValue) of
        {ok, Term} -> {ok, Term};
        _ -> error
    end.

