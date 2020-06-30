%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc Implements various unicode helper functions
%%% @end
%%%-------------------------------------------------------------------

-module(asim_lib_utils_unicode).
-author("madalin").

-include("../include/asim.hrl").

%% API
-export([characters_to_unicode_list/1]).
-export([characters_to_unicode_list/2]).
-export([unicode_list_to_binary/1]).
-export([unicode_list_to_binary/2]).

-export([trim/1]).
-export([trim/2]).
-export([trim_left/1]).
-export([trim_right/1]).

-export([is_unicode_letters/1]).
-export([is_unicode_name/1]).

-export([is_ascii/1]).
-export([is_ascii_alpha/1]).
-export([is_ascii_alpha_lower/1]).
-export([is_ascii_alpha_upper/1]).
-export([is_ascii_numeric/1]).
-export([is_ascii_alpha_numeric/1]).
-export([is_ascii_alpha_numeric_lower/1]).
-export([is_ascii_alpha_numeric_upper/1]).

-export([replace/3]).
-export([replace_string/3]).
-export([explode_trim_skip_empty/2]).

-export([to_lower/1]).
-export([to_upper/1]).
-export([to_title/1]).

%% @doc Converts a possibly deep list of integers and binaries into a list of integers representing Unicode characters.
%% Returns unicode character list on success or 'error' atom otherwise.
%% The binaries in the input can have characters encoded as one of the following:
%% - ISO Latin-1 (0-255, one character per byte). Here, case parameter InEncoding is to be specified as latin1.
%% - One of the UTF-encodings, which is specified as parameter InEncoding.
characters_to_unicode_list(Input) -> characters_to_unicode_list(Input, utf8).
characters_to_unicode_list(Input, InEncoding) ->

    %% Try decoding
    try unicode:characters_to_list(Input, InEncoding) of
        Decoded ->
            case erlang:is_list(Decoded) of
                true -> Decoded;
                _ -> error
            end
    catch
        _:_ -> error
    end.

%% @doc Converts a possibly unicode list of integers into a binary representing Unicode characters.
%% Returns binary on success or 'error' atom otherwise.
unicode_list_to_binary(Input) -> unicode_list_to_binary(Input, utf8).
unicode_list_to_binary(Input, InEncoding) ->

    %% Try decoding
    try unicode:characters_to_binary(Input, InEncoding) of
        Decoded ->
            case erlang:is_binary(Decoded) of
                true -> Decoded;
                _ -> error
            end
    catch
        _:_ -> error
    end.

%% @doc Trim unicode list left, right, both or none
trim(UnicodeList, left) -> trim_left(UnicodeList);
trim(UnicodeList, right) -> trim_right(UnicodeList);
trim(UnicodeList, both) -> trim(UnicodeList);
trim(UnicodeList, _) -> UnicodeList.

%% @doc Trim unicode list
trim(UnicodeList) when erlang:is_list(UnicodeList) ->
    LeftTrim        = trim_left(UnicodeList),
    Reversed        = lists:reverse(LeftTrim),
    TrimReversed    = trim_left(Reversed),
    lists:reverse(TrimReversed).

%% @doc Left trim unicode list
trim_left(UnicodeList) when is_list(UnicodeList) -> trim_left_unicodelist(UnicodeList).

%% @doc Right trim unicode list
trim_right(UnicodeList) when is_list(UnicodeList) ->
    Reversed        = lists:reverse(UnicodeList),
    TrimReversed    = trim_left(Reversed),
    lists:reverse(TrimReversed).

%% @doc Internal head trim function used by all the other
trim_left_unicodelist([16#0009|T]) -> trim_left_unicodelist(T);  %% character tabulation
trim_left_unicodelist([16#000A|T]) -> trim_left_unicodelist(T);  %% line feed
trim_left_unicodelist([16#000B|T]) -> trim_left_unicodelist(T);  %% line tabulation
trim_left_unicodelist([16#000C|T]) -> trim_left_unicodelist(T);  %% form feed
trim_left_unicodelist([16#000D|T]) -> trim_left_unicodelist(T);  %% carriage return
trim_left_unicodelist([16#0020|T]) -> trim_left_unicodelist(T);  %% space Most common (normal ASCII space)
trim_left_unicodelist([16#0085|T]) -> trim_left_unicodelist(T);  %% next line
trim_left_unicodelist([16#00A0|T]) -> trim_left_unicodelist(T);  %% no-break space, identical to 16#0020, but not a point at which a line may be broken.
trim_left_unicodelist([16#1680|T]) -> trim_left_unicodelist(T);  %% ogham space mark. Used for interword separation in Ogham text. Normally a vertical line in vertical text or a horizontal line in horizontal text, but may also be a blank space in "stemless" fonts.
trim_left_unicodelist([16#2000|T]) -> trim_left_unicodelist(T);  %% en quad 8192 - Punctuation Separator space Width of one en.
trim_left_unicodelist([16#2001|T]) -> trim_left_unicodelist(T);  %% em quad 8193 - Common General Punctuation Separator space Also known as "mutton quad". Width of one em.
trim_left_unicodelist([16#2002|T]) -> trim_left_unicodelist(T);  %% en space 8194 - Common General Punctuation Separator space Also known as "nut". Width of one en.
trim_left_unicodelist([16#2003|T]) -> trim_left_unicodelist(T);  %% em space 8195 - Common General Punctuation Separator space. Also known as "mutton". Width of one em.
trim_left_unicodelist([16#2004|T]) -> trim_left_unicodelist(T);  %% three-per-em space 8196 - Common General Punctuation Separator space. Also known as "thick space". One third of an em wide.
trim_left_unicodelist([16#2005|T]) -> trim_left_unicodelist(T);  %% four-per-em space 8197 - Common General Punctuation Separator space. Also known as "mid space". One fourth of an em wide.
trim_left_unicodelist([16#2006|T]) -> trim_left_unicodelist(T);  %% six-per-em space 8198 - Common General Punctuation Separator space. One sixth of an em wide. In computer typography, sometimes equated to 16#2009.
trim_left_unicodelist([16#2007|T]) -> trim_left_unicodelist(T);  %% figure space 8199 - Common General Punctuation Separator space. Figure space. In fonts with monospaced digits, equal to the width of one digit. HTML/XML named entity: &numsp;
trim_left_unicodelist([16#2008|T]) -> trim_left_unicodelist(T);  %% punctuation space 8200 - Common General Punctuation Separator space. As wide as the narrow punctuation in a font, HTML/XML named entity: &puncsp;
trim_left_unicodelist([16#2009|T]) -> trim_left_unicodelist(T);  %% thin space 8201 - Common General Punctuation Separator space. One-fifth (sometimes one-sixth) of an em wide. Recommended for use as a thousands separator for measures made with SI units.
trim_left_unicodelist([16#200A|T]) -> trim_left_unicodelist(T);  %% hair space 8202 - Common General Punctuation Separator space. Thinner than a thin space. HTML/XML named entity: &hairsp;
trim_left_unicodelist([16#2028|T]) -> trim_left_unicodelist(T);  %% line separator 8232 - Common General Punctuation Separator space. Line.
trim_left_unicodelist([16#2029|T]) -> trim_left_unicodelist(T);  %% paragraph separator 8233 - Common General Punctuation Separator space. Paragraph.
trim_left_unicodelist([16#202F|T]) -> trim_left_unicodelist(T);  %% narrow no-break space 8239 - Common General Punctuation Separator space. Narrow no-break space. Similar in function to 16#00A0 No-Break Space. When used with Mongolian, its width is usually one third of the normal space.
trim_left_unicodelist([16#205F|T]) -> trim_left_unicodelist(T);  %% medium mathematical space 8287 - Common General Punctuation Separator space. MMSP. Used in mathematical formulae.
trim_left_unicodelist([16#3000|T]) -> trim_left_unicodelist(T);  %% ideographic space 12288 - CJK Symbols Punctuation Separator, used, for example, in tai tou.
trim_left_unicodelist(Rest) -> Rest.

%% @doc Returns true if the specified unicode list contains only alphabetic letters or false otherwise.
%% All letters in all languages are taken in account not only standard english ASCII letters.
%% This function allow you to check for valid names for example.
is_unicode_letters([]) -> true;
is_unicode_letters([H|T]) ->
    case asim_lib_utils_unicode_char:is_unicode_alpha(H) of
        true -> is_unicode_letters(T);
        false -> false
    end.

is_unicode_name([]) -> true;
is_unicode_name([H|T]) ->
    case asim_lib_utils_unicode_char:is_unicode_alpha(H) of
        true -> is_unicode_name(T);
        false -> false
    end.

%% @doc Returns true if the specified unicode list contains only ASCII characters or false otherwise.
is_ascii([]) -> true;
is_ascii([H|T]) ->
    case asim_lib_utils_unicode_char:is_ascii(H) of
        true -> is_ascii(T);
        false -> false
    end.

%% @doc Returns true if the specified unicode list contains only ASCII alphabetic characters or false otherwise.
is_ascii_alpha([]) -> true;
is_ascii_alpha([H|T]) ->
    case asim_lib_utils_unicode_char:is_ascii_alpha(H) of
        true -> is_ascii_alpha(T);
        false -> false
    end.

%% @doc Returns true if the specified unicode list contains only ASCII alphabetic lower case characters or false otherwise.
is_ascii_alpha_lower([]) -> true;
is_ascii_alpha_lower([H|T]) ->
    case asim_lib_utils_unicode_char:is_ascii_alpha_lower(H) of
        true -> is_ascii_alpha_lower(T);
        false -> false
    end.

%% @doc Returns true if the specified unicode list contains only ASCII alphabetic upper case characters or false otherwise.
is_ascii_alpha_upper([]) -> true;
is_ascii_alpha_upper([H|T]) ->
    case asim_lib_utils_unicode_char:is_ascii_alpha_upper(H) of
        true -> is_ascii_alpha_upper(T);
        false -> false
    end.

%% @doc Returns true if the specified unicode list contains only ASCII numeric characters or false otherwise.
is_ascii_numeric([]) -> true;
is_ascii_numeric([H|T]) ->
    case asim_lib_utils_unicode_char:is_ascii_numeric(H) of
        true -> is_ascii_numeric(T);
        false -> false
    end.

%% @doc Returns true if the specified unicode list contains only ASCII alphabetic and numeric characters or false otherwise.
is_ascii_alpha_numeric([]) -> true;
is_ascii_alpha_numeric([H|T]) ->
    case asim_lib_utils_unicode_char:is_ascii_alpha_numeric(H) of
        true -> is_ascii_alpha_numeric(T);
        false -> false
    end.

%% @doc Returns true if the specified unicode list contains only ASCII alphabetic lower case and numeric characters or false otherwise.
is_ascii_alpha_numeric_lower([]) -> true;
is_ascii_alpha_numeric_lower([H|T]) ->
    case asim_lib_utils_unicode_char:is_ascii_alpha_numeric_lower(H) of
        true -> is_ascii_alpha_numeric_lower(T);
        false -> false
    end.

%% @doc Returns true if the specified unicode list contains only ASCII alphabetic upper case and numeric characters or false otherwise.
is_ascii_alpha_numeric_upper([]) -> true;
is_ascii_alpha_numeric_upper([H|T]) ->
    case asim_lib_utils_unicode_char:is_ascii_alpha_numeric_upper(H) of
        true -> is_ascii_alpha_numeric_upper(T);
        false -> false
    end.

%% @doc Replace the specified list of characters with the specified character or characters
replace([], _Replace, Subject) -> Subject;
replace([H|T], Replace, Subject) when erlang:is_list(H) ->
    NewSubject = replace_string(H, Replace, Subject),
    replace(T, Replace, NewSubject);
replace([H|T], Replace, Subject) when erlang:is_integer(H) ->
    NewSubject = replace_char(H, Replace, Subject),
    replace(T, Replace, NewSubject);
replace(Char, Replace, Subject) when erlang:is_integer(Char) -> replace_char(Char, Replace, Subject).

%% @doc Replace the specified character wuth the specified character or characters
replace_char(_Char, _Replace, []) -> [];
replace_char(Char, Replace, Subject) -> replace_char(Subject, Char, Replace, []).
replace_char([], _Char, _Replace, Acum) -> lists:flatten(Acum);
replace_char([H|T], H, Replace, Acum) ->
    NewAcum = lists:append(Acum, [Replace]),
    replace_char(T, H, Replace, NewAcum);
replace_char([H|T], Char, Replace, Acum) ->
    NewAcum = lists:append(Acum, [H]),
    replace_char(T, Char, Replace, NewAcum).

%% @doc Replace the specified string with the specified character or characters
replace_string([], _Replace, Subject) -> Subject;
replace_string(_Search, _Replace, []) -> [];
replace_string(Search, Replace, Subject) ->
    SearchLen   = erlang:length(Search),
    SubjectLen  = erlang:length(Subject),
    replace_string(Search, Replace, Subject, SearchLen, SubjectLen, []).
replace_string(_Search, _Replace, Subject, SearchLen, SubjectLen, Acum) when SubjectLen < SearchLen -> lists:flatten(lists:append(Acum, Subject));
replace_string(Search, Replace, Subject, SearchLen, SubjectLen, Acum) ->
    case lists:split(SearchLen, Subject) of
        {Search, RemainingSubject} ->

            %% Match/replace
            NewAcum = lists:append(Acum, [Replace]),
            replace_string(Search, Replace, RemainingSubject, SearchLen, SubjectLen-SearchLen, NewAcum);

        _ ->

            %% No match add next character to acumulator and continue
            [HSubject | TSubject] = Subject,
            NewAcum = lists:append(Acum, [HSubject]),
            replace_string(Search, Replace, TSubject, SearchLen, SubjectLen-1, NewAcum)

    end.

explode_trim_skip_empty(Search, Subject) -> explode_trim_skip_empty(Search, Subject, [], []).
explode_trim_skip_empty(_Search, [], Word, Acum) ->
    TWord = trim(Word),
    case TWord of
        [] -> Acum;
        _ -> lists:append(Acum, [TWord])
    end;
explode_trim_skip_empty(Search, [Search | T], Word, Acum) ->
    TWord = trim(Word),
    case TWord of
        [] -> explode_trim_skip_empty(Search, T, Word, Acum);
        _ ->
            NewAcum = lists:append(Acum, [TWord]),
            explode_trim_skip_empty(Search, T, [], NewAcum)
    end;
explode_trim_skip_empty(Search, [H|T], Word, Acum) ->
    NewWord = lists:append(Word, [H]),
    explode_trim_skip_empty(Search, T, NewWord, Acum).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% to_
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

to_upper(UnicodeList) when is_list(UnicodeList) -> [asim_lib_utils_unicode_char:to_upper(C) || C <- UnicodeList].

to_lower(UnicodeList) when is_list(UnicodeList) -> [asim_lib_utils_unicode_char:to_lower(C) || C <- UnicodeList].

to_title(UnicodeList) when is_binary(UnicodeList) -> [asim_lib_utils_unicode_char:to_title(C) || C <- UnicodeList].



