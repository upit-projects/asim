%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc Implements various helper functions to deal with unicode characters
%%% @end
%%%-------------------------------------------------------------------

-module(asim_lib_utils_unicode_char).
-author("madalin").

-include("../include/asim.hrl").

%% API
-export([is_unicode_alpha/1]).
-export([is_unicode_numeric/1]).
-export([is_unicode_alpha_numeric/1]).
-export([is_unicode_space/1]).
-export([is_unicode_line_break/1]).
-export([is_unicode_space_or_line_break/1]).

-export([is_ascii/1]).
-export([is_ascii_alpha/1]).
-export([is_ascii_alpha_lower/1]).
-export([is_ascii_alpha_upper/1]).
-export([is_ascii_numeric/1]).
-export([is_ascii_alpha_numeric/1]).
-export([is_ascii_alpha_numeric_lower/1]).
-export([is_ascii_alpha_numeric_upper/1]).

-export([to_upper/1]).
-export([to_lower/1]).
-export([to_title/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% is_unicode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns true if the specified character is valid alphabetic unicode letter or false otherwise
is_unicode_alpha(UnicodeChar) -> is_unicode_alpha(?UNICODE_ALPHABETIC_LIST, UnicodeChar).

%% @doc Iterate all unicode alphabetic intervals and compare with our letter
is_unicode_alpha([{V1,V2}|_T], UnicodeChar) when UnicodeChar >= V1, UnicodeChar =< V2 -> true;
is_unicode_alpha([H|_T], UnicodeChar) when UnicodeChar =:= H -> true;
%% Not in this interval, continue searching
is_unicode_alpha([_H|T], UnicodeChar) -> is_unicode_alpha(T, UnicodeChar);
%% Not found
is_unicode_alpha([], _UnicodeChar) -> false.

%% @doc Returns true if the specified character is valid numeric unicode letter or false otherwise
is_unicode_numeric(UnicodeChar) -> is_ascii_numeric(UnicodeChar).

%% @doc Returns true if the specified unicode character is valid alpha numeric unicode letter or false otherwise
is_unicode_alpha_numeric(UnicodeChar) ->

    case is_unicode_numeric(UnicodeChar) of
        true -> true;
        _ -> is_unicode_alpha(UnicodeChar)
    end.

%% @doc Returns true if the specified unicode character is a spacing unicode character or false otherwise
%% this method will return true if the character is one of the following:
%% - character tabulation
%% - line tabulation
%% - form feed
%% - space Most common (normal ASCII space)
%% - no-break space, identical to 16#0020, but not a point at which a line may be broken.
%% - ogham space mark. Used for interword separation in Ogham text. Normally a vertical line in vertical text or a horizontal line in horizontal text, but may also be a blank space in "stemless" fonts.
%% - en quad 8192 - Punctuation Separator space Width of one en.
%% - em quad 8193 - Common General Punctuation Separator space Also known as "mutton quad". Width of one em.
%% - en space 8194 - Common General Punctuation Separator space Also known as "nut". Width of one en.
%% - em space 8195 - Common General Punctuation Separator space. Also known as "mutton". Width of one em.
%% - three-per-em space 8196 - Common General Punctuation Separator space. Also known as "thick space". One third of an em wide.
%% - four-per-em space 8197 - Common General Punctuation Separator space. Also known as "mid space". One fourth of an em wide.
%% - six-per-em space 8198 - Common General Punctuation Separator space. One sixth of an em wide. In computer typography, sometimes equated to 16#2009.
%% - figure space 8199 - Common General Punctuation Separator space. Figure space. In fonts with monospaced digits, equal to the width of one digit. HTML/XML named entity: &numsp;
%% - punctuation space 8200 - Common General Punctuation Separator space. As wide as the narrow punctuation in a font, HTML/XML named entity: &puncsp;
%% - thin space 8201 - Common General Punctuation Separator space. One-fifth (sometimes one-sixth) of an em wide. Recommended for use as a thousands separator for measures made with SI units.
%% - hair space 8202 - Common General Punctuation Separator space. Thinner than a thin space. HTML/XML named entity: &hairsp;
%% - narrow no-break space 8239 - Common General Punctuation Separator space. Narrow no-break space. Similar in function to 16#00A0 No-Break Space. When used with Mongolian, its width is usually one third of the normal space.
%% - medium mathematical space 8287 - Common General Punctuation Separator space. MMSP. Used in mathematical formulae.
%% - ideographic space 12288 - CJK Symbols Punctuation Separator, used, for example, in tai tou.
is_unicode_space(16#0009) -> true;  %% character tabulation
is_unicode_space(16#000B) -> true;  %% line tabulation
is_unicode_space(16#000C) -> true;  %% form feed
is_unicode_space(16#0020) -> true;  %% space Most common (normal ASCII space)
is_unicode_space(16#00A0) -> true;  %% no-break space, identical to 16#0020, but not a point at which a line may be broken.
is_unicode_space(16#1680) -> true;  %% ogham space mark. Used for interword separation in Ogham text. Normally a vertical line in vertical text or a horizontal line in horizontal text, but may also be a blank space in "stemless" fonts.
is_unicode_space(16#2000) -> true;  %% en quad 8192 - Punctuation Separator space Width of one en.
is_unicode_space(16#2001) -> true;  %% em quad 8193 - Common General Punctuation Separator space Also known as "mutton quad". Width of one em.
is_unicode_space(16#2002) -> true;  %% en space 8194 - Common General Punctuation Separator space Also known as "nut". Width of one en.
is_unicode_space(16#2003) -> true;  %% em space 8195 - Common General Punctuation Separator space. Also known as "mutton". Width of one em.
is_unicode_space(16#2004) -> true;  %% three-per-em space 8196 - Common General Punctuation Separator space. Also known as "thick space". One third of an em wide.
is_unicode_space(16#2005) -> true;  %% four-per-em space 8197 - Common General Punctuation Separator space. Also known as "mid space". One fourth of an em wide.
is_unicode_space(16#2006) -> true;  %% six-per-em space 8198 - Common General Punctuation Separator space. One sixth of an em wide. In computer typography, sometimes equated to 16#2009.
is_unicode_space(16#2007) -> true;  %% figure space 8199 - Common General Punctuation Separator space. Figure space. In fonts with monospaced digits, equal to the width of one digit. HTML/XML named entity: &numsp;
is_unicode_space(16#2008) -> true;  %% punctuation space 8200 - Common General Punctuation Separator space. As wide as the narrow punctuation in a font, HTML/XML named entity: &puncsp;
is_unicode_space(16#2009) -> true;  %% thin space 8201 - Common General Punctuation Separator space. One-fifth (sometimes one-sixth) of an em wide. Recommended for use as a thousands separator for measures made with SI units.
is_unicode_space(16#200A) -> true;  %% hair space 8202 - Common General Punctuation Separator space. Thinner than a thin space. HTML/XML named entity: &hairsp;
is_unicode_space(16#202F) -> true;  %% narrow no-break space 8239 - Common General Punctuation Separator space. Narrow no-break space. Similar in function to 16#00A0 No-Break Space. When used with Mongolian, its width is usually one third of the normal space.
is_unicode_space(16#205F) -> true;  %% medium mathematical space 8287 - Common General Punctuation Separator space. MMSP. Used in mathematical formulae.
is_unicode_space(16#3000) -> true;  %% ideographic space 12288 - CJK Symbols Punctuation Separator, used, for example, in tai tou.
is_unicode_space(_) -> false.

%% @doc Returns true if the specified unicode character is a line break unicode character or false otherwise.
%% This method will return true if the character is one of the following:
%% - line feed
%% - carriage return
%% - next line
%% - line separator 8232 - Common General Punctuation Separator space. Line.
%% - paragraph separator 8233 - Common General Punctuation Separator space. Paragraph.
is_unicode_line_break(16#000A) -> true;  %% line feed
is_unicode_line_break(16#000D) -> true;  %% carriage return
is_unicode_line_break(16#0085) -> true;  %% next line
is_unicode_line_break(16#2028) -> true;  %% line separator 8232 - Common General Punctuation Separator space. Line.
is_unicode_line_break(16#2029) -> true;  %% paragraph separator 8233 - Common General Punctuation Separator space. Paragraph.
is_unicode_line_break(_) -> false.

%% @doc Returns true if the specified unicode character is a spacing unicode character or a line break unicode character.
%% this method will return true if the character is one of the following:
%% - character tabulation
%% - line feed
%% - line tabulation
%% - form feed
%% - carriage return
%% - space Most common (normal ASCII space)
%% - next line
%% - no-break space, identical to 16#0020, but not a point at which a line may be broken.
%% - ogham space mark. Used for interword separation in Ogham text. Normally a vertical line in vertical text or a horizontal line in horizontal text, but may also be a blank space in "stemless" fonts.
%% - en quad 8192 - Punctuation Separator space Width of one en.
%% - em quad 8193 - Common General Punctuation Separator space Also known as "mutton quad". Width of one em.
%% - en space 8194 - Common General Punctuation Separator space Also known as "nut". Width of one en.
%% - em space 8195 - Common General Punctuation Separator space. Also known as "mutton". Width of one em.
%% - three-per-em space 8196 - Common General Punctuation Separator space. Also known as "thick space". One third of an em wide.
%% - four-per-em space 8197 - Common General Punctuation Separator space. Also known as "mid space". One fourth of an em wide.
%% - six-per-em space 8198 - Common General Punctuation Separator space. One sixth of an em wide. In computer typography, sometimes equated to 16#2009.
%% - figure space 8199 - Common General Punctuation Separator space. Figure space. In fonts with monospaced digits, equal to the width of one digit. HTML/XML named entity: &numsp;
%% - punctuation space 8200 - Common General Punctuation Separator space. As wide as the narrow punctuation in a font, HTML/XML named entity: &puncsp;
%% - thin space 8201 - Common General Punctuation Separator space. One-fifth (sometimes one-sixth) of an em wide. Recommended for use as a thousands separator for measures made with SI units.
%% - hair space 8202 - Common General Punctuation Separator space. Thinner than a thin space. HTML/XML named entity: &hairsp;
%% - line separator 8232 - Common General Punctuation Separator space. Line.
%% - paragraph separator 8233 - Common General Punctuation Separator space. Paragraph.
%% - narrow no-break space 8239 - Common General Punctuation Separator space. Narrow no-break space. Similar in function to 16#00A0 No-Break Space. When used with Mongolian, its width is usually one third of the normal space.
%% - medium mathematical space 8287 - Common General Punctuation Separator space. MMSP. Used in mathematical formulae.
%% - ideographic space 12288 - CJK Symbols Punctuation Separator, used, for example, in tai tou.
is_unicode_space_or_line_break(16#0009) -> true;  %% character tabulation
is_unicode_space_or_line_break(16#000A) -> true;  %% line feed
is_unicode_space_or_line_break(16#000B) -> true;  %% line tabulation
is_unicode_space_or_line_break(16#000C) -> true;  %% form feed
is_unicode_space_or_line_break(16#000D) -> true;  %% carriage return
is_unicode_space_or_line_break(16#0020) -> true;  %% space Most common (normal ASCII space)
is_unicode_space_or_line_break(16#0085) -> true;  %% next line
is_unicode_space_or_line_break(16#00A0) -> true;  %% no-break space, identical to 16#0020, but not a point at which a line may be broken.
is_unicode_space_or_line_break(16#1680) -> true;  %% ogham space mark. Used for interword separation in Ogham text. Normally a vertical line in vertical text or a horizontal line in horizontal text, but may also be a blank space in "stemless" fonts.
is_unicode_space_or_line_break(16#2000) -> true;  %% en quad 8192 - Punctuation Separator space Width of one en.
is_unicode_space_or_line_break(16#2001) -> true;  %% em quad 8193 - Common General Punctuation Separator space Also known as "mutton quad". Width of one em.
is_unicode_space_or_line_break(16#2002) -> true;  %% en space 8194 - Common General Punctuation Separator space Also known as "nut". Width of one en.
is_unicode_space_or_line_break(16#2003) -> true;  %% em space 8195 - Common General Punctuation Separator space. Also known as "mutton". Width of one em.
is_unicode_space_or_line_break(16#2004) -> true;  %% three-per-em space 8196 - Common General Punctuation Separator space. Also known as "thick space". One third of an em wide.
is_unicode_space_or_line_break(16#2005) -> true;  %% four-per-em space 8197 - Common General Punctuation Separator space. Also known as "mid space". One fourth of an em wide.
is_unicode_space_or_line_break(16#2006) -> true;  %% six-per-em space 8198 - Common General Punctuation Separator space. One sixth of an em wide. In computer typography, sometimes equated to 16#2009.
is_unicode_space_or_line_break(16#2007) -> true;  %% figure space 8199 - Common General Punctuation Separator space. Figure space. In fonts with monospaced digits, equal to the width of one digit. HTML/XML named entity: &numsp;
is_unicode_space_or_line_break(16#2008) -> true;  %% punctuation space 8200 - Common General Punctuation Separator space. As wide as the narrow punctuation in a font, HTML/XML named entity: &puncsp;
is_unicode_space_or_line_break(16#2009) -> true;  %% thin space 8201 - Common General Punctuation Separator space. One-fifth (sometimes one-sixth) of an em wide. Recommended for use as a thousands separator for measures made with SI units.
is_unicode_space_or_line_break(16#200A) -> true;  %% hair space 8202 - Common General Punctuation Separator space. Thinner than a thin space. HTML/XML named entity: &hairsp;
is_unicode_space_or_line_break(16#2028) -> true;  %% line separator 8232 - Common General Punctuation Separator space. Line.
is_unicode_space_or_line_break(16#2029) -> true;  %% paragraph separator 8233 - Common General Punctuation Separator space. Paragraph.
is_unicode_space_or_line_break(16#202F) -> true;  %% narrow no-break space 8239 - Common General Punctuation Separator space. Narrow no-break space. Similar in function to 16#00A0 No-Break Space. When used with Mongolian, its width is usually one third of the normal space.
is_unicode_space_or_line_break(16#205F) -> true;  %% medium mathematical space 8287 - Common General Punctuation Separator space. MMSP. Used in mathematical formulae.
is_unicode_space_or_line_break(16#3000) -> true;  %% ideographic space 12288 - CJK Symbols Punctuation Separator, used, for example, in tai tou.
is_unicode_space_or_line_break(_) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% is_ascii
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns true if the specified unicode character is an ASCII character or false otherwise.
is_ascii(UnicodeChar) when UnicodeChar >= 0, UnicodeChar < 256 -> true;
is_ascii(_UnicodeChar) -> false.

%% @doc Returns true if the specified unicode character is an ASCII alphabetic character or false otherwise.
is_ascii_alpha(UnicodeChar) when UnicodeChar >= $a, UnicodeChar =< $z -> true;
is_ascii_alpha(UnicodeChar) when UnicodeChar >= $A, UnicodeChar =< $Z -> true;
is_ascii_alpha(_UnicodeChar) -> false.

%% @doc Returns true if the specified unicode character is an ASCII alphabetic lower case character or false otherwise.
is_ascii_alpha_lower(UnicodeChar) when UnicodeChar >= $a, UnicodeChar =< $z -> true;
is_ascii_alpha_lower(_UnicodeChar) -> false.

%% @doc Returns true if the specified unicode character is an ASCII alphabetic upper case character or false otherwise.
is_ascii_alpha_upper(UnicodeChar) when UnicodeChar >= $A, UnicodeChar =< $Z -> true;
is_ascii_alpha_upper(_UnicodeChar) -> false.

%% @doc Returns true if the specified unicode character is an ASCII numeric character or false otherwise.
is_ascii_numeric(UnicodeChar) when UnicodeChar >= $0, UnicodeChar =< $9 -> true;
is_ascii_numeric(_UnicodeChar) -> false.

%% @doc Returns true if the specified unicode character is an ASCII alpha numeric character or false otherwise.
is_ascii_alpha_numeric(UnicodeChar)
    when UnicodeChar >= $a, UnicodeChar =< $z;
         UnicodeChar >= $A, UnicodeChar =< $Z;
         UnicodeChar >= $0, UnicodeChar =< $9
                                          -> true;
is_ascii_alpha_numeric(_UnicodeChar) -> false.

%% @doc Returns true if the specified unicode character is an ASCII alpha numeric lower character or false otherwise.
is_ascii_alpha_numeric_lower(UnicodeChar)
    when UnicodeChar >= $a, UnicodeChar =< $z;
         UnicodeChar >= $0, UnicodeChar =< $9
                                     -> true;
is_ascii_alpha_numeric_lower(_UnicodeChar) -> false.

%% @doc Returns true if the specified unicode character is an ASCII alpha numeric upper character or false otherwise.
is_ascii_alpha_numeric_upper(UnicodeChar)
    when UnicodeChar >= $a, UnicodeChar =< $z;
         UnicodeChar >= $0, UnicodeChar =< $9
                                           -> true;
is_ascii_alpha_numeric_upper(_UnicodeChar) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% to_
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% LATIN SMALL LETTER A
to_upper(16#0061)->
    16#0041;
%% LATIN SMALL LETTER B
to_upper(16#0062)->
    16#0042;
%% LATIN SMALL LETTER C
to_upper(16#0063)->
    16#0043;
%% LATIN SMALL LETTER D
to_upper(16#0064)->
    16#0044;
%% LATIN SMALL LETTER E
to_upper(16#0065)->
    16#0045;
%% LATIN SMALL LETTER F
to_upper(16#0066)->
    16#0046;
%% LATIN SMALL LETTER G
to_upper(16#0067)->
    16#0047;
%% LATIN SMALL LETTER H
to_upper(16#0068)->
    16#0048;
%% LATIN SMALL LETTER I
to_upper(16#0069)->
    16#0049;
%% LATIN SMALL LETTER J
to_upper(16#006A)->
    16#004A;
%% LATIN SMALL LETTER K
to_upper(16#006B)->
    16#004B;
%% LATIN SMALL LETTER L
to_upper(16#006C)->
    16#004C;
%% LATIN SMALL LETTER M
to_upper(16#006D)->
    16#004D;
%% LATIN SMALL LETTER N
to_upper(16#006E)->
    16#004E;
%% LATIN SMALL LETTER O
to_upper(16#006F)->
    16#004F;
%% LATIN SMALL LETTER P
to_upper(16#0070)->
    16#0050;
%% LATIN SMALL LETTER Q
to_upper(16#0071)->
    16#0051;
%% LATIN SMALL LETTER R
to_upper(16#0072)->
    16#0052;
%% LATIN SMALL LETTER S
to_upper(16#0073)->
    16#0053;
%% LATIN SMALL LETTER T
to_upper(16#0074)->
    16#0054;
%% LATIN SMALL LETTER U
to_upper(16#0075)->
    16#0055;
%% LATIN SMALL LETTER V
to_upper(16#0076)->
    16#0056;
%% LATIN SMALL LETTER W
to_upper(16#0077)->
    16#0057;
%% LATIN SMALL LETTER X
to_upper(16#0078)->
    16#0058;
%% LATIN SMALL LETTER Y
to_upper(16#0079)->
    16#0059;
%% LATIN SMALL LETTER Z
to_upper(16#007A)->
    16#005A;
%% MICRO SIGN
to_upper(16#00B5)->
    16#039C;
%% LATIN SMALL LETTER A WITH GRAVE
to_upper(16#00E0)->
    16#00C0;
%% LATIN SMALL LETTER A WITH ACUTE
to_upper(16#00E1)->
    16#00C1;
%% LATIN SMALL LETTER A WITH CIRCUMFLEX
to_upper(16#00E2)->
    16#00C2;
%% LATIN SMALL LETTER A WITH TILDE
to_upper(16#00E3)->
    16#00C3;
%% LATIN SMALL LETTER A WITH DIAERESIS
to_upper(16#00E4)->
    16#00C4;
%% LATIN SMALL LETTER A WITH RING ABOVE
to_upper(16#00E5)->
    16#00C5;
%% LATIN SMALL LETTER AE
to_upper(16#00E6)->
    16#00C6;
%% LATIN SMALL LETTER C WITH CEDILLA
to_upper(16#00E7)->
    16#00C7;
%% LATIN SMALL LETTER E WITH GRAVE
to_upper(16#00E8)->
    16#00C8;
%% LATIN SMALL LETTER E WITH ACUTE
to_upper(16#00E9)->
    16#00C9;
%% LATIN SMALL LETTER E WITH CIRCUMFLEX
to_upper(16#00EA)->
    16#00CA;
%% LATIN SMALL LETTER E WITH DIAERESIS
to_upper(16#00EB)->
    16#00CB;
%% LATIN SMALL LETTER I WITH GRAVE
to_upper(16#00EC)->
    16#00CC;
%% LATIN SMALL LETTER I WITH ACUTE
to_upper(16#00ED)->
    16#00CD;
%% LATIN SMALL LETTER I WITH CIRCUMFLEX
to_upper(16#00EE)->
    16#00CE;
%% LATIN SMALL LETTER I WITH DIAERESIS
to_upper(16#00EF)->
    16#00CF;
%% LATIN SMALL LETTER ETH
to_upper(16#00F0)->
    16#00D0;
%% LATIN SMALL LETTER N WITH TILDE
to_upper(16#00F1)->
    16#00D1;
%% LATIN SMALL LETTER O WITH GRAVE
to_upper(16#00F2)->
    16#00D2;
%% LATIN SMALL LETTER O WITH ACUTE
to_upper(16#00F3)->
    16#00D3;
%% LATIN SMALL LETTER O WITH CIRCUMFLEX
to_upper(16#00F4)->
    16#00D4;
%% LATIN SMALL LETTER O WITH TILDE
to_upper(16#00F5)->
    16#00D5;
%% LATIN SMALL LETTER O WITH DIAERESIS
to_upper(16#00F6)->
    16#00D6;
%% LATIN SMALL LETTER O WITH STROKE
to_upper(16#00F8)->
    16#00D8;
%% LATIN SMALL LETTER U WITH GRAVE
to_upper(16#00F9)->
    16#00D9;
%% LATIN SMALL LETTER U WITH ACUTE
to_upper(16#00FA)->
    16#00DA;
%% LATIN SMALL LETTER U WITH CIRCUMFLEX
to_upper(16#00FB)->
    16#00DB;
%% LATIN SMALL LETTER U WITH DIAERESIS
to_upper(16#00FC)->
    16#00DC;
%% LATIN SMALL LETTER Y WITH ACUTE
to_upper(16#00FD)->
    16#00DD;
%% LATIN SMALL LETTER THORN
to_upper(16#00FE)->
    16#00DE;
%% LATIN SMALL LETTER Y WITH DIAERESIS
to_upper(16#00FF)->
    16#0178;
%% LATIN SMALL LETTER A WITH MACRON
to_upper(16#0101)->
    16#0100;
%% LATIN SMALL LETTER A WITH BREVE
to_upper(16#0103)->
    16#0102;
%% LATIN SMALL LETTER A WITH OGONEK
to_upper(16#0105)->
    16#0104;
%% LATIN SMALL LETTER C WITH ACUTE
to_upper(16#0107)->
    16#0106;
%% LATIN SMALL LETTER C WITH CIRCUMFLEX
to_upper(16#0109)->
    16#0108;
%% LATIN SMALL LETTER C WITH DOT ABOVE
to_upper(16#010B)->
    16#010A;
%% LATIN SMALL LETTER C WITH CARON
to_upper(16#010D)->
    16#010C;
%% LATIN SMALL LETTER D WITH CARON
to_upper(16#010F)->
    16#010E;
%% LATIN SMALL LETTER D WITH STROKE
to_upper(16#0111)->
    16#0110;
%% LATIN SMALL LETTER E WITH MACRON
to_upper(16#0113)->
    16#0112;
%% LATIN SMALL LETTER E WITH BREVE
to_upper(16#0115)->
    16#0114;
%% LATIN SMALL LETTER E WITH DOT ABOVE
to_upper(16#0117)->
    16#0116;
%% LATIN SMALL LETTER E WITH OGONEK
to_upper(16#0119)->
    16#0118;
%% LATIN SMALL LETTER E WITH CARON
to_upper(16#011B)->
    16#011A;
%% LATIN SMALL LETTER G WITH CIRCUMFLEX
to_upper(16#011D)->
    16#011C;
%% LATIN SMALL LETTER G WITH BREVE
to_upper(16#011F)->
    16#011E;
%% LATIN SMALL LETTER G WITH DOT ABOVE
to_upper(16#0121)->
    16#0120;
%% LATIN SMALL LETTER G WITH CEDILLA
to_upper(16#0123)->
    16#0122;
%% LATIN SMALL LETTER H WITH CIRCUMFLEX
to_upper(16#0125)->
    16#0124;
%% LATIN SMALL LETTER H WITH STROKE
to_upper(16#0127)->
    16#0126;
%% LATIN SMALL LETTER I WITH TILDE
to_upper(16#0129)->
    16#0128;
%% LATIN SMALL LETTER I WITH MACRON
to_upper(16#012B)->
    16#012A;
%% LATIN SMALL LETTER I WITH BREVE
to_upper(16#012D)->
    16#012C;
%% LATIN SMALL LETTER I WITH OGONEK
to_upper(16#012F)->
    16#012E;
%% LATIN SMALL LETTER DOTLESS I
to_upper(16#0131)->
    16#0049;
%% LATIN SMALL LIGATURE IJ
to_upper(16#0133)->
    16#0132;
%% LATIN SMALL LETTER J WITH CIRCUMFLEX
to_upper(16#0135)->
    16#0134;
%% LATIN SMALL LETTER K WITH CEDILLA
to_upper(16#0137)->
    16#0136;
%% LATIN SMALL LETTER L WITH ACUTE
to_upper(16#013A)->
    16#0139;
%% LATIN SMALL LETTER L WITH CEDILLA
to_upper(16#013C)->
    16#013B;
%% LATIN SMALL LETTER L WITH CARON
to_upper(16#013E)->
    16#013D;
%% LATIN SMALL LETTER L WITH MIDDLE DOT
to_upper(16#0140)->
    16#013F;
%% LATIN SMALL LETTER L WITH STROKE
to_upper(16#0142)->
    16#0141;
%% LATIN SMALL LETTER N WITH ACUTE
to_upper(16#0144)->
    16#0143;
%% LATIN SMALL LETTER N WITH CEDILLA
to_upper(16#0146)->
    16#0145;
%% LATIN SMALL LETTER N WITH CARON
to_upper(16#0148)->
    16#0147;
%% LATIN SMALL LETTER ENG
to_upper(16#014B)->
    16#014A;
%% LATIN SMALL LETTER O WITH MACRON
to_upper(16#014D)->
    16#014C;
%% LATIN SMALL LETTER O WITH BREVE
to_upper(16#014F)->
    16#014E;
%% LATIN SMALL LETTER O WITH DOUBLE ACUTE
to_upper(16#0151)->
    16#0150;
%% LATIN SMALL LIGATURE OE
to_upper(16#0153)->
    16#0152;
%% LATIN SMALL LETTER R WITH ACUTE
to_upper(16#0155)->
    16#0154;
%% LATIN SMALL LETTER R WITH CEDILLA
to_upper(16#0157)->
    16#0156;
%% LATIN SMALL LETTER R WITH CARON
to_upper(16#0159)->
    16#0158;
%% LATIN SMALL LETTER S WITH ACUTE
to_upper(16#015B)->
    16#015A;
%% LATIN SMALL LETTER S WITH CIRCUMFLEX
to_upper(16#015D)->
    16#015C;
%% LATIN SMALL LETTER S WITH CEDILLA
to_upper(16#015F)->
    16#015E;
%% LATIN SMALL LETTER S WITH CARON
to_upper(16#0161)->
    16#0160;
%% LATIN SMALL LETTER T WITH CEDILLA
to_upper(16#0163)->
    16#0162;
%% LATIN SMALL LETTER T WITH CARON
to_upper(16#0165)->
    16#0164;
%% LATIN SMALL LETTER T WITH STROKE
to_upper(16#0167)->
    16#0166;
%% LATIN SMALL LETTER U WITH TILDE
to_upper(16#0169)->
    16#0168;
%% LATIN SMALL LETTER U WITH MACRON
to_upper(16#016B)->
    16#016A;
%% LATIN SMALL LETTER U WITH BREVE
to_upper(16#016D)->
    16#016C;
%% LATIN SMALL LETTER U WITH RING ABOVE
to_upper(16#016F)->
    16#016E;
%% LATIN SMALL LETTER U WITH DOUBLE ACUTE
to_upper(16#0171)->
    16#0170;
%% LATIN SMALL LETTER U WITH OGONEK
to_upper(16#0173)->
    16#0172;
%% LATIN SMALL LETTER W WITH CIRCUMFLEX
to_upper(16#0175)->
    16#0174;
%% LATIN SMALL LETTER Y WITH CIRCUMFLEX
to_upper(16#0177)->
    16#0176;
%% LATIN SMALL LETTER Z WITH ACUTE
to_upper(16#017A)->
    16#0179;
%% LATIN SMALL LETTER Z WITH DOT ABOVE
to_upper(16#017C)->
    16#017B;
%% LATIN SMALL LETTER Z WITH CARON
to_upper(16#017E)->
    16#017D;
%% LATIN SMALL LETTER LONG S
to_upper(16#017F)->
    16#0053;
%% LATIN SMALL LETTER B WITH STROKE
to_upper(16#0180)->
    16#0243;
%% LATIN SMALL LETTER B WITH TOPBAR
to_upper(16#0183)->
    16#0182;
%% LATIN SMALL LETTER TONE SIX
to_upper(16#0185)->
    16#0184;
%% LATIN SMALL LETTER C WITH HOOK
to_upper(16#0188)->
    16#0187;
%% LATIN SMALL LETTER D WITH TOPBAR
to_upper(16#018C)->
    16#018B;
%% LATIN SMALL LETTER F WITH HOOK
to_upper(16#0192)->
    16#0191;
%% LATIN SMALL LETTER HV
to_upper(16#0195)->
    16#01F6;
%% LATIN SMALL LETTER K WITH HOOK
to_upper(16#0199)->
    16#0198;
%% LATIN SMALL LETTER L WITH BAR
to_upper(16#019A)->
    16#023D;
%% LATIN SMALL LETTER N WITH LONG RIGHT LEG
to_upper(16#019E)->
    16#0220;
%% LATIN SMALL LETTER O WITH HORN
to_upper(16#01A1)->
    16#01A0;
%% LATIN SMALL LETTER OI
to_upper(16#01A3)->
    16#01A2;
%% LATIN SMALL LETTER P WITH HOOK
to_upper(16#01A5)->
    16#01A4;
%% LATIN SMALL LETTER TONE TWO
to_upper(16#01A8)->
    16#01A7;
%% LATIN SMALL LETTER T WITH HOOK
to_upper(16#01AD)->
    16#01AC;
%% LATIN SMALL LETTER U WITH HORN
to_upper(16#01B0)->
    16#01AF;
%% LATIN SMALL LETTER Y WITH HOOK
to_upper(16#01B4)->
    16#01B3;
%% LATIN SMALL LETTER Z WITH STROKE
to_upper(16#01B6)->
    16#01B5;
%% LATIN SMALL LETTER EZH REVERSED
to_upper(16#01B9)->
    16#01B8;
%% LATIN SMALL LETTER TONE FIVE
to_upper(16#01BD)->
    16#01BC;
%% LATIN LETTER WYNN
to_upper(16#01BF)->
    16#01F7;
%% LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON
to_upper(16#01C5)->
    16#01C4;
%% LATIN SMALL LETTER DZ WITH CARON
to_upper(16#01C6)->
    16#01C4;
%% LATIN CAPITAL LETTER L WITH SMALL LETTER J
to_upper(16#01C8)->
    16#01C7;
%% LATIN SMALL LETTER LJ
to_upper(16#01C9)->
    16#01C7;
%% LATIN CAPITAL LETTER N WITH SMALL LETTER J
to_upper(16#01CB)->
    16#01CA;
%% LATIN SMALL LETTER NJ
to_upper(16#01CC)->
    16#01CA;
%% LATIN SMALL LETTER A WITH CARON
to_upper(16#01CE)->
    16#01CD;
%% LATIN SMALL LETTER I WITH CARON
to_upper(16#01D0)->
    16#01CF;
%% LATIN SMALL LETTER O WITH CARON
to_upper(16#01D2)->
    16#01D1;
%% LATIN SMALL LETTER U WITH CARON
to_upper(16#01D4)->
    16#01D3;
%% LATIN SMALL LETTER U WITH DIAERESIS AND MACRON
to_upper(16#01D6)->
    16#01D5;
%% LATIN SMALL LETTER U WITH DIAERESIS AND ACUTE
to_upper(16#01D8)->
    16#01D7;
%% LATIN SMALL LETTER U WITH DIAERESIS AND CARON
to_upper(16#01DA)->
    16#01D9;
%% LATIN SMALL LETTER U WITH DIAERESIS AND GRAVE
to_upper(16#01DC)->
    16#01DB;
%% LATIN SMALL LETTER TURNED E
to_upper(16#01DD)->
    16#018E;
%% LATIN SMALL LETTER A WITH DIAERESIS AND MACRON
to_upper(16#01DF)->
    16#01DE;
%% LATIN SMALL LETTER A WITH DOT ABOVE AND MACRON
to_upper(16#01E1)->
    16#01E0;
%% LATIN SMALL LETTER AE WITH MACRON
to_upper(16#01E3)->
    16#01E2;
%% LATIN SMALL LETTER G WITH STROKE
to_upper(16#01E5)->
    16#01E4;
%% LATIN SMALL LETTER G WITH CARON
to_upper(16#01E7)->
    16#01E6;
%% LATIN SMALL LETTER K WITH CARON
to_upper(16#01E9)->
    16#01E8;
%% LATIN SMALL LETTER O WITH OGONEK
to_upper(16#01EB)->
    16#01EA;
%% LATIN SMALL LETTER O WITH OGONEK AND MACRON
to_upper(16#01ED)->
    16#01EC;
%% LATIN SMALL LETTER EZH WITH CARON
to_upper(16#01EF)->
    16#01EE;
%% LATIN CAPITAL LETTER D WITH SMALL LETTER Z
to_upper(16#01F2)->
    16#01F1;
%% LATIN SMALL LETTER DZ
to_upper(16#01F3)->
    16#01F1;
%% LATIN SMALL LETTER G WITH ACUTE
to_upper(16#01F5)->
    16#01F4;
%% LATIN SMALL LETTER N WITH GRAVE
to_upper(16#01F9)->
    16#01F8;
%% LATIN SMALL LETTER A WITH RING ABOVE AND ACUTE
to_upper(16#01FB)->
    16#01FA;
%% LATIN SMALL LETTER AE WITH ACUTE
to_upper(16#01FD)->
    16#01FC;
%% LATIN SMALL LETTER O WITH STROKE AND ACUTE
to_upper(16#01FF)->
    16#01FE;
%% LATIN SMALL LETTER A WITH DOUBLE GRAVE
to_upper(16#0201)->
    16#0200;
%% LATIN SMALL LETTER A WITH INVERTED BREVE
to_upper(16#0203)->
    16#0202;
%% LATIN SMALL LETTER E WITH DOUBLE GRAVE
to_upper(16#0205)->
    16#0204;
%% LATIN SMALL LETTER E WITH INVERTED BREVE
to_upper(16#0207)->
    16#0206;
%% LATIN SMALL LETTER I WITH DOUBLE GRAVE
to_upper(16#0209)->
    16#0208;
%% LATIN SMALL LETTER I WITH INVERTED BREVE
to_upper(16#020B)->
    16#020A;
%% LATIN SMALL LETTER O WITH DOUBLE GRAVE
to_upper(16#020D)->
    16#020C;
%% LATIN SMALL LETTER O WITH INVERTED BREVE
to_upper(16#020F)->
    16#020E;
%% LATIN SMALL LETTER R WITH DOUBLE GRAVE
to_upper(16#0211)->
    16#0210;
%% LATIN SMALL LETTER R WITH INVERTED BREVE
to_upper(16#0213)->
    16#0212;
%% LATIN SMALL LETTER U WITH DOUBLE GRAVE
to_upper(16#0215)->
    16#0214;
%% LATIN SMALL LETTER U WITH INVERTED BREVE
to_upper(16#0217)->
    16#0216;
%% LATIN SMALL LETTER S WITH COMMA BELOW
to_upper(16#0219)->
    16#0218;
%% LATIN SMALL LETTER T WITH COMMA BELOW
to_upper(16#021B)->
    16#021A;
%% LATIN SMALL LETTER YOGH
to_upper(16#021D)->
    16#021C;
%% LATIN SMALL LETTER H WITH CARON
to_upper(16#021F)->
    16#021E;
%% LATIN SMALL LETTER OU
to_upper(16#0223)->
    16#0222;
%% LATIN SMALL LETTER Z WITH HOOK
to_upper(16#0225)->
    16#0224;
%% LATIN SMALL LETTER A WITH DOT ABOVE
to_upper(16#0227)->
    16#0226;
%% LATIN SMALL LETTER E WITH CEDILLA
to_upper(16#0229)->
    16#0228;
%% LATIN SMALL LETTER O WITH DIAERESIS AND MACRON
to_upper(16#022B)->
    16#022A;
%% LATIN SMALL LETTER O WITH TILDE AND MACRON
to_upper(16#022D)->
    16#022C;
%% LATIN SMALL LETTER O WITH DOT ABOVE
to_upper(16#022F)->
    16#022E;
%% LATIN SMALL LETTER O WITH DOT ABOVE AND MACRON
to_upper(16#0231)->
    16#0230;
%% LATIN SMALL LETTER Y WITH MACRON
to_upper(16#0233)->
    16#0232;
%% LATIN SMALL LETTER C WITH STROKE
to_upper(16#023C)->
    16#023B;
%% LATIN SMALL LETTER S WITH SWASH TAIL
to_upper(16#023F)->
    16#2C7E;
%% LATIN SMALL LETTER Z WITH SWASH TAIL
to_upper(16#0240)->
    16#2C7F;
%% LATIN SMALL LETTER GLOTTAL STOP
to_upper(16#0242)->
    16#0241;
%% LATIN SMALL LETTER E WITH STROKE
to_upper(16#0247)->
    16#0246;
%% LATIN SMALL LETTER J WITH STROKE
to_upper(16#0249)->
    16#0248;
%% LATIN SMALL LETTER Q WITH HOOK TAIL
to_upper(16#024B)->
    16#024A;
%% LATIN SMALL LETTER R WITH STROKE
to_upper(16#024D)->
    16#024C;
%% LATIN SMALL LETTER Y WITH STROKE
to_upper(16#024F)->
    16#024E;
%% LATIN SMALL LETTER TURNED A
to_upper(16#0250)->
    16#2C6F;
%% LATIN SMALL LETTER ALPHA
to_upper(16#0251)->
    16#2C6D;
%% LATIN SMALL LETTER TURNED ALPHA
to_upper(16#0252)->
    16#2C70;
%% LATIN SMALL LETTER B WITH HOOK
to_upper(16#0253)->
    16#0181;
%% LATIN SMALL LETTER OPEN O
to_upper(16#0254)->
    16#0186;
%% LATIN SMALL LETTER D WITH TAIL
to_upper(16#0256)->
    16#0189;
%% LATIN SMALL LETTER D WITH HOOK
to_upper(16#0257)->
    16#018A;
%% LATIN SMALL LETTER SCHWA
to_upper(16#0259)->
    16#018F;
%% LATIN SMALL LETTER OPEN E
to_upper(16#025B)->
    16#0190;
%% LATIN SMALL LETTER G WITH HOOK
to_upper(16#0260)->
    16#0193;
%% LATIN SMALL LETTER GAMMA
to_upper(16#0263)->
    16#0194;
%% LATIN SMALL LETTER TURNED H
to_upper(16#0265)->
    16#A78D;
%% LATIN SMALL LETTER H WITH HOOK
to_upper(16#0266)->
    16#A7AA;
%% LATIN SMALL LETTER I WITH STROKE
to_upper(16#0268)->
    16#0197;
%% LATIN SMALL LETTER IOTA
to_upper(16#0269)->
    16#0196;
%% LATIN SMALL LETTER L WITH MIDDLE TILDE
to_upper(16#026B)->
    16#2C62;
%% LATIN SMALL LETTER TURNED M
to_upper(16#026F)->
    16#019C;
%% LATIN SMALL LETTER M WITH HOOK
to_upper(16#0271)->
    16#2C6E;
%% LATIN SMALL LETTER N WITH LEFT HOOK
to_upper(16#0272)->
    16#019D;
%% LATIN SMALL LETTER BARRED O
to_upper(16#0275)->
    16#019F;
%% LATIN SMALL LETTER R WITH TAIL
to_upper(16#027D)->
    16#2C64;
%% LATIN LETTER SMALL CAPITAL R
to_upper(16#0280)->
    16#01A6;
%% LATIN SMALL LETTER ESH
to_upper(16#0283)->
    16#01A9;
%% LATIN SMALL LETTER T WITH RETROFLEX HOOK
to_upper(16#0288)->
    16#01AE;
%% LATIN SMALL LETTER U BAR
to_upper(16#0289)->
    16#0244;
%% LATIN SMALL LETTER UPSILON
to_upper(16#028A)->
    16#01B1;
%% LATIN SMALL LETTER V WITH HOOK
to_upper(16#028B)->
    16#01B2;
%% LATIN SMALL LETTER TURNED V
to_upper(16#028C)->
    16#0245;
%% LATIN SMALL LETTER EZH
to_upper(16#0292)->
    16#01B7;
%% COMBINING GREEK YPOGEGRAMMENI
to_upper(16#0345)->
    16#0399;
%% GREEK SMALL LETTER HETA
to_upper(16#0371)->
    16#0370;
%% GREEK SMALL LETTER ARCHAIC SAMPI
to_upper(16#0373)->
    16#0372;
%% GREEK SMALL LETTER PAMPHYLIAN DIGAMMA
to_upper(16#0377)->
    16#0376;
%% GREEK SMALL REVERSED LUNATE SIGMA SYMBOL
to_upper(16#037B)->
    16#03FD;
%% GREEK SMALL DOTTED LUNATE SIGMA SYMBOL
to_upper(16#037C)->
    16#03FE;
%% GREEK SMALL REVERSED DOTTED LUNATE SIGMA SYMBOL
to_upper(16#037D)->
    16#03FF;
%% GREEK SMALL LETTER ALPHA WITH TONOS
to_upper(16#03AC)->
    16#0386;
%% GREEK SMALL LETTER EPSILON WITH TONOS
to_upper(16#03AD)->
    16#0388;
%% GREEK SMALL LETTER ETA WITH TONOS
to_upper(16#03AE)->
    16#0389;
%% GREEK SMALL LETTER IOTA WITH TONOS
to_upper(16#03AF)->
    16#038A;
%% GREEK SMALL LETTER ALPHA
to_upper(16#03B1)->
    16#0391;
%% GREEK SMALL LETTER BETA
to_upper(16#03B2)->
    16#0392;
%% GREEK SMALL LETTER GAMMA
to_upper(16#03B3)->
    16#0393;
%% GREEK SMALL LETTER DELTA
to_upper(16#03B4)->
    16#0394;
%% GREEK SMALL LETTER EPSILON
to_upper(16#03B5)->
    16#0395;
%% GREEK SMALL LETTER ZETA
to_upper(16#03B6)->
    16#0396;
%% GREEK SMALL LETTER ETA
to_upper(16#03B7)->
    16#0397;
%% GREEK SMALL LETTER THETA
to_upper(16#03B8)->
    16#0398;
%% GREEK SMALL LETTER IOTA
to_upper(16#03B9)->
    16#0399;
%% GREEK SMALL LETTER KAPPA
to_upper(16#03BA)->
    16#039A;
%% GREEK SMALL LETTER LAMDA
to_upper(16#03BB)->
    16#039B;
%% GREEK SMALL LETTER MU
to_upper(16#03BC)->
    16#039C;
%% GREEK SMALL LETTER NU
to_upper(16#03BD)->
    16#039D;
%% GREEK SMALL LETTER XI
to_upper(16#03BE)->
    16#039E;
%% GREEK SMALL LETTER OMICRON
to_upper(16#03BF)->
    16#039F;
%% GREEK SMALL LETTER PI
to_upper(16#03C0)->
    16#03A0;
%% GREEK SMALL LETTER RHO
to_upper(16#03C1)->
    16#03A1;
%% GREEK SMALL LETTER FINAL SIGMA
to_upper(16#03C2)->
    16#03A3;
%% GREEK SMALL LETTER SIGMA
to_upper(16#03C3)->
    16#03A3;
%% GREEK SMALL LETTER TAU
to_upper(16#03C4)->
    16#03A4;
%% GREEK SMALL LETTER UPSILON
to_upper(16#03C5)->
    16#03A5;
%% GREEK SMALL LETTER PHI
to_upper(16#03C6)->
    16#03A6;
%% GREEK SMALL LETTER CHI
to_upper(16#03C7)->
    16#03A7;
%% GREEK SMALL LETTER PSI
to_upper(16#03C8)->
    16#03A8;
%% GREEK SMALL LETTER OMEGA
to_upper(16#03C9)->
    16#03A9;
%% GREEK SMALL LETTER IOTA WITH DIALYTIKA
to_upper(16#03CA)->
    16#03AA;
%% GREEK SMALL LETTER UPSILON WITH DIALYTIKA
to_upper(16#03CB)->
    16#03AB;
%% GREEK SMALL LETTER OMICRON WITH TONOS
to_upper(16#03CC)->
    16#038C;
%% GREEK SMALL LETTER UPSILON WITH TONOS
to_upper(16#03CD)->
    16#038E;
%% GREEK SMALL LETTER OMEGA WITH TONOS
to_upper(16#03CE)->
    16#038F;
%% GREEK BETA SYMBOL
to_upper(16#03D0)->
    16#0392;
%% GREEK THETA SYMBOL
to_upper(16#03D1)->
    16#0398;
%% GREEK PHI SYMBOL
to_upper(16#03D5)->
    16#03A6;
%% GREEK PI SYMBOL
to_upper(16#03D6)->
    16#03A0;
%% GREEK KAI SYMBOL
to_upper(16#03D7)->
    16#03CF;
%% GREEK SMALL LETTER ARCHAIC KOPPA
to_upper(16#03D9)->
    16#03D8;
%% GREEK SMALL LETTER STIGMA
to_upper(16#03DB)->
    16#03DA;
%% GREEK SMALL LETTER DIGAMMA
to_upper(16#03DD)->
    16#03DC;
%% GREEK SMALL LETTER KOPPA
to_upper(16#03DF)->
    16#03DE;
%% GREEK SMALL LETTER SAMPI
to_upper(16#03E1)->
    16#03E0;
%% COPTIC SMALL LETTER SHEI
to_upper(16#03E3)->
    16#03E2;
%% COPTIC SMALL LETTER FEI
to_upper(16#03E5)->
    16#03E4;
%% COPTIC SMALL LETTER KHEI
to_upper(16#03E7)->
    16#03E6;
%% COPTIC SMALL LETTER HORI
to_upper(16#03E9)->
    16#03E8;
%% COPTIC SMALL LETTER GANGIA
to_upper(16#03EB)->
    16#03EA;
%% COPTIC SMALL LETTER SHIMA
to_upper(16#03ED)->
    16#03EC;
%% COPTIC SMALL LETTER DEI
to_upper(16#03EF)->
    16#03EE;
%% GREEK KAPPA SYMBOL
to_upper(16#03F0)->
    16#039A;
%% GREEK RHO SYMBOL
to_upper(16#03F1)->
    16#03A1;
%% GREEK LUNATE SIGMA SYMBOL
to_upper(16#03F2)->
    16#03F9;
%% GREEK LUNATE EPSILON SYMBOL
to_upper(16#03F5)->
    16#0395;
%% GREEK SMALL LETTER SHO
to_upper(16#03F8)->
    16#03F7;
%% GREEK SMALL LETTER SAN
to_upper(16#03FB)->
    16#03FA;
%% CYRILLIC SMALL LETTER A
to_upper(16#0430)->
    16#0410;
%% CYRILLIC SMALL LETTER BE
to_upper(16#0431)->
    16#0411;
%% CYRILLIC SMALL LETTER VE
to_upper(16#0432)->
    16#0412;
%% CYRILLIC SMALL LETTER GHE
to_upper(16#0433)->
    16#0413;
%% CYRILLIC SMALL LETTER DE
to_upper(16#0434)->
    16#0414;
%% CYRILLIC SMALL LETTER IE
to_upper(16#0435)->
    16#0415;
%% CYRILLIC SMALL LETTER ZHE
to_upper(16#0436)->
    16#0416;
%% CYRILLIC SMALL LETTER ZE
to_upper(16#0437)->
    16#0417;
%% CYRILLIC SMALL LETTER I
to_upper(16#0438)->
    16#0418;
%% CYRILLIC SMALL LETTER SHORT I
to_upper(16#0439)->
    16#0419;
%% CYRILLIC SMALL LETTER KA
to_upper(16#043A)->
    16#041A;
%% CYRILLIC SMALL LETTER EL
to_upper(16#043B)->
    16#041B;
%% CYRILLIC SMALL LETTER EM
to_upper(16#043C)->
    16#041C;
%% CYRILLIC SMALL LETTER EN
to_upper(16#043D)->
    16#041D;
%% CYRILLIC SMALL LETTER O
to_upper(16#043E)->
    16#041E;
%% CYRILLIC SMALL LETTER PE
to_upper(16#043F)->
    16#041F;
%% CYRILLIC SMALL LETTER ER
to_upper(16#0440)->
    16#0420;
%% CYRILLIC SMALL LETTER ES
to_upper(16#0441)->
    16#0421;
%% CYRILLIC SMALL LETTER TE
to_upper(16#0442)->
    16#0422;
%% CYRILLIC SMALL LETTER U
to_upper(16#0443)->
    16#0423;
%% CYRILLIC SMALL LETTER EF
to_upper(16#0444)->
    16#0424;
%% CYRILLIC SMALL LETTER HA
to_upper(16#0445)->
    16#0425;
%% CYRILLIC SMALL LETTER TSE
to_upper(16#0446)->
    16#0426;
%% CYRILLIC SMALL LETTER CHE
to_upper(16#0447)->
    16#0427;
%% CYRILLIC SMALL LETTER SHA
to_upper(16#0448)->
    16#0428;
%% CYRILLIC SMALL LETTER SHCHA
to_upper(16#0449)->
    16#0429;
%% CYRILLIC SMALL LETTER HARD SIGN
to_upper(16#044A)->
    16#042A;
%% CYRILLIC SMALL LETTER YERU
to_upper(16#044B)->
    16#042B;
%% CYRILLIC SMALL LETTER SOFT SIGN
to_upper(16#044C)->
    16#042C;
%% CYRILLIC SMALL LETTER E
to_upper(16#044D)->
    16#042D;
%% CYRILLIC SMALL LETTER YU
to_upper(16#044E)->
    16#042E;
%% CYRILLIC SMALL LETTER YA
to_upper(16#044F)->
    16#042F;
%% CYRILLIC SMALL LETTER IE WITH GRAVE
to_upper(16#0450)->
    16#0400;
%% CYRILLIC SMALL LETTER IO
to_upper(16#0451)->
    16#0401;
%% CYRILLIC SMALL LETTER DJE
to_upper(16#0452)->
    16#0402;
%% CYRILLIC SMALL LETTER GJE
to_upper(16#0453)->
    16#0403;
%% CYRILLIC SMALL LETTER UKRAINIAN IE
to_upper(16#0454)->
    16#0404;
%% CYRILLIC SMALL LETTER DZE
to_upper(16#0455)->
    16#0405;
%% CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
to_upper(16#0456)->
    16#0406;
%% CYRILLIC SMALL LETTER YI
to_upper(16#0457)->
    16#0407;
%% CYRILLIC SMALL LETTER JE
to_upper(16#0458)->
    16#0408;
%% CYRILLIC SMALL LETTER LJE
to_upper(16#0459)->
    16#0409;
%% CYRILLIC SMALL LETTER NJE
to_upper(16#045A)->
    16#040A;
%% CYRILLIC SMALL LETTER TSHE
to_upper(16#045B)->
    16#040B;
%% CYRILLIC SMALL LETTER KJE
to_upper(16#045C)->
    16#040C;
%% CYRILLIC SMALL LETTER I WITH GRAVE
to_upper(16#045D)->
    16#040D;
%% CYRILLIC SMALL LETTER SHORT U
to_upper(16#045E)->
    16#040E;
%% CYRILLIC SMALL LETTER DZHE
to_upper(16#045F)->
    16#040F;
%% CYRILLIC SMALL LETTER OMEGA
to_upper(16#0461)->
    16#0460;
%% CYRILLIC SMALL LETTER YAT
to_upper(16#0463)->
    16#0462;
%% CYRILLIC SMALL LETTER IOTIFIED E
to_upper(16#0465)->
    16#0464;
%% CYRILLIC SMALL LETTER LITTLE YUS
to_upper(16#0467)->
    16#0466;
%% CYRILLIC SMALL LETTER IOTIFIED LITTLE YUS
to_upper(16#0469)->
    16#0468;
%% CYRILLIC SMALL LETTER BIG YUS
to_upper(16#046B)->
    16#046A;
%% CYRILLIC SMALL LETTER IOTIFIED BIG YUS
to_upper(16#046D)->
    16#046C;
%% CYRILLIC SMALL LETTER KSI
to_upper(16#046F)->
    16#046E;
%% CYRILLIC SMALL LETTER PSI
to_upper(16#0471)->
    16#0470;
%% CYRILLIC SMALL LETTER FITA
to_upper(16#0473)->
    16#0472;
%% CYRILLIC SMALL LETTER IZHITSA
to_upper(16#0475)->
    16#0474;
%% CYRILLIC SMALL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT
to_upper(16#0477)->
    16#0476;
%% CYRILLIC SMALL LETTER UK
to_upper(16#0479)->
    16#0478;
%% CYRILLIC SMALL LETTER ROUND OMEGA
to_upper(16#047B)->
    16#047A;
%% CYRILLIC SMALL LETTER OMEGA WITH TITLO
to_upper(16#047D)->
    16#047C;
%% CYRILLIC SMALL LETTER OT
to_upper(16#047F)->
    16#047E;
%% CYRILLIC SMALL LETTER KOPPA
to_upper(16#0481)->
    16#0480;
%% CYRILLIC SMALL LETTER SHORT I WITH TAIL
to_upper(16#048B)->
    16#048A;
%% CYRILLIC SMALL LETTER SEMISOFT SIGN
to_upper(16#048D)->
    16#048C;
%% CYRILLIC SMALL LETTER ER WITH TICK
to_upper(16#048F)->
    16#048E;
%% CYRILLIC SMALL LETTER GHE WITH UPTURN
to_upper(16#0491)->
    16#0490;
%% CYRILLIC SMALL LETTER GHE WITH STROKE
to_upper(16#0493)->
    16#0492;
%% CYRILLIC SMALL LETTER GHE WITH MIDDLE HOOK
to_upper(16#0495)->
    16#0494;
%% CYRILLIC SMALL LETTER ZHE WITH DESCENDER
to_upper(16#0497)->
    16#0496;
%% CYRILLIC SMALL LETTER ZE WITH DESCENDER
to_upper(16#0499)->
    16#0498;
%% CYRILLIC SMALL LETTER KA WITH DESCENDER
to_upper(16#049B)->
    16#049A;
%% CYRILLIC SMALL LETTER KA WITH VERTICAL STROKE
to_upper(16#049D)->
    16#049C;
%% CYRILLIC SMALL LETTER KA WITH STROKE
to_upper(16#049F)->
    16#049E;
%% CYRILLIC SMALL LETTER BASHKIR KA
to_upper(16#04A1)->
    16#04A0;
%% CYRILLIC SMALL LETTER EN WITH DESCENDER
to_upper(16#04A3)->
    16#04A2;
%% CYRILLIC SMALL LIGATURE EN GHE
to_upper(16#04A5)->
    16#04A4;
%% CYRILLIC SMALL LETTER PE WITH MIDDLE HOOK
to_upper(16#04A7)->
    16#04A6;
%% CYRILLIC SMALL LETTER ABKHASIAN HA
to_upper(16#04A9)->
    16#04A8;
%% CYRILLIC SMALL LETTER ES WITH DESCENDER
to_upper(16#04AB)->
    16#04AA;
%% CYRILLIC SMALL LETTER TE WITH DESCENDER
to_upper(16#04AD)->
    16#04AC;
%% CYRILLIC SMALL LETTER STRAIGHT U
to_upper(16#04AF)->
    16#04AE;
%% CYRILLIC SMALL LETTER STRAIGHT U WITH STROKE
to_upper(16#04B1)->
    16#04B0;
%% CYRILLIC SMALL LETTER HA WITH DESCENDER
to_upper(16#04B3)->
    16#04B2;
%% CYRILLIC SMALL LIGATURE TE TSE
to_upper(16#04B5)->
    16#04B4;
%% CYRILLIC SMALL LETTER CHE WITH DESCENDER
to_upper(16#04B7)->
    16#04B6;
%% CYRILLIC SMALL LETTER CHE WITH VERTICAL STROKE
to_upper(16#04B9)->
    16#04B8;
%% CYRILLIC SMALL LETTER SHHA
to_upper(16#04BB)->
    16#04BA;
%% CYRILLIC SMALL LETTER ABKHASIAN CHE
to_upper(16#04BD)->
    16#04BC;
%% CYRILLIC SMALL LETTER ABKHASIAN CHE WITH DESCENDER
to_upper(16#04BF)->
    16#04BE;
%% CYRILLIC SMALL LETTER ZHE WITH BREVE
to_upper(16#04C2)->
    16#04C1;
%% CYRILLIC SMALL LETTER KA WITH HOOK
to_upper(16#04C4)->
    16#04C3;
%% CYRILLIC SMALL LETTER EL WITH TAIL
to_upper(16#04C6)->
    16#04C5;
%% CYRILLIC SMALL LETTER EN WITH HOOK
to_upper(16#04C8)->
    16#04C7;
%% CYRILLIC SMALL LETTER EN WITH TAIL
to_upper(16#04CA)->
    16#04C9;
%% CYRILLIC SMALL LETTER KHAKASSIAN CHE
to_upper(16#04CC)->
    16#04CB;
%% CYRILLIC SMALL LETTER EM WITH TAIL
to_upper(16#04CE)->
    16#04CD;
%% CYRILLIC SMALL LETTER PALOCHKA
to_upper(16#04CF)->
    16#04C0;
%% CYRILLIC SMALL LETTER A WITH BREVE
to_upper(16#04D1)->
    16#04D0;
%% CYRILLIC SMALL LETTER A WITH DIAERESIS
to_upper(16#04D3)->
    16#04D2;
%% CYRILLIC SMALL LIGATURE A IE
to_upper(16#04D5)->
    16#04D4;
%% CYRILLIC SMALL LETTER IE WITH BREVE
to_upper(16#04D7)->
    16#04D6;
%% CYRILLIC SMALL LETTER SCHWA
to_upper(16#04D9)->
    16#04D8;
%% CYRILLIC SMALL LETTER SCHWA WITH DIAERESIS
to_upper(16#04DB)->
    16#04DA;
%% CYRILLIC SMALL LETTER ZHE WITH DIAERESIS
to_upper(16#04DD)->
    16#04DC;
%% CYRILLIC SMALL LETTER ZE WITH DIAERESIS
to_upper(16#04DF)->
    16#04DE;
%% CYRILLIC SMALL LETTER ABKHASIAN DZE
to_upper(16#04E1)->
    16#04E0;
%% CYRILLIC SMALL LETTER I WITH MACRON
to_upper(16#04E3)->
    16#04E2;
%% CYRILLIC SMALL LETTER I WITH DIAERESIS
to_upper(16#04E5)->
    16#04E4;
%% CYRILLIC SMALL LETTER O WITH DIAERESIS
to_upper(16#04E7)->
    16#04E6;
%% CYRILLIC SMALL LETTER BARRED O
to_upper(16#04E9)->
    16#04E8;
%% CYRILLIC SMALL LETTER BARRED O WITH DIAERESIS
to_upper(16#04EB)->
    16#04EA;
%% CYRILLIC SMALL LETTER E WITH DIAERESIS
to_upper(16#04ED)->
    16#04EC;
%% CYRILLIC SMALL LETTER U WITH MACRON
to_upper(16#04EF)->
    16#04EE;
%% CYRILLIC SMALL LETTER U WITH DIAERESIS
to_upper(16#04F1)->
    16#04F0;
%% CYRILLIC SMALL LETTER U WITH DOUBLE ACUTE
to_upper(16#04F3)->
    16#04F2;
%% CYRILLIC SMALL LETTER CHE WITH DIAERESIS
to_upper(16#04F5)->
    16#04F4;
%% CYRILLIC SMALL LETTER GHE WITH DESCENDER
to_upper(16#04F7)->
    16#04F6;
%% CYRILLIC SMALL LETTER YERU WITH DIAERESIS
to_upper(16#04F9)->
    16#04F8;
%% CYRILLIC SMALL LETTER GHE WITH STROKE AND HOOK
to_upper(16#04FB)->
    16#04FA;
%% CYRILLIC SMALL LETTER HA WITH HOOK
to_upper(16#04FD)->
    16#04FC;
%% CYRILLIC SMALL LETTER HA WITH STROKE
to_upper(16#04FF)->
    16#04FE;
%% CYRILLIC SMALL LETTER KOMI DE
to_upper(16#0501)->
    16#0500;
%% CYRILLIC SMALL LETTER KOMI DJE
to_upper(16#0503)->
    16#0502;
%% CYRILLIC SMALL LETTER KOMI ZJE
to_upper(16#0505)->
    16#0504;
%% CYRILLIC SMALL LETTER KOMI DZJE
to_upper(16#0507)->
    16#0506;
%% CYRILLIC SMALL LETTER KOMI LJE
to_upper(16#0509)->
    16#0508;
%% CYRILLIC SMALL LETTER KOMI NJE
to_upper(16#050B)->
    16#050A;
%% CYRILLIC SMALL LETTER KOMI SJE
to_upper(16#050D)->
    16#050C;
%% CYRILLIC SMALL LETTER KOMI TJE
to_upper(16#050F)->
    16#050E;
%% CYRILLIC SMALL LETTER REVERSED ZE
to_upper(16#0511)->
    16#0510;
%% CYRILLIC SMALL LETTER EL WITH HOOK
to_upper(16#0513)->
    16#0512;
%% CYRILLIC SMALL LETTER LHA
to_upper(16#0515)->
    16#0514;
%% CYRILLIC SMALL LETTER RHA
to_upper(16#0517)->
    16#0516;
%% CYRILLIC SMALL LETTER YAE
to_upper(16#0519)->
    16#0518;
%% CYRILLIC SMALL LETTER QA
to_upper(16#051B)->
    16#051A;
%% CYRILLIC SMALL LETTER WE
to_upper(16#051D)->
    16#051C;
%% CYRILLIC SMALL LETTER ALEUT KA
to_upper(16#051F)->
    16#051E;
%% CYRILLIC SMALL LETTER EL WITH MIDDLE HOOK
to_upper(16#0521)->
    16#0520;
%% CYRILLIC SMALL LETTER EN WITH MIDDLE HOOK
to_upper(16#0523)->
    16#0522;
%% CYRILLIC SMALL LETTER PE WITH DESCENDER
to_upper(16#0525)->
    16#0524;
%% CYRILLIC SMALL LETTER SHHA WITH DESCENDER
to_upper(16#0527)->
    16#0526;
%% ARMENIAN SMALL LETTER AYB
to_upper(16#0561)->
    16#0531;
%% ARMENIAN SMALL LETTER BEN
to_upper(16#0562)->
    16#0532;
%% ARMENIAN SMALL LETTER GIM
to_upper(16#0563)->
    16#0533;
%% ARMENIAN SMALL LETTER DA
to_upper(16#0564)->
    16#0534;
%% ARMENIAN SMALL LETTER ECH
to_upper(16#0565)->
    16#0535;
%% ARMENIAN SMALL LETTER ZA
to_upper(16#0566)->
    16#0536;
%% ARMENIAN SMALL LETTER EH
to_upper(16#0567)->
    16#0537;
%% ARMENIAN SMALL LETTER ET
to_upper(16#0568)->
    16#0538;
%% ARMENIAN SMALL LETTER TO
to_upper(16#0569)->
    16#0539;
%% ARMENIAN SMALL LETTER ZHE
to_upper(16#056A)->
    16#053A;
%% ARMENIAN SMALL LETTER INI
to_upper(16#056B)->
    16#053B;
%% ARMENIAN SMALL LETTER LIWN
to_upper(16#056C)->
    16#053C;
%% ARMENIAN SMALL LETTER XEH
to_upper(16#056D)->
    16#053D;
%% ARMENIAN SMALL LETTER CA
to_upper(16#056E)->
    16#053E;
%% ARMENIAN SMALL LETTER KEN
to_upper(16#056F)->
    16#053F;
%% ARMENIAN SMALL LETTER HO
to_upper(16#0570)->
    16#0540;
%% ARMENIAN SMALL LETTER JA
to_upper(16#0571)->
    16#0541;
%% ARMENIAN SMALL LETTER GHAD
to_upper(16#0572)->
    16#0542;
%% ARMENIAN SMALL LETTER CHEH
to_upper(16#0573)->
    16#0543;
%% ARMENIAN SMALL LETTER MEN
to_upper(16#0574)->
    16#0544;
%% ARMENIAN SMALL LETTER YI
to_upper(16#0575)->
    16#0545;
%% ARMENIAN SMALL LETTER NOW
to_upper(16#0576)->
    16#0546;
%% ARMENIAN SMALL LETTER SHA
to_upper(16#0577)->
    16#0547;
%% ARMENIAN SMALL LETTER VO
to_upper(16#0578)->
    16#0548;
%% ARMENIAN SMALL LETTER CHA
to_upper(16#0579)->
    16#0549;
%% ARMENIAN SMALL LETTER PEH
to_upper(16#057A)->
    16#054A;
%% ARMENIAN SMALL LETTER JHEH
to_upper(16#057B)->
    16#054B;
%% ARMENIAN SMALL LETTER RA
to_upper(16#057C)->
    16#054C;
%% ARMENIAN SMALL LETTER SEH
to_upper(16#057D)->
    16#054D;
%% ARMENIAN SMALL LETTER VEW
to_upper(16#057E)->
    16#054E;
%% ARMENIAN SMALL LETTER TIWN
to_upper(16#057F)->
    16#054F;
%% ARMENIAN SMALL LETTER REH
to_upper(16#0580)->
    16#0550;
%% ARMENIAN SMALL LETTER CO
to_upper(16#0581)->
    16#0551;
%% ARMENIAN SMALL LETTER YIWN
to_upper(16#0582)->
    16#0552;
%% ARMENIAN SMALL LETTER PIWR
to_upper(16#0583)->
    16#0553;
%% ARMENIAN SMALL LETTER KEH
to_upper(16#0584)->
    16#0554;
%% ARMENIAN SMALL LETTER OH
to_upper(16#0585)->
    16#0555;
%% ARMENIAN SMALL LETTER FEH
to_upper(16#0586)->
    16#0556;
%% LATIN SMALL LETTER INSULAR G
to_upper(16#1D79)->
    16#A77D;
%% LATIN SMALL LETTER P WITH STROKE
to_upper(16#1D7D)->
    16#2C63;
%% LATIN SMALL LETTER A WITH RING BELOW
to_upper(16#1E01)->
    16#1E00;
%% LATIN SMALL LETTER B WITH DOT ABOVE
to_upper(16#1E03)->
    16#1E02;
%% LATIN SMALL LETTER B WITH DOT BELOW
to_upper(16#1E05)->
    16#1E04;
%% LATIN SMALL LETTER B WITH LINE BELOW
to_upper(16#1E07)->
    16#1E06;
%% LATIN SMALL LETTER C WITH CEDILLA AND ACUTE
to_upper(16#1E09)->
    16#1E08;
%% LATIN SMALL LETTER D WITH DOT ABOVE
to_upper(16#1E0B)->
    16#1E0A;
%% LATIN SMALL LETTER D WITH DOT BELOW
to_upper(16#1E0D)->
    16#1E0C;
%% LATIN SMALL LETTER D WITH LINE BELOW
to_upper(16#1E0F)->
    16#1E0E;
%% LATIN SMALL LETTER D WITH CEDILLA
to_upper(16#1E11)->
    16#1E10;
%% LATIN SMALL LETTER D WITH CIRCUMFLEX BELOW
to_upper(16#1E13)->
    16#1E12;
%% LATIN SMALL LETTER E WITH MACRON AND GRAVE
to_upper(16#1E15)->
    16#1E14;
%% LATIN SMALL LETTER E WITH MACRON AND ACUTE
to_upper(16#1E17)->
    16#1E16;
%% LATIN SMALL LETTER E WITH CIRCUMFLEX BELOW
to_upper(16#1E19)->
    16#1E18;
%% LATIN SMALL LETTER E WITH TILDE BELOW
to_upper(16#1E1B)->
    16#1E1A;
%% LATIN SMALL LETTER E WITH CEDILLA AND BREVE
to_upper(16#1E1D)->
    16#1E1C;
%% LATIN SMALL LETTER F WITH DOT ABOVE
to_upper(16#1E1F)->
    16#1E1E;
%% LATIN SMALL LETTER G WITH MACRON
to_upper(16#1E21)->
    16#1E20;
%% LATIN SMALL LETTER H WITH DOT ABOVE
to_upper(16#1E23)->
    16#1E22;
%% LATIN SMALL LETTER H WITH DOT BELOW
to_upper(16#1E25)->
    16#1E24;
%% LATIN SMALL LETTER H WITH DIAERESIS
to_upper(16#1E27)->
    16#1E26;
%% LATIN SMALL LETTER H WITH CEDILLA
to_upper(16#1E29)->
    16#1E28;
%% LATIN SMALL LETTER H WITH BREVE BELOW
to_upper(16#1E2B)->
    16#1E2A;
%% LATIN SMALL LETTER I WITH TILDE BELOW
to_upper(16#1E2D)->
    16#1E2C;
%% LATIN SMALL LETTER I WITH DIAERESIS AND ACUTE
to_upper(16#1E2F)->
    16#1E2E;
%% LATIN SMALL LETTER K WITH ACUTE
to_upper(16#1E31)->
    16#1E30;
%% LATIN SMALL LETTER K WITH DOT BELOW
to_upper(16#1E33)->
    16#1E32;
%% LATIN SMALL LETTER K WITH LINE BELOW
to_upper(16#1E35)->
    16#1E34;
%% LATIN SMALL LETTER L WITH DOT BELOW
to_upper(16#1E37)->
    16#1E36;
%% LATIN SMALL LETTER L WITH DOT BELOW AND MACRON
to_upper(16#1E39)->
    16#1E38;
%% LATIN SMALL LETTER L WITH LINE BELOW
to_upper(16#1E3B)->
    16#1E3A;
%% LATIN SMALL LETTER L WITH CIRCUMFLEX BELOW
to_upper(16#1E3D)->
    16#1E3C;
%% LATIN SMALL LETTER M WITH ACUTE
to_upper(16#1E3F)->
    16#1E3E;
%% LATIN SMALL LETTER M WITH DOT ABOVE
to_upper(16#1E41)->
    16#1E40;
%% LATIN SMALL LETTER M WITH DOT BELOW
to_upper(16#1E43)->
    16#1E42;
%% LATIN SMALL LETTER N WITH DOT ABOVE
to_upper(16#1E45)->
    16#1E44;
%% LATIN SMALL LETTER N WITH DOT BELOW
to_upper(16#1E47)->
    16#1E46;
%% LATIN SMALL LETTER N WITH LINE BELOW
to_upper(16#1E49)->
    16#1E48;
%% LATIN SMALL LETTER N WITH CIRCUMFLEX BELOW
to_upper(16#1E4B)->
    16#1E4A;
%% LATIN SMALL LETTER O WITH TILDE AND ACUTE
to_upper(16#1E4D)->
    16#1E4C;
%% LATIN SMALL LETTER O WITH TILDE AND DIAERESIS
to_upper(16#1E4F)->
    16#1E4E;
%% LATIN SMALL LETTER O WITH MACRON AND GRAVE
to_upper(16#1E51)->
    16#1E50;
%% LATIN SMALL LETTER O WITH MACRON AND ACUTE
to_upper(16#1E53)->
    16#1E52;
%% LATIN SMALL LETTER P WITH ACUTE
to_upper(16#1E55)->
    16#1E54;
%% LATIN SMALL LETTER P WITH DOT ABOVE
to_upper(16#1E57)->
    16#1E56;
%% LATIN SMALL LETTER R WITH DOT ABOVE
to_upper(16#1E59)->
    16#1E58;
%% LATIN SMALL LETTER R WITH DOT BELOW
to_upper(16#1E5B)->
    16#1E5A;
%% LATIN SMALL LETTER R WITH DOT BELOW AND MACRON
to_upper(16#1E5D)->
    16#1E5C;
%% LATIN SMALL LETTER R WITH LINE BELOW
to_upper(16#1E5F)->
    16#1E5E;
%% LATIN SMALL LETTER S WITH DOT ABOVE
to_upper(16#1E61)->
    16#1E60;
%% LATIN SMALL LETTER S WITH DOT BELOW
to_upper(16#1E63)->
    16#1E62;
%% LATIN SMALL LETTER S WITH ACUTE AND DOT ABOVE
to_upper(16#1E65)->
    16#1E64;
%% LATIN SMALL LETTER S WITH CARON AND DOT ABOVE
to_upper(16#1E67)->
    16#1E66;
%% LATIN SMALL LETTER S WITH DOT BELOW AND DOT ABOVE
to_upper(16#1E69)->
    16#1E68;
%% LATIN SMALL LETTER T WITH DOT ABOVE
to_upper(16#1E6B)->
    16#1E6A;
%% LATIN SMALL LETTER T WITH DOT BELOW
to_upper(16#1E6D)->
    16#1E6C;
%% LATIN SMALL LETTER T WITH LINE BELOW
to_upper(16#1E6F)->
    16#1E6E;
%% LATIN SMALL LETTER T WITH CIRCUMFLEX BELOW
to_upper(16#1E71)->
    16#1E70;
%% LATIN SMALL LETTER U WITH DIAERESIS BELOW
to_upper(16#1E73)->
    16#1E72;
%% LATIN SMALL LETTER U WITH TILDE BELOW
to_upper(16#1E75)->
    16#1E74;
%% LATIN SMALL LETTER U WITH CIRCUMFLEX BELOW
to_upper(16#1E77)->
    16#1E76;
%% LATIN SMALL LETTER U WITH TILDE AND ACUTE
to_upper(16#1E79)->
    16#1E78;
%% LATIN SMALL LETTER U WITH MACRON AND DIAERESIS
to_upper(16#1E7B)->
    16#1E7A;
%% LATIN SMALL LETTER V WITH TILDE
to_upper(16#1E7D)->
    16#1E7C;
%% LATIN SMALL LETTER V WITH DOT BELOW
to_upper(16#1E7F)->
    16#1E7E;
%% LATIN SMALL LETTER W WITH GRAVE
to_upper(16#1E81)->
    16#1E80;
%% LATIN SMALL LETTER W WITH ACUTE
to_upper(16#1E83)->
    16#1E82;
%% LATIN SMALL LETTER W WITH DIAERESIS
to_upper(16#1E85)->
    16#1E84;
%% LATIN SMALL LETTER W WITH DOT ABOVE
to_upper(16#1E87)->
    16#1E86;
%% LATIN SMALL LETTER W WITH DOT BELOW
to_upper(16#1E89)->
    16#1E88;
%% LATIN SMALL LETTER X WITH DOT ABOVE
to_upper(16#1E8B)->
    16#1E8A;
%% LATIN SMALL LETTER X WITH DIAERESIS
to_upper(16#1E8D)->
    16#1E8C;
%% LATIN SMALL LETTER Y WITH DOT ABOVE
to_upper(16#1E8F)->
    16#1E8E;
%% LATIN SMALL LETTER Z WITH CIRCUMFLEX
to_upper(16#1E91)->
    16#1E90;
%% LATIN SMALL LETTER Z WITH DOT BELOW
to_upper(16#1E93)->
    16#1E92;
%% LATIN SMALL LETTER Z WITH LINE BELOW
to_upper(16#1E95)->
    16#1E94;
%% LATIN SMALL LETTER LONG S WITH DOT ABOVE
to_upper(16#1E9B)->
    16#1E60;
%% LATIN SMALL LETTER A WITH DOT BELOW
to_upper(16#1EA1)->
    16#1EA0;
%% LATIN SMALL LETTER A WITH HOOK ABOVE
to_upper(16#1EA3)->
    16#1EA2;
%% LATIN SMALL LETTER A WITH CIRCUMFLEX AND ACUTE
to_upper(16#1EA5)->
    16#1EA4;
%% LATIN SMALL LETTER A WITH CIRCUMFLEX AND GRAVE
to_upper(16#1EA7)->
    16#1EA6;
%% LATIN SMALL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
to_upper(16#1EA9)->
    16#1EA8;
%% LATIN SMALL LETTER A WITH CIRCUMFLEX AND TILDE
to_upper(16#1EAB)->
    16#1EAA;
%% LATIN SMALL LETTER A WITH CIRCUMFLEX AND DOT BELOW
to_upper(16#1EAD)->
    16#1EAC;
%% LATIN SMALL LETTER A WITH BREVE AND ACUTE
to_upper(16#1EAF)->
    16#1EAE;
%% LATIN SMALL LETTER A WITH BREVE AND GRAVE
to_upper(16#1EB1)->
    16#1EB0;
%% LATIN SMALL LETTER A WITH BREVE AND HOOK ABOVE
to_upper(16#1EB3)->
    16#1EB2;
%% LATIN SMALL LETTER A WITH BREVE AND TILDE
to_upper(16#1EB5)->
    16#1EB4;
%% LATIN SMALL LETTER A WITH BREVE AND DOT BELOW
to_upper(16#1EB7)->
    16#1EB6;
%% LATIN SMALL LETTER E WITH DOT BELOW
to_upper(16#1EB9)->
    16#1EB8;
%% LATIN SMALL LETTER E WITH HOOK ABOVE
to_upper(16#1EBB)->
    16#1EBA;
%% LATIN SMALL LETTER E WITH TILDE
to_upper(16#1EBD)->
    16#1EBC;
%% LATIN SMALL LETTER E WITH CIRCUMFLEX AND ACUTE
to_upper(16#1EBF)->
    16#1EBE;
%% LATIN SMALL LETTER E WITH CIRCUMFLEX AND GRAVE
to_upper(16#1EC1)->
    16#1EC0;
%% LATIN SMALL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
to_upper(16#1EC3)->
    16#1EC2;
%% LATIN SMALL LETTER E WITH CIRCUMFLEX AND TILDE
to_upper(16#1EC5)->
    16#1EC4;
%% LATIN SMALL LETTER E WITH CIRCUMFLEX AND DOT BELOW
to_upper(16#1EC7)->
    16#1EC6;
%% LATIN SMALL LETTER I WITH HOOK ABOVE
to_upper(16#1EC9)->
    16#1EC8;
%% LATIN SMALL LETTER I WITH DOT BELOW
to_upper(16#1ECB)->
    16#1ECA;
%% LATIN SMALL LETTER O WITH DOT BELOW
to_upper(16#1ECD)->
    16#1ECC;
%% LATIN SMALL LETTER O WITH HOOK ABOVE
to_upper(16#1ECF)->
    16#1ECE;
%% LATIN SMALL LETTER O WITH CIRCUMFLEX AND ACUTE
to_upper(16#1ED1)->
    16#1ED0;
%% LATIN SMALL LETTER O WITH CIRCUMFLEX AND GRAVE
to_upper(16#1ED3)->
    16#1ED2;
%% LATIN SMALL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
to_upper(16#1ED5)->
    16#1ED4;
%% LATIN SMALL LETTER O WITH CIRCUMFLEX AND TILDE
to_upper(16#1ED7)->
    16#1ED6;
%% LATIN SMALL LETTER O WITH CIRCUMFLEX AND DOT BELOW
to_upper(16#1ED9)->
    16#1ED8;
%% LATIN SMALL LETTER O WITH HORN AND ACUTE
to_upper(16#1EDB)->
    16#1EDA;
%% LATIN SMALL LETTER O WITH HORN AND GRAVE
to_upper(16#1EDD)->
    16#1EDC;
%% LATIN SMALL LETTER O WITH HORN AND HOOK ABOVE
to_upper(16#1EDF)->
    16#1EDE;
%% LATIN SMALL LETTER O WITH HORN AND TILDE
to_upper(16#1EE1)->
    16#1EE0;
%% LATIN SMALL LETTER O WITH HORN AND DOT BELOW
to_upper(16#1EE3)->
    16#1EE2;
%% LATIN SMALL LETTER U WITH DOT BELOW
to_upper(16#1EE5)->
    16#1EE4;
%% LATIN SMALL LETTER U WITH HOOK ABOVE
to_upper(16#1EE7)->
    16#1EE6;
%% LATIN SMALL LETTER U WITH HORN AND ACUTE
to_upper(16#1EE9)->
    16#1EE8;
%% LATIN SMALL LETTER U WITH HORN AND GRAVE
to_upper(16#1EEB)->
    16#1EEA;
%% LATIN SMALL LETTER U WITH HORN AND HOOK ABOVE
to_upper(16#1EED)->
    16#1EEC;
%% LATIN SMALL LETTER U WITH HORN AND TILDE
to_upper(16#1EEF)->
    16#1EEE;
%% LATIN SMALL LETTER U WITH HORN AND DOT BELOW
to_upper(16#1EF1)->
    16#1EF0;
%% LATIN SMALL LETTER Y WITH GRAVE
to_upper(16#1EF3)->
    16#1EF2;
%% LATIN SMALL LETTER Y WITH DOT BELOW
to_upper(16#1EF5)->
    16#1EF4;
%% LATIN SMALL LETTER Y WITH HOOK ABOVE
to_upper(16#1EF7)->
    16#1EF6;
%% LATIN SMALL LETTER Y WITH TILDE
to_upper(16#1EF9)->
    16#1EF8;
%% LATIN SMALL LETTER MIDDLE-WELSH LL
to_upper(16#1EFB)->
    16#1EFA;
%% LATIN SMALL LETTER MIDDLE-WELSH V
to_upper(16#1EFD)->
    16#1EFC;
%% LATIN SMALL LETTER Y WITH LOOP
to_upper(16#1EFF)->
    16#1EFE;
%% GREEK SMALL LETTER ALPHA WITH PSILI
to_upper(16#1F00)->
    16#1F08;
%% GREEK SMALL LETTER ALPHA WITH DASIA
to_upper(16#1F01)->
    16#1F09;
%% GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA
to_upper(16#1F02)->
    16#1F0A;
%% GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA
to_upper(16#1F03)->
    16#1F0B;
%% GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA
to_upper(16#1F04)->
    16#1F0C;
%% GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA
to_upper(16#1F05)->
    16#1F0D;
%% GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI
to_upper(16#1F06)->
    16#1F0E;
%% GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI
to_upper(16#1F07)->
    16#1F0F;
%% GREEK SMALL LETTER EPSILON WITH PSILI
to_upper(16#1F10)->
    16#1F18;
%% GREEK SMALL LETTER EPSILON WITH DASIA
to_upper(16#1F11)->
    16#1F19;
%% GREEK SMALL LETTER EPSILON WITH PSILI AND VARIA
to_upper(16#1F12)->
    16#1F1A;
%% GREEK SMALL LETTER EPSILON WITH DASIA AND VARIA
to_upper(16#1F13)->
    16#1F1B;
%% GREEK SMALL LETTER EPSILON WITH PSILI AND OXIA
to_upper(16#1F14)->
    16#1F1C;
%% GREEK SMALL LETTER EPSILON WITH DASIA AND OXIA
to_upper(16#1F15)->
    16#1F1D;
%% GREEK SMALL LETTER ETA WITH PSILI
to_upper(16#1F20)->
    16#1F28;
%% GREEK SMALL LETTER ETA WITH DASIA
to_upper(16#1F21)->
    16#1F29;
%% GREEK SMALL LETTER ETA WITH PSILI AND VARIA
to_upper(16#1F22)->
    16#1F2A;
%% GREEK SMALL LETTER ETA WITH DASIA AND VARIA
to_upper(16#1F23)->
    16#1F2B;
%% GREEK SMALL LETTER ETA WITH PSILI AND OXIA
to_upper(16#1F24)->
    16#1F2C;
%% GREEK SMALL LETTER ETA WITH DASIA AND OXIA
to_upper(16#1F25)->
    16#1F2D;
%% GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI
to_upper(16#1F26)->
    16#1F2E;
%% GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI
to_upper(16#1F27)->
    16#1F2F;
%% GREEK SMALL LETTER IOTA WITH PSILI
to_upper(16#1F30)->
    16#1F38;
%% GREEK SMALL LETTER IOTA WITH DASIA
to_upper(16#1F31)->
    16#1F39;
%% GREEK SMALL LETTER IOTA WITH PSILI AND VARIA
to_upper(16#1F32)->
    16#1F3A;
%% GREEK SMALL LETTER IOTA WITH DASIA AND VARIA
to_upper(16#1F33)->
    16#1F3B;
%% GREEK SMALL LETTER IOTA WITH PSILI AND OXIA
to_upper(16#1F34)->
    16#1F3C;
%% GREEK SMALL LETTER IOTA WITH DASIA AND OXIA
to_upper(16#1F35)->
    16#1F3D;
%% GREEK SMALL LETTER IOTA WITH PSILI AND PERISPOMENI
to_upper(16#1F36)->
    16#1F3E;
%% GREEK SMALL LETTER IOTA WITH DASIA AND PERISPOMENI
to_upper(16#1F37)->
    16#1F3F;
%% GREEK SMALL LETTER OMICRON WITH PSILI
to_upper(16#1F40)->
    16#1F48;
%% GREEK SMALL LETTER OMICRON WITH DASIA
to_upper(16#1F41)->
    16#1F49;
%% GREEK SMALL LETTER OMICRON WITH PSILI AND VARIA
to_upper(16#1F42)->
    16#1F4A;
%% GREEK SMALL LETTER OMICRON WITH DASIA AND VARIA
to_upper(16#1F43)->
    16#1F4B;
%% GREEK SMALL LETTER OMICRON WITH PSILI AND OXIA
to_upper(16#1F44)->
    16#1F4C;
%% GREEK SMALL LETTER OMICRON WITH DASIA AND OXIA
to_upper(16#1F45)->
    16#1F4D;
%% GREEK SMALL LETTER UPSILON WITH DASIA
to_upper(16#1F51)->
    16#1F59;
%% GREEK SMALL LETTER UPSILON WITH DASIA AND VARIA
to_upper(16#1F53)->
    16#1F5B;
%% GREEK SMALL LETTER UPSILON WITH DASIA AND OXIA
to_upper(16#1F55)->
    16#1F5D;
%% GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI
to_upper(16#1F57)->
    16#1F5F;
%% GREEK SMALL LETTER OMEGA WITH PSILI
to_upper(16#1F60)->
    16#1F68;
%% GREEK SMALL LETTER OMEGA WITH DASIA
to_upper(16#1F61)->
    16#1F69;
%% GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA
to_upper(16#1F62)->
    16#1F6A;
%% GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA
to_upper(16#1F63)->
    16#1F6B;
%% GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA
to_upper(16#1F64)->
    16#1F6C;
%% GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA
to_upper(16#1F65)->
    16#1F6D;
%% GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI
to_upper(16#1F66)->
    16#1F6E;
%% GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI
to_upper(16#1F67)->
    16#1F6F;
%% GREEK SMALL LETTER ALPHA WITH VARIA
to_upper(16#1F70)->
    16#1FBA;
%% GREEK SMALL LETTER ALPHA WITH OXIA
to_upper(16#1F71)->
    16#1FBB;
%% GREEK SMALL LETTER EPSILON WITH VARIA
to_upper(16#1F72)->
    16#1FC8;
%% GREEK SMALL LETTER EPSILON WITH OXIA
to_upper(16#1F73)->
    16#1FC9;
%% GREEK SMALL LETTER ETA WITH VARIA
to_upper(16#1F74)->
    16#1FCA;
%% GREEK SMALL LETTER ETA WITH OXIA
to_upper(16#1F75)->
    16#1FCB;
%% GREEK SMALL LETTER IOTA WITH VARIA
to_upper(16#1F76)->
    16#1FDA;
%% GREEK SMALL LETTER IOTA WITH OXIA
to_upper(16#1F77)->
    16#1FDB;
%% GREEK SMALL LETTER OMICRON WITH VARIA
to_upper(16#1F78)->
    16#1FF8;
%% GREEK SMALL LETTER OMICRON WITH OXIA
to_upper(16#1F79)->
    16#1FF9;
%% GREEK SMALL LETTER UPSILON WITH VARIA
to_upper(16#1F7A)->
    16#1FEA;
%% GREEK SMALL LETTER UPSILON WITH OXIA
to_upper(16#1F7B)->
    16#1FEB;
%% GREEK SMALL LETTER OMEGA WITH VARIA
to_upper(16#1F7C)->
    16#1FFA;
%% GREEK SMALL LETTER OMEGA WITH OXIA
to_upper(16#1F7D)->
    16#1FFB;
%% GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI
to_upper(16#1F80)->
    16#1F88;
%% GREEK SMALL LETTER ALPHA WITH DASIA AND YPOGEGRAMMENI
to_upper(16#1F81)->
    16#1F89;
%% GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA AND YPOGEGRAMMENI
to_upper(16#1F82)->
    16#1F8A;
%% GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA AND YPOGEGRAMMENI
to_upper(16#1F83)->
    16#1F8B;
%% GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA AND YPOGEGRAMMENI
to_upper(16#1F84)->
    16#1F8C;
%% GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA AND YPOGEGRAMMENI
to_upper(16#1F85)->
    16#1F8D;
%% GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
to_upper(16#1F86)->
    16#1F8E;
%% GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
to_upper(16#1F87)->
    16#1F8F;
%% GREEK SMALL LETTER ETA WITH PSILI AND YPOGEGRAMMENI
to_upper(16#1F90)->
    16#1F98;
%% GREEK SMALL LETTER ETA WITH DASIA AND YPOGEGRAMMENI
to_upper(16#1F91)->
    16#1F99;
%% GREEK SMALL LETTER ETA WITH PSILI AND VARIA AND YPOGEGRAMMENI
to_upper(16#1F92)->
    16#1F9A;
%% GREEK SMALL LETTER ETA WITH DASIA AND VARIA AND YPOGEGRAMMENI
to_upper(16#1F93)->
    16#1F9B;
%% GREEK SMALL LETTER ETA WITH PSILI AND OXIA AND YPOGEGRAMMENI
to_upper(16#1F94)->
    16#1F9C;
%% GREEK SMALL LETTER ETA WITH DASIA AND OXIA AND YPOGEGRAMMENI
to_upper(16#1F95)->
    16#1F9D;
%% GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
to_upper(16#1F96)->
    16#1F9E;
%% GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
to_upper(16#1F97)->
    16#1F9F;
%% GREEK SMALL LETTER OMEGA WITH PSILI AND YPOGEGRAMMENI
to_upper(16#1FA0)->
    16#1FA8;
%% GREEK SMALL LETTER OMEGA WITH DASIA AND YPOGEGRAMMENI
to_upper(16#1FA1)->
    16#1FA9;
%% GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA AND YPOGEGRAMMENI
to_upper(16#1FA2)->
    16#1FAA;
%% GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA AND YPOGEGRAMMENI
to_upper(16#1FA3)->
    16#1FAB;
%% GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA AND YPOGEGRAMMENI
to_upper(16#1FA4)->
    16#1FAC;
%% GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA AND YPOGEGRAMMENI
to_upper(16#1FA5)->
    16#1FAD;
%% GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
to_upper(16#1FA6)->
    16#1FAE;
%% GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
to_upper(16#1FA7)->
    16#1FAF;
%% GREEK SMALL LETTER ALPHA WITH VRACHY
to_upper(16#1FB0)->
    16#1FB8;
%% GREEK SMALL LETTER ALPHA WITH MACRON
to_upper(16#1FB1)->
    16#1FB9;
%% GREEK SMALL LETTER ALPHA WITH YPOGEGRAMMENI
to_upper(16#1FB3)->
    16#1FBC;
%% GREEK PROSGEGRAMMENI
to_upper(16#1FBE)->
    16#0399;
%% GREEK SMALL LETTER ETA WITH YPOGEGRAMMENI
to_upper(16#1FC3)->
    16#1FCC;
%% GREEK SMALL LETTER IOTA WITH VRACHY
to_upper(16#1FD0)->
    16#1FD8;
%% GREEK SMALL LETTER IOTA WITH MACRON
to_upper(16#1FD1)->
    16#1FD9;
%% GREEK SMALL LETTER UPSILON WITH VRACHY
to_upper(16#1FE0)->
    16#1FE8;
%% GREEK SMALL LETTER UPSILON WITH MACRON
to_upper(16#1FE1)->
    16#1FE9;
%% GREEK SMALL LETTER RHO WITH DASIA
to_upper(16#1FE5)->
    16#1FEC;
%% GREEK SMALL LETTER OMEGA WITH YPOGEGRAMMENI
to_upper(16#1FF3)->
    16#1FFC;
%% TURNED SMALL F
to_upper(16#214E)->
    16#2132;
%% SMALL ROMAN NUMERAL ONE
to_upper(16#2170)->
    16#2160;
%% SMALL ROMAN NUMERAL TWO
to_upper(16#2171)->
    16#2161;
%% SMALL ROMAN NUMERAL THREE
to_upper(16#2172)->
    16#2162;
%% SMALL ROMAN NUMERAL FOUR
to_upper(16#2173)->
    16#2163;
%% SMALL ROMAN NUMERAL FIVE
to_upper(16#2174)->
    16#2164;
%% SMALL ROMAN NUMERAL SIX
to_upper(16#2175)->
    16#2165;
%% SMALL ROMAN NUMERAL SEVEN
to_upper(16#2176)->
    16#2166;
%% SMALL ROMAN NUMERAL EIGHT
to_upper(16#2177)->
    16#2167;
%% SMALL ROMAN NUMERAL NINE
to_upper(16#2178)->
    16#2168;
%% SMALL ROMAN NUMERAL TEN
to_upper(16#2179)->
    16#2169;
%% SMALL ROMAN NUMERAL ELEVEN
to_upper(16#217A)->
    16#216A;
%% SMALL ROMAN NUMERAL TWELVE
to_upper(16#217B)->
    16#216B;
%% SMALL ROMAN NUMERAL FIFTY
to_upper(16#217C)->
    16#216C;
%% SMALL ROMAN NUMERAL ONE HUNDRED
to_upper(16#217D)->
    16#216D;
%% SMALL ROMAN NUMERAL FIVE HUNDRED
to_upper(16#217E)->
    16#216E;
%% SMALL ROMAN NUMERAL ONE THOUSAND
to_upper(16#217F)->
    16#216F;
%% LATIN SMALL LETTER REVERSED C
to_upper(16#2184)->
    16#2183;
%% CIRCLED LATIN SMALL LETTER A
to_upper(16#24D0)->
    16#24B6;
%% CIRCLED LATIN SMALL LETTER B
to_upper(16#24D1)->
    16#24B7;
%% CIRCLED LATIN SMALL LETTER C
to_upper(16#24D2)->
    16#24B8;
%% CIRCLED LATIN SMALL LETTER D
to_upper(16#24D3)->
    16#24B9;
%% CIRCLED LATIN SMALL LETTER E
to_upper(16#24D4)->
    16#24BA;
%% CIRCLED LATIN SMALL LETTER F
to_upper(16#24D5)->
    16#24BB;
%% CIRCLED LATIN SMALL LETTER G
to_upper(16#24D6)->
    16#24BC;
%% CIRCLED LATIN SMALL LETTER H
to_upper(16#24D7)->
    16#24BD;
%% CIRCLED LATIN SMALL LETTER I
to_upper(16#24D8)->
    16#24BE;
%% CIRCLED LATIN SMALL LETTER J
to_upper(16#24D9)->
    16#24BF;
%% CIRCLED LATIN SMALL LETTER K
to_upper(16#24DA)->
    16#24C0;
%% CIRCLED LATIN SMALL LETTER L
to_upper(16#24DB)->
    16#24C1;
%% CIRCLED LATIN SMALL LETTER M
to_upper(16#24DC)->
    16#24C2;
%% CIRCLED LATIN SMALL LETTER N
to_upper(16#24DD)->
    16#24C3;
%% CIRCLED LATIN SMALL LETTER O
to_upper(16#24DE)->
    16#24C4;
%% CIRCLED LATIN SMALL LETTER P
to_upper(16#24DF)->
    16#24C5;
%% CIRCLED LATIN SMALL LETTER Q
to_upper(16#24E0)->
    16#24C6;
%% CIRCLED LATIN SMALL LETTER R
to_upper(16#24E1)->
    16#24C7;
%% CIRCLED LATIN SMALL LETTER S
to_upper(16#24E2)->
    16#24C8;
%% CIRCLED LATIN SMALL LETTER T
to_upper(16#24E3)->
    16#24C9;
%% CIRCLED LATIN SMALL LETTER U
to_upper(16#24E4)->
    16#24CA;
%% CIRCLED LATIN SMALL LETTER V
to_upper(16#24E5)->
    16#24CB;
%% CIRCLED LATIN SMALL LETTER W
to_upper(16#24E6)->
    16#24CC;
%% CIRCLED LATIN SMALL LETTER X
to_upper(16#24E7)->
    16#24CD;
%% CIRCLED LATIN SMALL LETTER Y
to_upper(16#24E8)->
    16#24CE;
%% CIRCLED LATIN SMALL LETTER Z
to_upper(16#24E9)->
    16#24CF;
%% GLAGOLITIC SMALL LETTER AZU
to_upper(16#2C30)->
    16#2C00;
%% GLAGOLITIC SMALL LETTER BUKY
to_upper(16#2C31)->
    16#2C01;
%% GLAGOLITIC SMALL LETTER VEDE
to_upper(16#2C32)->
    16#2C02;
%% GLAGOLITIC SMALL LETTER GLAGOLI
to_upper(16#2C33)->
    16#2C03;
%% GLAGOLITIC SMALL LETTER DOBRO
to_upper(16#2C34)->
    16#2C04;
%% GLAGOLITIC SMALL LETTER YESTU
to_upper(16#2C35)->
    16#2C05;
%% GLAGOLITIC SMALL LETTER ZHIVETE
to_upper(16#2C36)->
    16#2C06;
%% GLAGOLITIC SMALL LETTER DZELO
to_upper(16#2C37)->
    16#2C07;
%% GLAGOLITIC SMALL LETTER ZEMLJA
to_upper(16#2C38)->
    16#2C08;
%% GLAGOLITIC SMALL LETTER IZHE
to_upper(16#2C39)->
    16#2C09;
%% GLAGOLITIC SMALL LETTER INITIAL IZHE
to_upper(16#2C3A)->
    16#2C0A;
%% GLAGOLITIC SMALL LETTER I
to_upper(16#2C3B)->
    16#2C0B;
%% GLAGOLITIC SMALL LETTER DJERVI
to_upper(16#2C3C)->
    16#2C0C;
%% GLAGOLITIC SMALL LETTER KAKO
to_upper(16#2C3D)->
    16#2C0D;
%% GLAGOLITIC SMALL LETTER LJUDIJE
to_upper(16#2C3E)->
    16#2C0E;
%% GLAGOLITIC SMALL LETTER MYSLITE
to_upper(16#2C3F)->
    16#2C0F;
%% GLAGOLITIC SMALL LETTER NASHI
to_upper(16#2C40)->
    16#2C10;
%% GLAGOLITIC SMALL LETTER ONU
to_upper(16#2C41)->
    16#2C11;
%% GLAGOLITIC SMALL LETTER POKOJI
to_upper(16#2C42)->
    16#2C12;
%% GLAGOLITIC SMALL LETTER RITSI
to_upper(16#2C43)->
    16#2C13;
%% GLAGOLITIC SMALL LETTER SLOVO
to_upper(16#2C44)->
    16#2C14;
%% GLAGOLITIC SMALL LETTER TVRIDO
to_upper(16#2C45)->
    16#2C15;
%% GLAGOLITIC SMALL LETTER UKU
to_upper(16#2C46)->
    16#2C16;
%% GLAGOLITIC SMALL LETTER FRITU
to_upper(16#2C47)->
    16#2C17;
%% GLAGOLITIC SMALL LETTER HERU
to_upper(16#2C48)->
    16#2C18;
%% GLAGOLITIC SMALL LETTER OTU
to_upper(16#2C49)->
    16#2C19;
%% GLAGOLITIC SMALL LETTER PE
to_upper(16#2C4A)->
    16#2C1A;
%% GLAGOLITIC SMALL LETTER SHTA
to_upper(16#2C4B)->
    16#2C1B;
%% GLAGOLITIC SMALL LETTER TSI
to_upper(16#2C4C)->
    16#2C1C;
%% GLAGOLITIC SMALL LETTER CHRIVI
to_upper(16#2C4D)->
    16#2C1D;
%% GLAGOLITIC SMALL LETTER SHA
to_upper(16#2C4E)->
    16#2C1E;
%% GLAGOLITIC SMALL LETTER YERU
to_upper(16#2C4F)->
    16#2C1F;
%% GLAGOLITIC SMALL LETTER YERI
to_upper(16#2C50)->
    16#2C20;
%% GLAGOLITIC SMALL LETTER YATI
to_upper(16#2C51)->
    16#2C21;
%% GLAGOLITIC SMALL LETTER SPIDERY HA
to_upper(16#2C52)->
    16#2C22;
%% GLAGOLITIC SMALL LETTER YU
to_upper(16#2C53)->
    16#2C23;
%% GLAGOLITIC SMALL LETTER SMALL YUS
to_upper(16#2C54)->
    16#2C24;
%% GLAGOLITIC SMALL LETTER SMALL YUS WITH TAIL
to_upper(16#2C55)->
    16#2C25;
%% GLAGOLITIC SMALL LETTER YO
to_upper(16#2C56)->
    16#2C26;
%% GLAGOLITIC SMALL LETTER IOTATED SMALL YUS
to_upper(16#2C57)->
    16#2C27;
%% GLAGOLITIC SMALL LETTER BIG YUS
to_upper(16#2C58)->
    16#2C28;
%% GLAGOLITIC SMALL LETTER IOTATED BIG YUS
to_upper(16#2C59)->
    16#2C29;
%% GLAGOLITIC SMALL LETTER FITA
to_upper(16#2C5A)->
    16#2C2A;
%% GLAGOLITIC SMALL LETTER IZHITSA
to_upper(16#2C5B)->
    16#2C2B;
%% GLAGOLITIC SMALL LETTER SHTAPIC
to_upper(16#2C5C)->
    16#2C2C;
%% GLAGOLITIC SMALL LETTER TROKUTASTI A
to_upper(16#2C5D)->
    16#2C2D;
%% GLAGOLITIC SMALL LETTER LATINATE MYSLITE
to_upper(16#2C5E)->
    16#2C2E;
%% LATIN SMALL LETTER L WITH DOUBLE BAR
to_upper(16#2C61)->
    16#2C60;
%% LATIN SMALL LETTER A WITH STROKE
to_upper(16#2C65)->
    16#023A;
%% LATIN SMALL LETTER T WITH DIAGONAL STROKE
to_upper(16#2C66)->
    16#023E;
%% LATIN SMALL LETTER H WITH DESCENDER
to_upper(16#2C68)->
    16#2C67;
%% LATIN SMALL LETTER K WITH DESCENDER
to_upper(16#2C6A)->
    16#2C69;
%% LATIN SMALL LETTER Z WITH DESCENDER
to_upper(16#2C6C)->
    16#2C6B;
%% LATIN SMALL LETTER W WITH HOOK
to_upper(16#2C73)->
    16#2C72;
%% LATIN SMALL LETTER HALF H
to_upper(16#2C76)->
    16#2C75;
%% COPTIC SMALL LETTER ALFA
to_upper(16#2C81)->
    16#2C80;
%% COPTIC SMALL LETTER VIDA
to_upper(16#2C83)->
    16#2C82;
%% COPTIC SMALL LETTER GAMMA
to_upper(16#2C85)->
    16#2C84;
%% COPTIC SMALL LETTER DALDA
to_upper(16#2C87)->
    16#2C86;
%% COPTIC SMALL LETTER EIE
to_upper(16#2C89)->
    16#2C88;
%% COPTIC SMALL LETTER SOU
to_upper(16#2C8B)->
    16#2C8A;
%% COPTIC SMALL LETTER ZATA
to_upper(16#2C8D)->
    16#2C8C;
%% COPTIC SMALL LETTER HATE
to_upper(16#2C8F)->
    16#2C8E;
%% COPTIC SMALL LETTER THETHE
to_upper(16#2C91)->
    16#2C90;
%% COPTIC SMALL LETTER IAUDA
to_upper(16#2C93)->
    16#2C92;
%% COPTIC SMALL LETTER KAPA
to_upper(16#2C95)->
    16#2C94;
%% COPTIC SMALL LETTER LAULA
to_upper(16#2C97)->
    16#2C96;
%% COPTIC SMALL LETTER MI
to_upper(16#2C99)->
    16#2C98;
%% COPTIC SMALL LETTER NI
to_upper(16#2C9B)->
    16#2C9A;
%% COPTIC SMALL LETTER KSI
to_upper(16#2C9D)->
    16#2C9C;
%% COPTIC SMALL LETTER O
to_upper(16#2C9F)->
    16#2C9E;
%% COPTIC SMALL LETTER PI
to_upper(16#2CA1)->
    16#2CA0;
%% COPTIC SMALL LETTER RO
to_upper(16#2CA3)->
    16#2CA2;
%% COPTIC SMALL LETTER SIMA
to_upper(16#2CA5)->
    16#2CA4;
%% COPTIC SMALL LETTER TAU
to_upper(16#2CA7)->
    16#2CA6;
%% COPTIC SMALL LETTER UA
to_upper(16#2CA9)->
    16#2CA8;
%% COPTIC SMALL LETTER FI
to_upper(16#2CAB)->
    16#2CAA;
%% COPTIC SMALL LETTER KHI
to_upper(16#2CAD)->
    16#2CAC;
%% COPTIC SMALL LETTER PSI
to_upper(16#2CAF)->
    16#2CAE;
%% COPTIC SMALL LETTER OOU
to_upper(16#2CB1)->
    16#2CB0;
%% COPTIC SMALL LETTER DIALECT-P ALEF
to_upper(16#2CB3)->
    16#2CB2;
%% COPTIC SMALL LETTER OLD COPTIC AIN
to_upper(16#2CB5)->
    16#2CB4;
%% COPTIC SMALL LETTER CRYPTOGRAMMIC EIE
to_upper(16#2CB7)->
    16#2CB6;
%% COPTIC SMALL LETTER DIALECT-P KAPA
to_upper(16#2CB9)->
    16#2CB8;
%% COPTIC SMALL LETTER DIALECT-P NI
to_upper(16#2CBB)->
    16#2CBA;
%% COPTIC SMALL LETTER CRYPTOGRAMMIC NI
to_upper(16#2CBD)->
    16#2CBC;
%% COPTIC SMALL LETTER OLD COPTIC OOU
to_upper(16#2CBF)->
    16#2CBE;
%% COPTIC SMALL LETTER SAMPI
to_upper(16#2CC1)->
    16#2CC0;
%% COPTIC SMALL LETTER CROSSED SHEI
to_upper(16#2CC3)->
    16#2CC2;
%% COPTIC SMALL LETTER OLD COPTIC SHEI
to_upper(16#2CC5)->
    16#2CC4;
%% COPTIC SMALL LETTER OLD COPTIC ESH
to_upper(16#2CC7)->
    16#2CC6;
%% COPTIC SMALL LETTER AKHMIMIC KHEI
to_upper(16#2CC9)->
    16#2CC8;
%% COPTIC SMALL LETTER DIALECT-P HORI
to_upper(16#2CCB)->
    16#2CCA;
%% COPTIC SMALL LETTER OLD COPTIC HORI
to_upper(16#2CCD)->
    16#2CCC;
%% COPTIC SMALL LETTER OLD COPTIC HA
to_upper(16#2CCF)->
    16#2CCE;
%% COPTIC SMALL LETTER L-SHAPED HA
to_upper(16#2CD1)->
    16#2CD0;
%% COPTIC SMALL LETTER OLD COPTIC HEI
to_upper(16#2CD3)->
    16#2CD2;
%% COPTIC SMALL LETTER OLD COPTIC HAT
to_upper(16#2CD5)->
    16#2CD4;
%% COPTIC SMALL LETTER OLD COPTIC GANGIA
to_upper(16#2CD7)->
    16#2CD6;
%% COPTIC SMALL LETTER OLD COPTIC DJA
to_upper(16#2CD9)->
    16#2CD8;
%% COPTIC SMALL LETTER OLD COPTIC SHIMA
to_upper(16#2CDB)->
    16#2CDA;
%% COPTIC SMALL LETTER OLD NUBIAN SHIMA
to_upper(16#2CDD)->
    16#2CDC;
%% COPTIC SMALL LETTER OLD NUBIAN NGI
to_upper(16#2CDF)->
    16#2CDE;
%% COPTIC SMALL LETTER OLD NUBIAN NYI
to_upper(16#2CE1)->
    16#2CE0;
%% COPTIC SMALL LETTER OLD NUBIAN WAU
to_upper(16#2CE3)->
    16#2CE2;
%% COPTIC SMALL LETTER CRYPTOGRAMMIC SHEI
to_upper(16#2CEC)->
    16#2CEB;
%% COPTIC SMALL LETTER CRYPTOGRAMMIC GANGIA
to_upper(16#2CEE)->
    16#2CED;
%% COPTIC SMALL LETTER BOHAIRIC KHEI
to_upper(16#2CF3)->
    16#2CF2;
%% GEORGIAN SMALL LETTER AN
to_upper(16#2D00)->
    16#10A0;
%% GEORGIAN SMALL LETTER BAN
to_upper(16#2D01)->
    16#10A1;
%% GEORGIAN SMALL LETTER GAN
to_upper(16#2D02)->
    16#10A2;
%% GEORGIAN SMALL LETTER DON
to_upper(16#2D03)->
    16#10A3;
%% GEORGIAN SMALL LETTER EN
to_upper(16#2D04)->
    16#10A4;
%% GEORGIAN SMALL LETTER VIN
to_upper(16#2D05)->
    16#10A5;
%% GEORGIAN SMALL LETTER ZEN
to_upper(16#2D06)->
    16#10A6;
%% GEORGIAN SMALL LETTER TAN
to_upper(16#2D07)->
    16#10A7;
%% GEORGIAN SMALL LETTER IN
to_upper(16#2D08)->
    16#10A8;
%% GEORGIAN SMALL LETTER KAN
to_upper(16#2D09)->
    16#10A9;
%% GEORGIAN SMALL LETTER LAS
to_upper(16#2D0A)->
    16#10AA;
%% GEORGIAN SMALL LETTER MAN
to_upper(16#2D0B)->
    16#10AB;
%% GEORGIAN SMALL LETTER NAR
to_upper(16#2D0C)->
    16#10AC;
%% GEORGIAN SMALL LETTER ON
to_upper(16#2D0D)->
    16#10AD;
%% GEORGIAN SMALL LETTER PAR
to_upper(16#2D0E)->
    16#10AE;
%% GEORGIAN SMALL LETTER ZHAR
to_upper(16#2D0F)->
    16#10AF;
%% GEORGIAN SMALL LETTER RAE
to_upper(16#2D10)->
    16#10B0;
%% GEORGIAN SMALL LETTER SAN
to_upper(16#2D11)->
    16#10B1;
%% GEORGIAN SMALL LETTER TAR
to_upper(16#2D12)->
    16#10B2;
%% GEORGIAN SMALL LETTER UN
to_upper(16#2D13)->
    16#10B3;
%% GEORGIAN SMALL LETTER PHAR
to_upper(16#2D14)->
    16#10B4;
%% GEORGIAN SMALL LETTER KHAR
to_upper(16#2D15)->
    16#10B5;
%% GEORGIAN SMALL LETTER GHAN
to_upper(16#2D16)->
    16#10B6;
%% GEORGIAN SMALL LETTER QAR
to_upper(16#2D17)->
    16#10B7;
%% GEORGIAN SMALL LETTER SHIN
to_upper(16#2D18)->
    16#10B8;
%% GEORGIAN SMALL LETTER CHIN
to_upper(16#2D19)->
    16#10B9;
%% GEORGIAN SMALL LETTER CAN
to_upper(16#2D1A)->
    16#10BA;
%% GEORGIAN SMALL LETTER JIL
to_upper(16#2D1B)->
    16#10BB;
%% GEORGIAN SMALL LETTER CIL
to_upper(16#2D1C)->
    16#10BC;
%% GEORGIAN SMALL LETTER CHAR
to_upper(16#2D1D)->
    16#10BD;
%% GEORGIAN SMALL LETTER XAN
to_upper(16#2D1E)->
    16#10BE;
%% GEORGIAN SMALL LETTER JHAN
to_upper(16#2D1F)->
    16#10BF;
%% GEORGIAN SMALL LETTER HAE
to_upper(16#2D20)->
    16#10C0;
%% GEORGIAN SMALL LETTER HE
to_upper(16#2D21)->
    16#10C1;
%% GEORGIAN SMALL LETTER HIE
to_upper(16#2D22)->
    16#10C2;
%% GEORGIAN SMALL LETTER WE
to_upper(16#2D23)->
    16#10C3;
%% GEORGIAN SMALL LETTER HAR
to_upper(16#2D24)->
    16#10C4;
%% GEORGIAN SMALL LETTER HOE
to_upper(16#2D25)->
    16#10C5;
%% GEORGIAN SMALL LETTER YN
to_upper(16#2D27)->
    16#10C7;
%% GEORGIAN SMALL LETTER AEN
to_upper(16#2D2D)->
    16#10CD;
%% CYRILLIC SMALL LETTER ZEMLYA
to_upper(16#A641)->
    16#A640;
%% CYRILLIC SMALL LETTER DZELO
to_upper(16#A643)->
    16#A642;
%% CYRILLIC SMALL LETTER REVERSED DZE
to_upper(16#A645)->
    16#A644;
%% CYRILLIC SMALL LETTER IOTA
to_upper(16#A647)->
    16#A646;
%% CYRILLIC SMALL LETTER DJERV
to_upper(16#A649)->
    16#A648;
%% CYRILLIC SMALL LETTER MONOGRAPH UK
to_upper(16#A64B)->
    16#A64A;
%% CYRILLIC SMALL LETTER BROAD OMEGA
to_upper(16#A64D)->
    16#A64C;
%% CYRILLIC SMALL LETTER NEUTRAL YER
to_upper(16#A64F)->
    16#A64E;
%% CYRILLIC SMALL LETTER YERU WITH BACK YER
to_upper(16#A651)->
    16#A650;
%% CYRILLIC SMALL LETTER IOTIFIED YAT
to_upper(16#A653)->
    16#A652;
%% CYRILLIC SMALL LETTER REVERSED YU
to_upper(16#A655)->
    16#A654;
%% CYRILLIC SMALL LETTER IOTIFIED A
to_upper(16#A657)->
    16#A656;
%% CYRILLIC SMALL LETTER CLOSED LITTLE YUS
to_upper(16#A659)->
    16#A658;
%% CYRILLIC SMALL LETTER BLENDED YUS
to_upper(16#A65B)->
    16#A65A;
%% CYRILLIC SMALL LETTER IOTIFIED CLOSED LITTLE YUS
to_upper(16#A65D)->
    16#A65C;
%% CYRILLIC SMALL LETTER YN
to_upper(16#A65F)->
    16#A65E;
%% CYRILLIC SMALL LETTER REVERSED TSE
to_upper(16#A661)->
    16#A660;
%% CYRILLIC SMALL LETTER SOFT DE
to_upper(16#A663)->
    16#A662;
%% CYRILLIC SMALL LETTER SOFT EL
to_upper(16#A665)->
    16#A664;
%% CYRILLIC SMALL LETTER SOFT EM
to_upper(16#A667)->
    16#A666;
%% CYRILLIC SMALL LETTER MONOCULAR O
to_upper(16#A669)->
    16#A668;
%% CYRILLIC SMALL LETTER BINOCULAR O
to_upper(16#A66B)->
    16#A66A;
%% CYRILLIC SMALL LETTER DOUBLE MONOCULAR O
to_upper(16#A66D)->
    16#A66C;
%% CYRILLIC SMALL LETTER DWE
to_upper(16#A681)->
    16#A680;
%% CYRILLIC SMALL LETTER DZWE
to_upper(16#A683)->
    16#A682;
%% CYRILLIC SMALL LETTER ZHWE
to_upper(16#A685)->
    16#A684;
%% CYRILLIC SMALL LETTER CCHE
to_upper(16#A687)->
    16#A686;
%% CYRILLIC SMALL LETTER DZZE
to_upper(16#A689)->
    16#A688;
%% CYRILLIC SMALL LETTER TE WITH MIDDLE HOOK
to_upper(16#A68B)->
    16#A68A;
%% CYRILLIC SMALL LETTER TWE
to_upper(16#A68D)->
    16#A68C;
%% CYRILLIC SMALL LETTER TSWE
to_upper(16#A68F)->
    16#A68E;
%% CYRILLIC SMALL LETTER TSSE
to_upper(16#A691)->
    16#A690;
%% CYRILLIC SMALL LETTER TCHE
to_upper(16#A693)->
    16#A692;
%% CYRILLIC SMALL LETTER HWE
to_upper(16#A695)->
    16#A694;
%% CYRILLIC SMALL LETTER SHWE
to_upper(16#A697)->
    16#A696;
%% LATIN SMALL LETTER EGYPTOLOGICAL ALEF
to_upper(16#A723)->
    16#A722;
%% LATIN SMALL LETTER EGYPTOLOGICAL AIN
to_upper(16#A725)->
    16#A724;
%% LATIN SMALL LETTER HENG
to_upper(16#A727)->
    16#A726;
%% LATIN SMALL LETTER TZ
to_upper(16#A729)->
    16#A728;
%% LATIN SMALL LETTER TRESILLO
to_upper(16#A72B)->
    16#A72A;
%% LATIN SMALL LETTER CUATRILLO
to_upper(16#A72D)->
    16#A72C;
%% LATIN SMALL LETTER CUATRILLO WITH COMMA
to_upper(16#A72F)->
    16#A72E;
%% LATIN SMALL LETTER AA
to_upper(16#A733)->
    16#A732;
%% LATIN SMALL LETTER AO
to_upper(16#A735)->
    16#A734;
%% LATIN SMALL LETTER AU
to_upper(16#A737)->
    16#A736;
%% LATIN SMALL LETTER AV
to_upper(16#A739)->
    16#A738;
%% LATIN SMALL LETTER AV WITH HORIZONTAL BAR
to_upper(16#A73B)->
    16#A73A;
%% LATIN SMALL LETTER AY
to_upper(16#A73D)->
    16#A73C;
%% LATIN SMALL LETTER REVERSED C WITH DOT
to_upper(16#A73F)->
    16#A73E;
%% LATIN SMALL LETTER K WITH STROKE
to_upper(16#A741)->
    16#A740;
%% LATIN SMALL LETTER K WITH DIAGONAL STROKE
to_upper(16#A743)->
    16#A742;
%% LATIN SMALL LETTER K WITH STROKE AND DIAGONAL STROKE
to_upper(16#A745)->
    16#A744;
%% LATIN SMALL LETTER BROKEN L
to_upper(16#A747)->
    16#A746;
%% LATIN SMALL LETTER L WITH HIGH STROKE
to_upper(16#A749)->
    16#A748;
%% LATIN SMALL LETTER O WITH LONG STROKE OVERLAY
to_upper(16#A74B)->
    16#A74A;
%% LATIN SMALL LETTER O WITH LOOP
to_upper(16#A74D)->
    16#A74C;
%% LATIN SMALL LETTER OO
to_upper(16#A74F)->
    16#A74E;
%% LATIN SMALL LETTER P WITH STROKE THROUGH DESCENDER
to_upper(16#A751)->
    16#A750;
%% LATIN SMALL LETTER P WITH FLOURISH
to_upper(16#A753)->
    16#A752;
%% LATIN SMALL LETTER P WITH SQUIRREL TAIL
to_upper(16#A755)->
    16#A754;
%% LATIN SMALL LETTER Q WITH STROKE THROUGH DESCENDER
to_upper(16#A757)->
    16#A756;
%% LATIN SMALL LETTER Q WITH DIAGONAL STROKE
to_upper(16#A759)->
    16#A758;
%% LATIN SMALL LETTER R ROTUNDA
to_upper(16#A75B)->
    16#A75A;
%% LATIN SMALL LETTER RUM ROTUNDA
to_upper(16#A75D)->
    16#A75C;
%% LATIN SMALL LETTER V WITH DIAGONAL STROKE
to_upper(16#A75F)->
    16#A75E;
%% LATIN SMALL LETTER VY
to_upper(16#A761)->
    16#A760;
%% LATIN SMALL LETTER VISIGOTHIC Z
to_upper(16#A763)->
    16#A762;
%% LATIN SMALL LETTER THORN WITH STROKE
to_upper(16#A765)->
    16#A764;
%% LATIN SMALL LETTER THORN WITH STROKE THROUGH DESCENDER
to_upper(16#A767)->
    16#A766;
%% LATIN SMALL LETTER VEND
to_upper(16#A769)->
    16#A768;
%% LATIN SMALL LETTER ET
to_upper(16#A76B)->
    16#A76A;
%% LATIN SMALL LETTER IS
to_upper(16#A76D)->
    16#A76C;
%% LATIN SMALL LETTER CON
to_upper(16#A76F)->
    16#A76E;
%% LATIN SMALL LETTER INSULAR D
to_upper(16#A77A)->
    16#A779;
%% LATIN SMALL LETTER INSULAR F
to_upper(16#A77C)->
    16#A77B;
%% LATIN SMALL LETTER TURNED INSULAR G
to_upper(16#A77F)->
    16#A77E;
%% LATIN SMALL LETTER TURNED L
to_upper(16#A781)->
    16#A780;
%% LATIN SMALL LETTER INSULAR R
to_upper(16#A783)->
    16#A782;
%% LATIN SMALL LETTER INSULAR S
to_upper(16#A785)->
    16#A784;
%% LATIN SMALL LETTER INSULAR T
to_upper(16#A787)->
    16#A786;
%% LATIN SMALL LETTER SALTILLO
to_upper(16#A78C)->
    16#A78B;
%% LATIN SMALL LETTER N WITH DESCENDER
to_upper(16#A791)->
    16#A790;
%% LATIN SMALL LETTER C WITH BAR
to_upper(16#A793)->
    16#A792;
%% LATIN SMALL LETTER G WITH OBLIQUE STROKE
to_upper(16#A7A1)->
    16#A7A0;
%% LATIN SMALL LETTER K WITH OBLIQUE STROKE
to_upper(16#A7A3)->
    16#A7A2;
%% LATIN SMALL LETTER N WITH OBLIQUE STROKE
to_upper(16#A7A5)->
    16#A7A4;
%% LATIN SMALL LETTER R WITH OBLIQUE STROKE
to_upper(16#A7A7)->
    16#A7A6;
%% LATIN SMALL LETTER S WITH OBLIQUE STROKE
to_upper(16#A7A9)->
    16#A7A8;
%% FULLWIDTH LATIN SMALL LETTER A
to_upper(16#FF41)->
    16#FF21;
%% FULLWIDTH LATIN SMALL LETTER B
to_upper(16#FF42)->
    16#FF22;
%% FULLWIDTH LATIN SMALL LETTER C
to_upper(16#FF43)->
    16#FF23;
%% FULLWIDTH LATIN SMALL LETTER D
to_upper(16#FF44)->
    16#FF24;
%% FULLWIDTH LATIN SMALL LETTER E
to_upper(16#FF45)->
    16#FF25;
%% FULLWIDTH LATIN SMALL LETTER F
to_upper(16#FF46)->
    16#FF26;
%% FULLWIDTH LATIN SMALL LETTER G
to_upper(16#FF47)->
    16#FF27;
%% FULLWIDTH LATIN SMALL LETTER H
to_upper(16#FF48)->
    16#FF28;
%% FULLWIDTH LATIN SMALL LETTER I
to_upper(16#FF49)->
    16#FF29;
%% FULLWIDTH LATIN SMALL LETTER J
to_upper(16#FF4A)->
    16#FF2A;
%% FULLWIDTH LATIN SMALL LETTER K
to_upper(16#FF4B)->
    16#FF2B;
%% FULLWIDTH LATIN SMALL LETTER L
to_upper(16#FF4C)->
    16#FF2C;
%% FULLWIDTH LATIN SMALL LETTER M
to_upper(16#FF4D)->
    16#FF2D;
%% FULLWIDTH LATIN SMALL LETTER N
to_upper(16#FF4E)->
    16#FF2E;
%% FULLWIDTH LATIN SMALL LETTER O
to_upper(16#FF4F)->
    16#FF2F;
%% FULLWIDTH LATIN SMALL LETTER P
to_upper(16#FF50)->
    16#FF30;
%% FULLWIDTH LATIN SMALL LETTER Q
to_upper(16#FF51)->
    16#FF31;
%% FULLWIDTH LATIN SMALL LETTER R
to_upper(16#FF52)->
    16#FF32;
%% FULLWIDTH LATIN SMALL LETTER S
to_upper(16#FF53)->
    16#FF33;
%% FULLWIDTH LATIN SMALL LETTER T
to_upper(16#FF54)->
    16#FF34;
%% FULLWIDTH LATIN SMALL LETTER U
to_upper(16#FF55)->
    16#FF35;
%% FULLWIDTH LATIN SMALL LETTER V
to_upper(16#FF56)->
    16#FF36;
%% FULLWIDTH LATIN SMALL LETTER W
to_upper(16#FF57)->
    16#FF37;
%% FULLWIDTH LATIN SMALL LETTER X
to_upper(16#FF58)->
    16#FF38;
%% FULLWIDTH LATIN SMALL LETTER Y
to_upper(16#FF59)->
    16#FF39;
%% FULLWIDTH LATIN SMALL LETTER Z
to_upper(16#FF5A)->
    16#FF3A;
%% DESERET SMALL LETTER LONG I
to_upper(16#10428)->
    16#10400;
%% DESERET SMALL LETTER LONG E
to_upper(16#10429)->
    16#10401;
%% DESERET SMALL LETTER LONG A
to_upper(16#1042A)->
    16#10402;
%% DESERET SMALL LETTER LONG AH
to_upper(16#1042B)->
    16#10403;
%% DESERET SMALL LETTER LONG O
to_upper(16#1042C)->
    16#10404;
%% DESERET SMALL LETTER LONG OO
to_upper(16#1042D)->
    16#10405;
%% DESERET SMALL LETTER SHORT I
to_upper(16#1042E)->
    16#10406;
%% DESERET SMALL LETTER SHORT E
to_upper(16#1042F)->
    16#10407;
%% DESERET SMALL LETTER SHORT A
to_upper(16#10430)->
    16#10408;
%% DESERET SMALL LETTER SHORT AH
to_upper(16#10431)->
    16#10409;
%% DESERET SMALL LETTER SHORT O
to_upper(16#10432)->
    16#1040A;
%% DESERET SMALL LETTER SHORT OO
to_upper(16#10433)->
    16#1040B;
%% DESERET SMALL LETTER AY
to_upper(16#10434)->
    16#1040C;
%% DESERET SMALL LETTER OW
to_upper(16#10435)->
    16#1040D;
%% DESERET SMALL LETTER WU
to_upper(16#10436)->
    16#1040E;
%% DESERET SMALL LETTER YEE
to_upper(16#10437)->
    16#1040F;
%% DESERET SMALL LETTER H
to_upper(16#10438)->
    16#10410;
%% DESERET SMALL LETTER PEE
to_upper(16#10439)->
    16#10411;
%% DESERET SMALL LETTER BEE
to_upper(16#1043A)->
    16#10412;
%% DESERET SMALL LETTER TEE
to_upper(16#1043B)->
    16#10413;
%% DESERET SMALL LETTER DEE
to_upper(16#1043C)->
    16#10414;
%% DESERET SMALL LETTER CHEE
to_upper(16#1043D)->
    16#10415;
%% DESERET SMALL LETTER JEE
to_upper(16#1043E)->
    16#10416;
%% DESERET SMALL LETTER KAY
to_upper(16#1043F)->
    16#10417;
%% DESERET SMALL LETTER GAY
to_upper(16#10440)->
    16#10418;
%% DESERET SMALL LETTER EF
to_upper(16#10441)->
    16#10419;
%% DESERET SMALL LETTER VEE
to_upper(16#10442)->
    16#1041A;
%% DESERET SMALL LETTER ETH
to_upper(16#10443)->
    16#1041B;
%% DESERET SMALL LETTER THEE
to_upper(16#10444)->
    16#1041C;
%% DESERET SMALL LETTER ES
to_upper(16#10445)->
    16#1041D;
%% DESERET SMALL LETTER ZEE
to_upper(16#10446)->
    16#1041E;
%% DESERET SMALL LETTER ESH
to_upper(16#10447)->
    16#1041F;
%% DESERET SMALL LETTER ZHEE
to_upper(16#10448)->
    16#10420;
%% DESERET SMALL LETTER ER
to_upper(16#10449)->
    16#10421;
%% DESERET SMALL LETTER EL
to_upper(16#1044A)->
    16#10422;
%% DESERET SMALL LETTER EM
to_upper(16#1044B)->
    16#10423;
%% DESERET SMALL LETTER EN
to_upper(16#1044C)->
    16#10424;
%% DESERET SMALL LETTER ENG
to_upper(16#1044D)->
    16#10425;
%% DESERET SMALL LETTER OI
to_upper(16#1044E)->
    16#10426;
%% DESERET SMALL LETTER EW
to_upper(16#1044F)->
    16#10427;
to_upper(C) ->
    C.

%% LATIN CAPITAL LETTER A
to_lower(16#0041)->
    16#0061;
%% LATIN CAPITAL LETTER B
to_lower(16#0042)->
    16#0062;
%% LATIN CAPITAL LETTER C
to_lower(16#0043)->
    16#0063;
%% LATIN CAPITAL LETTER D
to_lower(16#0044)->
    16#0064;
%% LATIN CAPITAL LETTER E
to_lower(16#0045)->
    16#0065;
%% LATIN CAPITAL LETTER F
to_lower(16#0046)->
    16#0066;
%% LATIN CAPITAL LETTER G
to_lower(16#0047)->
    16#0067;
%% LATIN CAPITAL LETTER H
to_lower(16#0048)->
    16#0068;
%% LATIN CAPITAL LETTER I
to_lower(16#0049)->
    16#0069;
%% LATIN CAPITAL LETTER J
to_lower(16#004A)->
    16#006A;
%% LATIN CAPITAL LETTER K
to_lower(16#004B)->
    16#006B;
%% LATIN CAPITAL LETTER L
to_lower(16#004C)->
    16#006C;
%% LATIN CAPITAL LETTER M
to_lower(16#004D)->
    16#006D;
%% LATIN CAPITAL LETTER N
to_lower(16#004E)->
    16#006E;
%% LATIN CAPITAL LETTER O
to_lower(16#004F)->
    16#006F;
%% LATIN CAPITAL LETTER P
to_lower(16#0050)->
    16#0070;
%% LATIN CAPITAL LETTER Q
to_lower(16#0051)->
    16#0071;
%% LATIN CAPITAL LETTER R
to_lower(16#0052)->
    16#0072;
%% LATIN CAPITAL LETTER S
to_lower(16#0053)->
    16#0073;
%% LATIN CAPITAL LETTER T
to_lower(16#0054)->
    16#0074;
%% LATIN CAPITAL LETTER U
to_lower(16#0055)->
    16#0075;
%% LATIN CAPITAL LETTER V
to_lower(16#0056)->
    16#0076;
%% LATIN CAPITAL LETTER W
to_lower(16#0057)->
    16#0077;
%% LATIN CAPITAL LETTER X
to_lower(16#0058)->
    16#0078;
%% LATIN CAPITAL LETTER Y
to_lower(16#0059)->
    16#0079;
%% LATIN CAPITAL LETTER Z
to_lower(16#005A)->
    16#007A;
%% LATIN CAPITAL LETTER A WITH GRAVE
to_lower(16#00C0)->
    16#00E0;
%% LATIN CAPITAL LETTER A WITH ACUTE
to_lower(16#00C1)->
    16#00E1;
%% LATIN CAPITAL LETTER A WITH CIRCUMFLEX
to_lower(16#00C2)->
    16#00E2;
%% LATIN CAPITAL LETTER A WITH TILDE
to_lower(16#00C3)->
    16#00E3;
%% LATIN CAPITAL LETTER A WITH DIAERESIS
to_lower(16#00C4)->
    16#00E4;
%% LATIN CAPITAL LETTER A WITH RING ABOVE
to_lower(16#00C5)->
    16#00E5;
%% LATIN CAPITAL LETTER AE
to_lower(16#00C6)->
    16#00E6;
%% LATIN CAPITAL LETTER C WITH CEDILLA
to_lower(16#00C7)->
    16#00E7;
%% LATIN CAPITAL LETTER E WITH GRAVE
to_lower(16#00C8)->
    16#00E8;
%% LATIN CAPITAL LETTER E WITH ACUTE
to_lower(16#00C9)->
    16#00E9;
%% LATIN CAPITAL LETTER E WITH CIRCUMFLEX
to_lower(16#00CA)->
    16#00EA;
%% LATIN CAPITAL LETTER E WITH DIAERESIS
to_lower(16#00CB)->
    16#00EB;
%% LATIN CAPITAL LETTER I WITH GRAVE
to_lower(16#00CC)->
    16#00EC;
%% LATIN CAPITAL LETTER I WITH ACUTE
to_lower(16#00CD)->
    16#00ED;
%% LATIN CAPITAL LETTER I WITH CIRCUMFLEX
to_lower(16#00CE)->
    16#00EE;
%% LATIN CAPITAL LETTER I WITH DIAERESIS
to_lower(16#00CF)->
    16#00EF;
%% LATIN CAPITAL LETTER ETH
to_lower(16#00D0)->
    16#00F0;
%% LATIN CAPITAL LETTER N WITH TILDE
to_lower(16#00D1)->
    16#00F1;
%% LATIN CAPITAL LETTER O WITH GRAVE
to_lower(16#00D2)->
    16#00F2;
%% LATIN CAPITAL LETTER O WITH ACUTE
to_lower(16#00D3)->
    16#00F3;
%% LATIN CAPITAL LETTER O WITH CIRCUMFLEX
to_lower(16#00D4)->
    16#00F4;
%% LATIN CAPITAL LETTER O WITH TILDE
to_lower(16#00D5)->
    16#00F5;
%% LATIN CAPITAL LETTER O WITH DIAERESIS
to_lower(16#00D6)->
    16#00F6;
%% LATIN CAPITAL LETTER O WITH STROKE
to_lower(16#00D8)->
    16#00F8;
%% LATIN CAPITAL LETTER U WITH GRAVE
to_lower(16#00D9)->
    16#00F9;
%% LATIN CAPITAL LETTER U WITH ACUTE
to_lower(16#00DA)->
    16#00FA;
%% LATIN CAPITAL LETTER U WITH CIRCUMFLEX
to_lower(16#00DB)->
    16#00FB;
%% LATIN CAPITAL LETTER U WITH DIAERESIS
to_lower(16#00DC)->
    16#00FC;
%% LATIN CAPITAL LETTER Y WITH ACUTE
to_lower(16#00DD)->
    16#00FD;
%% LATIN CAPITAL LETTER THORN
to_lower(16#00DE)->
    16#00FE;
%% LATIN CAPITAL LETTER A WITH MACRON
to_lower(16#0100)->
    16#0101;
%% LATIN CAPITAL LETTER A WITH BREVE
to_lower(16#0102)->
    16#0103;
%% LATIN CAPITAL LETTER A WITH OGONEK
to_lower(16#0104)->
    16#0105;
%% LATIN CAPITAL LETTER C WITH ACUTE
to_lower(16#0106)->
    16#0107;
%% LATIN CAPITAL LETTER C WITH CIRCUMFLEX
to_lower(16#0108)->
    16#0109;
%% LATIN CAPITAL LETTER C WITH DOT ABOVE
to_lower(16#010A)->
    16#010B;
%% LATIN CAPITAL LETTER C WITH CARON
to_lower(16#010C)->
    16#010D;
%% LATIN CAPITAL LETTER D WITH CARON
to_lower(16#010E)->
    16#010F;
%% LATIN CAPITAL LETTER D WITH STROKE
to_lower(16#0110)->
    16#0111;
%% LATIN CAPITAL LETTER E WITH MACRON
to_lower(16#0112)->
    16#0113;
%% LATIN CAPITAL LETTER E WITH BREVE
to_lower(16#0114)->
    16#0115;
%% LATIN CAPITAL LETTER E WITH DOT ABOVE
to_lower(16#0116)->
    16#0117;
%% LATIN CAPITAL LETTER E WITH OGONEK
to_lower(16#0118)->
    16#0119;
%% LATIN CAPITAL LETTER E WITH CARON
to_lower(16#011A)->
    16#011B;
%% LATIN CAPITAL LETTER G WITH CIRCUMFLEX
to_lower(16#011C)->
    16#011D;
%% LATIN CAPITAL LETTER G WITH BREVE
to_lower(16#011E)->
    16#011F;
%% LATIN CAPITAL LETTER G WITH DOT ABOVE
to_lower(16#0120)->
    16#0121;
%% LATIN CAPITAL LETTER G WITH CEDILLA
to_lower(16#0122)->
    16#0123;
%% LATIN CAPITAL LETTER H WITH CIRCUMFLEX
to_lower(16#0124)->
    16#0125;
%% LATIN CAPITAL LETTER H WITH STROKE
to_lower(16#0126)->
    16#0127;
%% LATIN CAPITAL LETTER I WITH TILDE
to_lower(16#0128)->
    16#0129;
%% LATIN CAPITAL LETTER I WITH MACRON
to_lower(16#012A)->
    16#012B;
%% LATIN CAPITAL LETTER I WITH BREVE
to_lower(16#012C)->
    16#012D;
%% LATIN CAPITAL LETTER I WITH OGONEK
to_lower(16#012E)->
    16#012F;
%% LATIN CAPITAL LETTER I WITH DOT ABOVE
to_lower(16#0130)->
    16#0069;
%% LATIN CAPITAL LIGATURE IJ
to_lower(16#0132)->
    16#0133;
%% LATIN CAPITAL LETTER J WITH CIRCUMFLEX
to_lower(16#0134)->
    16#0135;
%% LATIN CAPITAL LETTER K WITH CEDILLA
to_lower(16#0136)->
    16#0137;
%% LATIN CAPITAL LETTER L WITH ACUTE
to_lower(16#0139)->
    16#013A;
%% LATIN CAPITAL LETTER L WITH CEDILLA
to_lower(16#013B)->
    16#013C;
%% LATIN CAPITAL LETTER L WITH CARON
to_lower(16#013D)->
    16#013E;
%% LATIN CAPITAL LETTER L WITH MIDDLE DOT
to_lower(16#013F)->
    16#0140;
%% LATIN CAPITAL LETTER L WITH STROKE
to_lower(16#0141)->
    16#0142;
%% LATIN CAPITAL LETTER N WITH ACUTE
to_lower(16#0143)->
    16#0144;
%% LATIN CAPITAL LETTER N WITH CEDILLA
to_lower(16#0145)->
    16#0146;
%% LATIN CAPITAL LETTER N WITH CARON
to_lower(16#0147)->
    16#0148;
%% LATIN CAPITAL LETTER ENG
to_lower(16#014A)->
    16#014B;
%% LATIN CAPITAL LETTER O WITH MACRON
to_lower(16#014C)->
    16#014D;
%% LATIN CAPITAL LETTER O WITH BREVE
to_lower(16#014E)->
    16#014F;
%% LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
to_lower(16#0150)->
    16#0151;
%% LATIN CAPITAL LIGATURE OE
to_lower(16#0152)->
    16#0153;
%% LATIN CAPITAL LETTER R WITH ACUTE
to_lower(16#0154)->
    16#0155;
%% LATIN CAPITAL LETTER R WITH CEDILLA
to_lower(16#0156)->
    16#0157;
%% LATIN CAPITAL LETTER R WITH CARON
to_lower(16#0158)->
    16#0159;
%% LATIN CAPITAL LETTER S WITH ACUTE
to_lower(16#015A)->
    16#015B;
%% LATIN CAPITAL LETTER S WITH CIRCUMFLEX
to_lower(16#015C)->
    16#015D;
%% LATIN CAPITAL LETTER S WITH CEDILLA
to_lower(16#015E)->
    16#015F;
%% LATIN CAPITAL LETTER S WITH CARON
to_lower(16#0160)->
    16#0161;
%% LATIN CAPITAL LETTER T WITH CEDILLA
to_lower(16#0162)->
    16#0163;
%% LATIN CAPITAL LETTER T WITH CARON
to_lower(16#0164)->
    16#0165;
%% LATIN CAPITAL LETTER T WITH STROKE
to_lower(16#0166)->
    16#0167;
%% LATIN CAPITAL LETTER U WITH TILDE
to_lower(16#0168)->
    16#0169;
%% LATIN CAPITAL LETTER U WITH MACRON
to_lower(16#016A)->
    16#016B;
%% LATIN CAPITAL LETTER U WITH BREVE
to_lower(16#016C)->
    16#016D;
%% LATIN CAPITAL LETTER U WITH RING ABOVE
to_lower(16#016E)->
    16#016F;
%% LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
to_lower(16#0170)->
    16#0171;
%% LATIN CAPITAL LETTER U WITH OGONEK
to_lower(16#0172)->
    16#0173;
%% LATIN CAPITAL LETTER W WITH CIRCUMFLEX
to_lower(16#0174)->
    16#0175;
%% LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
to_lower(16#0176)->
    16#0177;
%% LATIN CAPITAL LETTER Y WITH DIAERESIS
to_lower(16#0178)->
    16#00FF;
%% LATIN CAPITAL LETTER Z WITH ACUTE
to_lower(16#0179)->
    16#017A;
%% LATIN CAPITAL LETTER Z WITH DOT ABOVE
to_lower(16#017B)->
    16#017C;
%% LATIN CAPITAL LETTER Z WITH CARON
to_lower(16#017D)->
    16#017E;
%% LATIN CAPITAL LETTER B WITH HOOK
to_lower(16#0181)->
    16#0253;
%% LATIN CAPITAL LETTER B WITH TOPBAR
to_lower(16#0182)->
    16#0183;
%% LATIN CAPITAL LETTER TONE SIX
to_lower(16#0184)->
    16#0185;
%% LATIN CAPITAL LETTER OPEN O
to_lower(16#0186)->
    16#0254;
%% LATIN CAPITAL LETTER C WITH HOOK
to_lower(16#0187)->
    16#0188;
%% LATIN CAPITAL LETTER AFRICAN D
to_lower(16#0189)->
    16#0256;
%% LATIN CAPITAL LETTER D WITH HOOK
to_lower(16#018A)->
    16#0257;
%% LATIN CAPITAL LETTER D WITH TOPBAR
to_lower(16#018B)->
    16#018C;
%% LATIN CAPITAL LETTER REVERSED E
to_lower(16#018E)->
    16#01DD;
%% LATIN CAPITAL LETTER SCHWA
to_lower(16#018F)->
    16#0259;
%% LATIN CAPITAL LETTER OPEN E
to_lower(16#0190)->
    16#025B;
%% LATIN CAPITAL LETTER F WITH HOOK
to_lower(16#0191)->
    16#0192;
%% LATIN CAPITAL LETTER G WITH HOOK
to_lower(16#0193)->
    16#0260;
%% LATIN CAPITAL LETTER GAMMA
to_lower(16#0194)->
    16#0263;
%% LATIN CAPITAL LETTER IOTA
to_lower(16#0196)->
    16#0269;
%% LATIN CAPITAL LETTER I WITH STROKE
to_lower(16#0197)->
    16#0268;
%% LATIN CAPITAL LETTER K WITH HOOK
to_lower(16#0198)->
    16#0199;
%% LATIN CAPITAL LETTER TURNED M
to_lower(16#019C)->
    16#026F;
%% LATIN CAPITAL LETTER N WITH LEFT HOOK
to_lower(16#019D)->
    16#0272;
%% LATIN CAPITAL LETTER O WITH MIDDLE TILDE
to_lower(16#019F)->
    16#0275;
%% LATIN CAPITAL LETTER O WITH HORN
to_lower(16#01A0)->
    16#01A1;
%% LATIN CAPITAL LETTER OI
to_lower(16#01A2)->
    16#01A3;
%% LATIN CAPITAL LETTER P WITH HOOK
to_lower(16#01A4)->
    16#01A5;
%% LATIN LETTER YR
to_lower(16#01A6)->
    16#0280;
%% LATIN CAPITAL LETTER TONE TWO
to_lower(16#01A7)->
    16#01A8;
%% LATIN CAPITAL LETTER ESH
to_lower(16#01A9)->
    16#0283;
%% LATIN CAPITAL LETTER T WITH HOOK
to_lower(16#01AC)->
    16#01AD;
%% LATIN CAPITAL LETTER T WITH RETROFLEX HOOK
to_lower(16#01AE)->
    16#0288;
%% LATIN CAPITAL LETTER U WITH HORN
to_lower(16#01AF)->
    16#01B0;
%% LATIN CAPITAL LETTER UPSILON
to_lower(16#01B1)->
    16#028A;
%% LATIN CAPITAL LETTER V WITH HOOK
to_lower(16#01B2)->
    16#028B;
%% LATIN CAPITAL LETTER Y WITH HOOK
to_lower(16#01B3)->
    16#01B4;
%% LATIN CAPITAL LETTER Z WITH STROKE
to_lower(16#01B5)->
    16#01B6;
%% LATIN CAPITAL LETTER EZH
to_lower(16#01B7)->
    16#0292;
%% LATIN CAPITAL LETTER EZH REVERSED
to_lower(16#01B8)->
    16#01B9;
%% LATIN CAPITAL LETTER TONE FIVE
to_lower(16#01BC)->
    16#01BD;
%% LATIN CAPITAL LETTER DZ WITH CARON
to_lower(16#01C4)->
    16#01C6;
%% LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON
to_lower(16#01C5)->
    16#01C6;
%% LATIN CAPITAL LETTER LJ
to_lower(16#01C7)->
    16#01C9;
%% LATIN CAPITAL LETTER L WITH SMALL LETTER J
to_lower(16#01C8)->
    16#01C9;
%% LATIN CAPITAL LETTER NJ
to_lower(16#01CA)->
    16#01CC;
%% LATIN CAPITAL LETTER N WITH SMALL LETTER J
to_lower(16#01CB)->
    16#01CC;
%% LATIN CAPITAL LETTER A WITH CARON
to_lower(16#01CD)->
    16#01CE;
%% LATIN CAPITAL LETTER I WITH CARON
to_lower(16#01CF)->
    16#01D0;
%% LATIN CAPITAL LETTER O WITH CARON
to_lower(16#01D1)->
    16#01D2;
%% LATIN CAPITAL LETTER U WITH CARON
to_lower(16#01D3)->
    16#01D4;
%% LATIN CAPITAL LETTER U WITH DIAERESIS AND MACRON
to_lower(16#01D5)->
    16#01D6;
%% LATIN CAPITAL LETTER U WITH DIAERESIS AND ACUTE
to_lower(16#01D7)->
    16#01D8;
%% LATIN CAPITAL LETTER U WITH DIAERESIS AND CARON
to_lower(16#01D9)->
    16#01DA;
%% LATIN CAPITAL LETTER U WITH DIAERESIS AND GRAVE
to_lower(16#01DB)->
    16#01DC;
%% LATIN CAPITAL LETTER A WITH DIAERESIS AND MACRON
to_lower(16#01DE)->
    16#01DF;
%% LATIN CAPITAL LETTER A WITH DOT ABOVE AND MACRON
to_lower(16#01E0)->
    16#01E1;
%% LATIN CAPITAL LETTER AE WITH MACRON
to_lower(16#01E2)->
    16#01E3;
%% LATIN CAPITAL LETTER G WITH STROKE
to_lower(16#01E4)->
    16#01E5;
%% LATIN CAPITAL LETTER G WITH CARON
to_lower(16#01E6)->
    16#01E7;
%% LATIN CAPITAL LETTER K WITH CARON
to_lower(16#01E8)->
    16#01E9;
%% LATIN CAPITAL LETTER O WITH OGONEK
to_lower(16#01EA)->
    16#01EB;
%% LATIN CAPITAL LETTER O WITH OGONEK AND MACRON
to_lower(16#01EC)->
    16#01ED;
%% LATIN CAPITAL LETTER EZH WITH CARON
to_lower(16#01EE)->
    16#01EF;
%% LATIN CAPITAL LETTER DZ
to_lower(16#01F1)->
    16#01F3;
%% LATIN CAPITAL LETTER D WITH SMALL LETTER Z
to_lower(16#01F2)->
    16#01F3;
%% LATIN CAPITAL LETTER G WITH ACUTE
to_lower(16#01F4)->
    16#01F5;
%% LATIN CAPITAL LETTER HWAIR
to_lower(16#01F6)->
    16#0195;
%% LATIN CAPITAL LETTER WYNN
to_lower(16#01F7)->
    16#01BF;
%% LATIN CAPITAL LETTER N WITH GRAVE
to_lower(16#01F8)->
    16#01F9;
%% LATIN CAPITAL LETTER A WITH RING ABOVE AND ACUTE
to_lower(16#01FA)->
    16#01FB;
%% LATIN CAPITAL LETTER AE WITH ACUTE
to_lower(16#01FC)->
    16#01FD;
%% LATIN CAPITAL LETTER O WITH STROKE AND ACUTE
to_lower(16#01FE)->
    16#01FF;
%% LATIN CAPITAL LETTER A WITH DOUBLE GRAVE
to_lower(16#0200)->
    16#0201;
%% LATIN CAPITAL LETTER A WITH INVERTED BREVE
to_lower(16#0202)->
    16#0203;
%% LATIN CAPITAL LETTER E WITH DOUBLE GRAVE
to_lower(16#0204)->
    16#0205;
%% LATIN CAPITAL LETTER E WITH INVERTED BREVE
to_lower(16#0206)->
    16#0207;
%% LATIN CAPITAL LETTER I WITH DOUBLE GRAVE
to_lower(16#0208)->
    16#0209;
%% LATIN CAPITAL LETTER I WITH INVERTED BREVE
to_lower(16#020A)->
    16#020B;
%% LATIN CAPITAL LETTER O WITH DOUBLE GRAVE
to_lower(16#020C)->
    16#020D;
%% LATIN CAPITAL LETTER O WITH INVERTED BREVE
to_lower(16#020E)->
    16#020F;
%% LATIN CAPITAL LETTER R WITH DOUBLE GRAVE
to_lower(16#0210)->
    16#0211;
%% LATIN CAPITAL LETTER R WITH INVERTED BREVE
to_lower(16#0212)->
    16#0213;
%% LATIN CAPITAL LETTER U WITH DOUBLE GRAVE
to_lower(16#0214)->
    16#0215;
%% LATIN CAPITAL LETTER U WITH INVERTED BREVE
to_lower(16#0216)->
    16#0217;
%% LATIN CAPITAL LETTER S WITH COMMA BELOW
to_lower(16#0218)->
    16#0219;
%% LATIN CAPITAL LETTER T WITH COMMA BELOW
to_lower(16#021A)->
    16#021B;
%% LATIN CAPITAL LETTER YOGH
to_lower(16#021C)->
    16#021D;
%% LATIN CAPITAL LETTER H WITH CARON
to_lower(16#021E)->
    16#021F;
%% LATIN CAPITAL LETTER N WITH LONG RIGHT LEG
to_lower(16#0220)->
    16#019E;
%% LATIN CAPITAL LETTER OU
to_lower(16#0222)->
    16#0223;
%% LATIN CAPITAL LETTER Z WITH HOOK
to_lower(16#0224)->
    16#0225;
%% LATIN CAPITAL LETTER A WITH DOT ABOVE
to_lower(16#0226)->
    16#0227;
%% LATIN CAPITAL LETTER E WITH CEDILLA
to_lower(16#0228)->
    16#0229;
%% LATIN CAPITAL LETTER O WITH DIAERESIS AND MACRON
to_lower(16#022A)->
    16#022B;
%% LATIN CAPITAL LETTER O WITH TILDE AND MACRON
to_lower(16#022C)->
    16#022D;
%% LATIN CAPITAL LETTER O WITH DOT ABOVE
to_lower(16#022E)->
    16#022F;
%% LATIN CAPITAL LETTER O WITH DOT ABOVE AND MACRON
to_lower(16#0230)->
    16#0231;
%% LATIN CAPITAL LETTER Y WITH MACRON
to_lower(16#0232)->
    16#0233;
%% LATIN CAPITAL LETTER A WITH STROKE
to_lower(16#023A)->
    16#2C65;
%% LATIN CAPITAL LETTER C WITH STROKE
to_lower(16#023B)->
    16#023C;
%% LATIN CAPITAL LETTER L WITH BAR
to_lower(16#023D)->
    16#019A;
%% LATIN CAPITAL LETTER T WITH DIAGONAL STROKE
to_lower(16#023E)->
    16#2C66;
%% LATIN CAPITAL LETTER GLOTTAL STOP
to_lower(16#0241)->
    16#0242;
%% LATIN CAPITAL LETTER B WITH STROKE
to_lower(16#0243)->
    16#0180;
%% LATIN CAPITAL LETTER U BAR
to_lower(16#0244)->
    16#0289;
%% LATIN CAPITAL LETTER TURNED V
to_lower(16#0245)->
    16#028C;
%% LATIN CAPITAL LETTER E WITH STROKE
to_lower(16#0246)->
    16#0247;
%% LATIN CAPITAL LETTER J WITH STROKE
to_lower(16#0248)->
    16#0249;
%% LATIN CAPITAL LETTER SMALL Q WITH HOOK TAIL
to_lower(16#024A)->
    16#024B;
%% LATIN CAPITAL LETTER R WITH STROKE
to_lower(16#024C)->
    16#024D;
%% LATIN CAPITAL LETTER Y WITH STROKE
to_lower(16#024E)->
    16#024F;
%% GREEK CAPITAL LETTER HETA
to_lower(16#0370)->
    16#0371;
%% GREEK CAPITAL LETTER ARCHAIC SAMPI
to_lower(16#0372)->
    16#0373;
%% GREEK CAPITAL LETTER PAMPHYLIAN DIGAMMA
to_lower(16#0376)->
    16#0377;
%% GREEK CAPITAL LETTER ALPHA WITH TONOS
to_lower(16#0386)->
    16#03AC;
%% GREEK CAPITAL LETTER EPSILON WITH TONOS
to_lower(16#0388)->
    16#03AD;
%% GREEK CAPITAL LETTER ETA WITH TONOS
to_lower(16#0389)->
    16#03AE;
%% GREEK CAPITAL LETTER IOTA WITH TONOS
to_lower(16#038A)->
    16#03AF;
%% GREEK CAPITAL LETTER OMICRON WITH TONOS
to_lower(16#038C)->
    16#03CC;
%% GREEK CAPITAL LETTER UPSILON WITH TONOS
to_lower(16#038E)->
    16#03CD;
%% GREEK CAPITAL LETTER OMEGA WITH TONOS
to_lower(16#038F)->
    16#03CE;
%% GREEK CAPITAL LETTER ALPHA
to_lower(16#0391)->
    16#03B1;
%% GREEK CAPITAL LETTER BETA
to_lower(16#0392)->
    16#03B2;
%% GREEK CAPITAL LETTER GAMMA
to_lower(16#0393)->
    16#03B3;
%% GREEK CAPITAL LETTER DELTA
to_lower(16#0394)->
    16#03B4;
%% GREEK CAPITAL LETTER EPSILON
to_lower(16#0395)->
    16#03B5;
%% GREEK CAPITAL LETTER ZETA
to_lower(16#0396)->
    16#03B6;
%% GREEK CAPITAL LETTER ETA
to_lower(16#0397)->
    16#03B7;
%% GREEK CAPITAL LETTER THETA
to_lower(16#0398)->
    16#03B8;
%% GREEK CAPITAL LETTER IOTA
to_lower(16#0399)->
    16#03B9;
%% GREEK CAPITAL LETTER KAPPA
to_lower(16#039A)->
    16#03BA;
%% GREEK CAPITAL LETTER LAMDA
to_lower(16#039B)->
    16#03BB;
%% GREEK CAPITAL LETTER MU
to_lower(16#039C)->
    16#03BC;
%% GREEK CAPITAL LETTER NU
to_lower(16#039D)->
    16#03BD;
%% GREEK CAPITAL LETTER XI
to_lower(16#039E)->
    16#03BE;
%% GREEK CAPITAL LETTER OMICRON
to_lower(16#039F)->
    16#03BF;
%% GREEK CAPITAL LETTER PI
to_lower(16#03A0)->
    16#03C0;
%% GREEK CAPITAL LETTER RHO
to_lower(16#03A1)->
    16#03C1;
%% GREEK CAPITAL LETTER SIGMA
to_lower(16#03A3)->
    16#03C3;
%% GREEK CAPITAL LETTER TAU
to_lower(16#03A4)->
    16#03C4;
%% GREEK CAPITAL LETTER UPSILON
to_lower(16#03A5)->
    16#03C5;
%% GREEK CAPITAL LETTER PHI
to_lower(16#03A6)->
    16#03C6;
%% GREEK CAPITAL LETTER CHI
to_lower(16#03A7)->
    16#03C7;
%% GREEK CAPITAL LETTER PSI
to_lower(16#03A8)->
    16#03C8;
%% GREEK CAPITAL LETTER OMEGA
to_lower(16#03A9)->
    16#03C9;
%% GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
to_lower(16#03AA)->
    16#03CA;
%% GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
to_lower(16#03AB)->
    16#03CB;
%% GREEK CAPITAL KAI SYMBOL
to_lower(16#03CF)->
    16#03D7;
%% GREEK LETTER ARCHAIC KOPPA
to_lower(16#03D8)->
    16#03D9;
%% GREEK LETTER STIGMA
to_lower(16#03DA)->
    16#03DB;
%% GREEK LETTER DIGAMMA
to_lower(16#03DC)->
    16#03DD;
%% GREEK LETTER KOPPA
to_lower(16#03DE)->
    16#03DF;
%% GREEK LETTER SAMPI
to_lower(16#03E0)->
    16#03E1;
%% COPTIC CAPITAL LETTER SHEI
to_lower(16#03E2)->
    16#03E3;
%% COPTIC CAPITAL LETTER FEI
to_lower(16#03E4)->
    16#03E5;
%% COPTIC CAPITAL LETTER KHEI
to_lower(16#03E6)->
    16#03E7;
%% COPTIC CAPITAL LETTER HORI
to_lower(16#03E8)->
    16#03E9;
%% COPTIC CAPITAL LETTER GANGIA
to_lower(16#03EA)->
    16#03EB;
%% COPTIC CAPITAL LETTER SHIMA
to_lower(16#03EC)->
    16#03ED;
%% COPTIC CAPITAL LETTER DEI
to_lower(16#03EE)->
    16#03EF;
%% GREEK CAPITAL THETA SYMBOL
to_lower(16#03F4)->
    16#03B8;
%% GREEK CAPITAL LETTER SHO
to_lower(16#03F7)->
    16#03F8;
%% GREEK CAPITAL LUNATE SIGMA SYMBOL
to_lower(16#03F9)->
    16#03F2;
%% GREEK CAPITAL LETTER SAN
to_lower(16#03FA)->
    16#03FB;
%% GREEK CAPITAL REVERSED LUNATE SIGMA SYMBOL
to_lower(16#03FD)->
    16#037B;
%% GREEK CAPITAL DOTTED LUNATE SIGMA SYMBOL
to_lower(16#03FE)->
    16#037C;
%% GREEK CAPITAL REVERSED DOTTED LUNATE SIGMA SYMBOL
to_lower(16#03FF)->
    16#037D;
%% CYRILLIC CAPITAL LETTER IE WITH GRAVE
to_lower(16#0400)->
    16#0450;
%% CYRILLIC CAPITAL LETTER IO
to_lower(16#0401)->
    16#0451;
%% CYRILLIC CAPITAL LETTER DJE
to_lower(16#0402)->
    16#0452;
%% CYRILLIC CAPITAL LETTER GJE
to_lower(16#0403)->
    16#0453;
%% CYRILLIC CAPITAL LETTER UKRAINIAN IE
to_lower(16#0404)->
    16#0454;
%% CYRILLIC CAPITAL LETTER DZE
to_lower(16#0405)->
    16#0455;
%% CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
to_lower(16#0406)->
    16#0456;
%% CYRILLIC CAPITAL LETTER YI
to_lower(16#0407)->
    16#0457;
%% CYRILLIC CAPITAL LETTER JE
to_lower(16#0408)->
    16#0458;
%% CYRILLIC CAPITAL LETTER LJE
to_lower(16#0409)->
    16#0459;
%% CYRILLIC CAPITAL LETTER NJE
to_lower(16#040A)->
    16#045A;
%% CYRILLIC CAPITAL LETTER TSHE
to_lower(16#040B)->
    16#045B;
%% CYRILLIC CAPITAL LETTER KJE
to_lower(16#040C)->
    16#045C;
%% CYRILLIC CAPITAL LETTER I WITH GRAVE
to_lower(16#040D)->
    16#045D;
%% CYRILLIC CAPITAL LETTER SHORT U
to_lower(16#040E)->
    16#045E;
%% CYRILLIC CAPITAL LETTER DZHE
to_lower(16#040F)->
    16#045F;
%% CYRILLIC CAPITAL LETTER A
to_lower(16#0410)->
    16#0430;
%% CYRILLIC CAPITAL LETTER BE
to_lower(16#0411)->
    16#0431;
%% CYRILLIC CAPITAL LETTER VE
to_lower(16#0412)->
    16#0432;
%% CYRILLIC CAPITAL LETTER GHE
to_lower(16#0413)->
    16#0433;
%% CYRILLIC CAPITAL LETTER DE
to_lower(16#0414)->
    16#0434;
%% CYRILLIC CAPITAL LETTER IE
to_lower(16#0415)->
    16#0435;
%% CYRILLIC CAPITAL LETTER ZHE
to_lower(16#0416)->
    16#0436;
%% CYRILLIC CAPITAL LETTER ZE
to_lower(16#0417)->
    16#0437;
%% CYRILLIC CAPITAL LETTER I
to_lower(16#0418)->
    16#0438;
%% CYRILLIC CAPITAL LETTER SHORT I
to_lower(16#0419)->
    16#0439;
%% CYRILLIC CAPITAL LETTER KA
to_lower(16#041A)->
    16#043A;
%% CYRILLIC CAPITAL LETTER EL
to_lower(16#041B)->
    16#043B;
%% CYRILLIC CAPITAL LETTER EM
to_lower(16#041C)->
    16#043C;
%% CYRILLIC CAPITAL LETTER EN
to_lower(16#041D)->
    16#043D;
%% CYRILLIC CAPITAL LETTER O
to_lower(16#041E)->
    16#043E;
%% CYRILLIC CAPITAL LETTER PE
to_lower(16#041F)->
    16#043F;
%% CYRILLIC CAPITAL LETTER ER
to_lower(16#0420)->
    16#0440;
%% CYRILLIC CAPITAL LETTER ES
to_lower(16#0421)->
    16#0441;
%% CYRILLIC CAPITAL LETTER TE
to_lower(16#0422)->
    16#0442;
%% CYRILLIC CAPITAL LETTER U
to_lower(16#0423)->
    16#0443;
%% CYRILLIC CAPITAL LETTER EF
to_lower(16#0424)->
    16#0444;
%% CYRILLIC CAPITAL LETTER HA
to_lower(16#0425)->
    16#0445;
%% CYRILLIC CAPITAL LETTER TSE
to_lower(16#0426)->
    16#0446;
%% CYRILLIC CAPITAL LETTER CHE
to_lower(16#0427)->
    16#0447;
%% CYRILLIC CAPITAL LETTER SHA
to_lower(16#0428)->
    16#0448;
%% CYRILLIC CAPITAL LETTER SHCHA
to_lower(16#0429)->
    16#0449;
%% CYRILLIC CAPITAL LETTER HARD SIGN
to_lower(16#042A)->
    16#044A;
%% CYRILLIC CAPITAL LETTER YERU
to_lower(16#042B)->
    16#044B;
%% CYRILLIC CAPITAL LETTER SOFT SIGN
to_lower(16#042C)->
    16#044C;
%% CYRILLIC CAPITAL LETTER E
to_lower(16#042D)->
    16#044D;
%% CYRILLIC CAPITAL LETTER YU
to_lower(16#042E)->
    16#044E;
%% CYRILLIC CAPITAL LETTER YA
to_lower(16#042F)->
    16#044F;
%% CYRILLIC CAPITAL LETTER OMEGA
to_lower(16#0460)->
    16#0461;
%% CYRILLIC CAPITAL LETTER YAT
to_lower(16#0462)->
    16#0463;
%% CYRILLIC CAPITAL LETTER IOTIFIED E
to_lower(16#0464)->
    16#0465;
%% CYRILLIC CAPITAL LETTER LITTLE YUS
to_lower(16#0466)->
    16#0467;
%% CYRILLIC CAPITAL LETTER IOTIFIED LITTLE YUS
to_lower(16#0468)->
    16#0469;
%% CYRILLIC CAPITAL LETTER BIG YUS
to_lower(16#046A)->
    16#046B;
%% CYRILLIC CAPITAL LETTER IOTIFIED BIG YUS
to_lower(16#046C)->
    16#046D;
%% CYRILLIC CAPITAL LETTER KSI
to_lower(16#046E)->
    16#046F;
%% CYRILLIC CAPITAL LETTER PSI
to_lower(16#0470)->
    16#0471;
%% CYRILLIC CAPITAL LETTER FITA
to_lower(16#0472)->
    16#0473;
%% CYRILLIC CAPITAL LETTER IZHITSA
to_lower(16#0474)->
    16#0475;
%% CYRILLIC CAPITAL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT
to_lower(16#0476)->
    16#0477;
%% CYRILLIC CAPITAL LETTER UK
to_lower(16#0478)->
    16#0479;
%% CYRILLIC CAPITAL LETTER ROUND OMEGA
to_lower(16#047A)->
    16#047B;
%% CYRILLIC CAPITAL LETTER OMEGA WITH TITLO
to_lower(16#047C)->
    16#047D;
%% CYRILLIC CAPITAL LETTER OT
to_lower(16#047E)->
    16#047F;
%% CYRILLIC CAPITAL LETTER KOPPA
to_lower(16#0480)->
    16#0481;
%% CYRILLIC CAPITAL LETTER SHORT I WITH TAIL
to_lower(16#048A)->
    16#048B;
%% CYRILLIC CAPITAL LETTER SEMISOFT SIGN
to_lower(16#048C)->
    16#048D;
%% CYRILLIC CAPITAL LETTER ER WITH TICK
to_lower(16#048E)->
    16#048F;
%% CYRILLIC CAPITAL LETTER GHE WITH UPTURN
to_lower(16#0490)->
    16#0491;
%% CYRILLIC CAPITAL LETTER GHE WITH STROKE
to_lower(16#0492)->
    16#0493;
%% CYRILLIC CAPITAL LETTER GHE WITH MIDDLE HOOK
to_lower(16#0494)->
    16#0495;
%% CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER
to_lower(16#0496)->
    16#0497;
%% CYRILLIC CAPITAL LETTER ZE WITH DESCENDER
to_lower(16#0498)->
    16#0499;
%% CYRILLIC CAPITAL LETTER KA WITH DESCENDER
to_lower(16#049A)->
    16#049B;
%% CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE
to_lower(16#049C)->
    16#049D;
%% CYRILLIC CAPITAL LETTER KA WITH STROKE
to_lower(16#049E)->
    16#049F;
%% CYRILLIC CAPITAL LETTER BASHKIR KA
to_lower(16#04A0)->
    16#04A1;
%% CYRILLIC CAPITAL LETTER EN WITH DESCENDER
to_lower(16#04A2)->
    16#04A3;
%% CYRILLIC CAPITAL LIGATURE EN GHE
to_lower(16#04A4)->
    16#04A5;
%% CYRILLIC CAPITAL LETTER PE WITH MIDDLE HOOK
to_lower(16#04A6)->
    16#04A7;
%% CYRILLIC CAPITAL LETTER ABKHASIAN HA
to_lower(16#04A8)->
    16#04A9;
%% CYRILLIC CAPITAL LETTER ES WITH DESCENDER
to_lower(16#04AA)->
    16#04AB;
%% CYRILLIC CAPITAL LETTER TE WITH DESCENDER
to_lower(16#04AC)->
    16#04AD;
%% CYRILLIC CAPITAL LETTER STRAIGHT U
to_lower(16#04AE)->
    16#04AF;
%% CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE
to_lower(16#04B0)->
    16#04B1;
%% CYRILLIC CAPITAL LETTER HA WITH DESCENDER
to_lower(16#04B2)->
    16#04B3;
%% CYRILLIC CAPITAL LIGATURE TE TSE
to_lower(16#04B4)->
    16#04B5;
%% CYRILLIC CAPITAL LETTER CHE WITH DESCENDER
to_lower(16#04B6)->
    16#04B7;
%% CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE
to_lower(16#04B8)->
    16#04B9;
%% CYRILLIC CAPITAL LETTER SHHA
to_lower(16#04BA)->
    16#04BB;
%% CYRILLIC CAPITAL LETTER ABKHASIAN CHE
to_lower(16#04BC)->
    16#04BD;
%% CYRILLIC CAPITAL LETTER ABKHASIAN CHE WITH DESCENDER
to_lower(16#04BE)->
    16#04BF;
%% CYRILLIC LETTER PALOCHKA
to_lower(16#04C0)->
    16#04CF;
%% CYRILLIC CAPITAL LETTER ZHE WITH BREVE
to_lower(16#04C1)->
    16#04C2;
%% CYRILLIC CAPITAL LETTER KA WITH HOOK
to_lower(16#04C3)->
    16#04C4;
%% CYRILLIC CAPITAL LETTER EL WITH TAIL
to_lower(16#04C5)->
    16#04C6;
%% CYRILLIC CAPITAL LETTER EN WITH HOOK
to_lower(16#04C7)->
    16#04C8;
%% CYRILLIC CAPITAL LETTER EN WITH TAIL
to_lower(16#04C9)->
    16#04CA;
%% CYRILLIC CAPITAL LETTER KHAKASSIAN CHE
to_lower(16#04CB)->
    16#04CC;
%% CYRILLIC CAPITAL LETTER EM WITH TAIL
to_lower(16#04CD)->
    16#04CE;
%% CYRILLIC CAPITAL LETTER A WITH BREVE
to_lower(16#04D0)->
    16#04D1;
%% CYRILLIC CAPITAL LETTER A WITH DIAERESIS
to_lower(16#04D2)->
    16#04D3;
%% CYRILLIC CAPITAL LIGATURE A IE
to_lower(16#04D4)->
    16#04D5;
%% CYRILLIC CAPITAL LETTER IE WITH BREVE
to_lower(16#04D6)->
    16#04D7;
%% CYRILLIC CAPITAL LETTER SCHWA
to_lower(16#04D8)->
    16#04D9;
%% CYRILLIC CAPITAL LETTER SCHWA WITH DIAERESIS
to_lower(16#04DA)->
    16#04DB;
%% CYRILLIC CAPITAL LETTER ZHE WITH DIAERESIS
to_lower(16#04DC)->
    16#04DD;
%% CYRILLIC CAPITAL LETTER ZE WITH DIAERESIS
to_lower(16#04DE)->
    16#04DF;
%% CYRILLIC CAPITAL LETTER ABKHASIAN DZE
to_lower(16#04E0)->
    16#04E1;
%% CYRILLIC CAPITAL LETTER I WITH MACRON
to_lower(16#04E2)->
    16#04E3;
%% CYRILLIC CAPITAL LETTER I WITH DIAERESIS
to_lower(16#04E4)->
    16#04E5;
%% CYRILLIC CAPITAL LETTER O WITH DIAERESIS
to_lower(16#04E6)->
    16#04E7;
%% CYRILLIC CAPITAL LETTER BARRED O
to_lower(16#04E8)->
    16#04E9;
%% CYRILLIC CAPITAL LETTER BARRED O WITH DIAERESIS
to_lower(16#04EA)->
    16#04EB;
%% CYRILLIC CAPITAL LETTER E WITH DIAERESIS
to_lower(16#04EC)->
    16#04ED;
%% CYRILLIC CAPITAL LETTER U WITH MACRON
to_lower(16#04EE)->
    16#04EF;
%% CYRILLIC CAPITAL LETTER U WITH DIAERESIS
to_lower(16#04F0)->
    16#04F1;
%% CYRILLIC CAPITAL LETTER U WITH DOUBLE ACUTE
to_lower(16#04F2)->
    16#04F3;
%% CYRILLIC CAPITAL LETTER CHE WITH DIAERESIS
to_lower(16#04F4)->
    16#04F5;
%% CYRILLIC CAPITAL LETTER GHE WITH DESCENDER
to_lower(16#04F6)->
    16#04F7;
%% CYRILLIC CAPITAL LETTER YERU WITH DIAERESIS
to_lower(16#04F8)->
    16#04F9;
%% CYRILLIC CAPITAL LETTER GHE WITH STROKE AND HOOK
to_lower(16#04FA)->
    16#04FB;
%% CYRILLIC CAPITAL LETTER HA WITH HOOK
to_lower(16#04FC)->
    16#04FD;
%% CYRILLIC CAPITAL LETTER HA WITH STROKE
to_lower(16#04FE)->
    16#04FF;
%% CYRILLIC CAPITAL LETTER KOMI DE
to_lower(16#0500)->
    16#0501;
%% CYRILLIC CAPITAL LETTER KOMI DJE
to_lower(16#0502)->
    16#0503;
%% CYRILLIC CAPITAL LETTER KOMI ZJE
to_lower(16#0504)->
    16#0505;
%% CYRILLIC CAPITAL LETTER KOMI DZJE
to_lower(16#0506)->
    16#0507;
%% CYRILLIC CAPITAL LETTER KOMI LJE
to_lower(16#0508)->
    16#0509;
%% CYRILLIC CAPITAL LETTER KOMI NJE
to_lower(16#050A)->
    16#050B;
%% CYRILLIC CAPITAL LETTER KOMI SJE
to_lower(16#050C)->
    16#050D;
%% CYRILLIC CAPITAL LETTER KOMI TJE
to_lower(16#050E)->
    16#050F;
%% CYRILLIC CAPITAL LETTER REVERSED ZE
to_lower(16#0510)->
    16#0511;
%% CYRILLIC CAPITAL LETTER EL WITH HOOK
to_lower(16#0512)->
    16#0513;
%% CYRILLIC CAPITAL LETTER LHA
to_lower(16#0514)->
    16#0515;
%% CYRILLIC CAPITAL LETTER RHA
to_lower(16#0516)->
    16#0517;
%% CYRILLIC CAPITAL LETTER YAE
to_lower(16#0518)->
    16#0519;
%% CYRILLIC CAPITAL LETTER QA
to_lower(16#051A)->
    16#051B;
%% CYRILLIC CAPITAL LETTER WE
to_lower(16#051C)->
    16#051D;
%% CYRILLIC CAPITAL LETTER ALEUT KA
to_lower(16#051E)->
    16#051F;
%% CYRILLIC CAPITAL LETTER EL WITH MIDDLE HOOK
to_lower(16#0520)->
    16#0521;
%% CYRILLIC CAPITAL LETTER EN WITH MIDDLE HOOK
to_lower(16#0522)->
    16#0523;
%% CYRILLIC CAPITAL LETTER PE WITH DESCENDER
to_lower(16#0524)->
    16#0525;
%% CYRILLIC CAPITAL LETTER SHHA WITH DESCENDER
to_lower(16#0526)->
    16#0527;
%% ARMENIAN CAPITAL LETTER AYB
to_lower(16#0531)->
    16#0561;
%% ARMENIAN CAPITAL LETTER BEN
to_lower(16#0532)->
    16#0562;
%% ARMENIAN CAPITAL LETTER GIM
to_lower(16#0533)->
    16#0563;
%% ARMENIAN CAPITAL LETTER DA
to_lower(16#0534)->
    16#0564;
%% ARMENIAN CAPITAL LETTER ECH
to_lower(16#0535)->
    16#0565;
%% ARMENIAN CAPITAL LETTER ZA
to_lower(16#0536)->
    16#0566;
%% ARMENIAN CAPITAL LETTER EH
to_lower(16#0537)->
    16#0567;
%% ARMENIAN CAPITAL LETTER ET
to_lower(16#0538)->
    16#0568;
%% ARMENIAN CAPITAL LETTER TO
to_lower(16#0539)->
    16#0569;
%% ARMENIAN CAPITAL LETTER ZHE
to_lower(16#053A)->
    16#056A;
%% ARMENIAN CAPITAL LETTER INI
to_lower(16#053B)->
    16#056B;
%% ARMENIAN CAPITAL LETTER LIWN
to_lower(16#053C)->
    16#056C;
%% ARMENIAN CAPITAL LETTER XEH
to_lower(16#053D)->
    16#056D;
%% ARMENIAN CAPITAL LETTER CA
to_lower(16#053E)->
    16#056E;
%% ARMENIAN CAPITAL LETTER KEN
to_lower(16#053F)->
    16#056F;
%% ARMENIAN CAPITAL LETTER HO
to_lower(16#0540)->
    16#0570;
%% ARMENIAN CAPITAL LETTER JA
to_lower(16#0541)->
    16#0571;
%% ARMENIAN CAPITAL LETTER GHAD
to_lower(16#0542)->
    16#0572;
%% ARMENIAN CAPITAL LETTER CHEH
to_lower(16#0543)->
    16#0573;
%% ARMENIAN CAPITAL LETTER MEN
to_lower(16#0544)->
    16#0574;
%% ARMENIAN CAPITAL LETTER YI
to_lower(16#0545)->
    16#0575;
%% ARMENIAN CAPITAL LETTER NOW
to_lower(16#0546)->
    16#0576;
%% ARMENIAN CAPITAL LETTER SHA
to_lower(16#0547)->
    16#0577;
%% ARMENIAN CAPITAL LETTER VO
to_lower(16#0548)->
    16#0578;
%% ARMENIAN CAPITAL LETTER CHA
to_lower(16#0549)->
    16#0579;
%% ARMENIAN CAPITAL LETTER PEH
to_lower(16#054A)->
    16#057A;
%% ARMENIAN CAPITAL LETTER JHEH
to_lower(16#054B)->
    16#057B;
%% ARMENIAN CAPITAL LETTER RA
to_lower(16#054C)->
    16#057C;
%% ARMENIAN CAPITAL LETTER SEH
to_lower(16#054D)->
    16#057D;
%% ARMENIAN CAPITAL LETTER VEW
to_lower(16#054E)->
    16#057E;
%% ARMENIAN CAPITAL LETTER TIWN
to_lower(16#054F)->
    16#057F;
%% ARMENIAN CAPITAL LETTER REH
to_lower(16#0550)->
    16#0580;
%% ARMENIAN CAPITAL LETTER CO
to_lower(16#0551)->
    16#0581;
%% ARMENIAN CAPITAL LETTER YIWN
to_lower(16#0552)->
    16#0582;
%% ARMENIAN CAPITAL LETTER PIWR
to_lower(16#0553)->
    16#0583;
%% ARMENIAN CAPITAL LETTER KEH
to_lower(16#0554)->
    16#0584;
%% ARMENIAN CAPITAL LETTER OH
to_lower(16#0555)->
    16#0585;
%% ARMENIAN CAPITAL LETTER FEH
to_lower(16#0556)->
    16#0586;
%% GEORGIAN CAPITAL LETTER AN
to_lower(16#10A0)->
    16#2D00;
%% GEORGIAN CAPITAL LETTER BAN
to_lower(16#10A1)->
    16#2D01;
%% GEORGIAN CAPITAL LETTER GAN
to_lower(16#10A2)->
    16#2D02;
%% GEORGIAN CAPITAL LETTER DON
to_lower(16#10A3)->
    16#2D03;
%% GEORGIAN CAPITAL LETTER EN
to_lower(16#10A4)->
    16#2D04;
%% GEORGIAN CAPITAL LETTER VIN
to_lower(16#10A5)->
    16#2D05;
%% GEORGIAN CAPITAL LETTER ZEN
to_lower(16#10A6)->
    16#2D06;
%% GEORGIAN CAPITAL LETTER TAN
to_lower(16#10A7)->
    16#2D07;
%% GEORGIAN CAPITAL LETTER IN
to_lower(16#10A8)->
    16#2D08;
%% GEORGIAN CAPITAL LETTER KAN
to_lower(16#10A9)->
    16#2D09;
%% GEORGIAN CAPITAL LETTER LAS
to_lower(16#10AA)->
    16#2D0A;
%% GEORGIAN CAPITAL LETTER MAN
to_lower(16#10AB)->
    16#2D0B;
%% GEORGIAN CAPITAL LETTER NAR
to_lower(16#10AC)->
    16#2D0C;
%% GEORGIAN CAPITAL LETTER ON
to_lower(16#10AD)->
    16#2D0D;
%% GEORGIAN CAPITAL LETTER PAR
to_lower(16#10AE)->
    16#2D0E;
%% GEORGIAN CAPITAL LETTER ZHAR
to_lower(16#10AF)->
    16#2D0F;
%% GEORGIAN CAPITAL LETTER RAE
to_lower(16#10B0)->
    16#2D10;
%% GEORGIAN CAPITAL LETTER SAN
to_lower(16#10B1)->
    16#2D11;
%% GEORGIAN CAPITAL LETTER TAR
to_lower(16#10B2)->
    16#2D12;
%% GEORGIAN CAPITAL LETTER UN
to_lower(16#10B3)->
    16#2D13;
%% GEORGIAN CAPITAL LETTER PHAR
to_lower(16#10B4)->
    16#2D14;
%% GEORGIAN CAPITAL LETTER KHAR
to_lower(16#10B5)->
    16#2D15;
%% GEORGIAN CAPITAL LETTER GHAN
to_lower(16#10B6)->
    16#2D16;
%% GEORGIAN CAPITAL LETTER QAR
to_lower(16#10B7)->
    16#2D17;
%% GEORGIAN CAPITAL LETTER SHIN
to_lower(16#10B8)->
    16#2D18;
%% GEORGIAN CAPITAL LETTER CHIN
to_lower(16#10B9)->
    16#2D19;
%% GEORGIAN CAPITAL LETTER CAN
to_lower(16#10BA)->
    16#2D1A;
%% GEORGIAN CAPITAL LETTER JIL
to_lower(16#10BB)->
    16#2D1B;
%% GEORGIAN CAPITAL LETTER CIL
to_lower(16#10BC)->
    16#2D1C;
%% GEORGIAN CAPITAL LETTER CHAR
to_lower(16#10BD)->
    16#2D1D;
%% GEORGIAN CAPITAL LETTER XAN
to_lower(16#10BE)->
    16#2D1E;
%% GEORGIAN CAPITAL LETTER JHAN
to_lower(16#10BF)->
    16#2D1F;
%% GEORGIAN CAPITAL LETTER HAE
to_lower(16#10C0)->
    16#2D20;
%% GEORGIAN CAPITAL LETTER HE
to_lower(16#10C1)->
    16#2D21;
%% GEORGIAN CAPITAL LETTER HIE
to_lower(16#10C2)->
    16#2D22;
%% GEORGIAN CAPITAL LETTER WE
to_lower(16#10C3)->
    16#2D23;
%% GEORGIAN CAPITAL LETTER HAR
to_lower(16#10C4)->
    16#2D24;
%% GEORGIAN CAPITAL LETTER HOE
to_lower(16#10C5)->
    16#2D25;
%% GEORGIAN CAPITAL LETTER YN
to_lower(16#10C7)->
    16#2D27;
%% GEORGIAN CAPITAL LETTER AEN
to_lower(16#10CD)->
    16#2D2D;
%% LATIN CAPITAL LETTER A WITH RING BELOW
to_lower(16#1E00)->
    16#1E01;
%% LATIN CAPITAL LETTER B WITH DOT ABOVE
to_lower(16#1E02)->
    16#1E03;
%% LATIN CAPITAL LETTER B WITH DOT BELOW
to_lower(16#1E04)->
    16#1E05;
%% LATIN CAPITAL LETTER B WITH LINE BELOW
to_lower(16#1E06)->
    16#1E07;
%% LATIN CAPITAL LETTER C WITH CEDILLA AND ACUTE
to_lower(16#1E08)->
    16#1E09;
%% LATIN CAPITAL LETTER D WITH DOT ABOVE
to_lower(16#1E0A)->
    16#1E0B;
%% LATIN CAPITAL LETTER D WITH DOT BELOW
to_lower(16#1E0C)->
    16#1E0D;
%% LATIN CAPITAL LETTER D WITH LINE BELOW
to_lower(16#1E0E)->
    16#1E0F;
%% LATIN CAPITAL LETTER D WITH CEDILLA
to_lower(16#1E10)->
    16#1E11;
%% LATIN CAPITAL LETTER D WITH CIRCUMFLEX BELOW
to_lower(16#1E12)->
    16#1E13;
%% LATIN CAPITAL LETTER E WITH MACRON AND GRAVE
to_lower(16#1E14)->
    16#1E15;
%% LATIN CAPITAL LETTER E WITH MACRON AND ACUTE
to_lower(16#1E16)->
    16#1E17;
%% LATIN CAPITAL LETTER E WITH CIRCUMFLEX BELOW
to_lower(16#1E18)->
    16#1E19;
%% LATIN CAPITAL LETTER E WITH TILDE BELOW
to_lower(16#1E1A)->
    16#1E1B;
%% LATIN CAPITAL LETTER E WITH CEDILLA AND BREVE
to_lower(16#1E1C)->
    16#1E1D;
%% LATIN CAPITAL LETTER F WITH DOT ABOVE
to_lower(16#1E1E)->
    16#1E1F;
%% LATIN CAPITAL LETTER G WITH MACRON
to_lower(16#1E20)->
    16#1E21;
%% LATIN CAPITAL LETTER H WITH DOT ABOVE
to_lower(16#1E22)->
    16#1E23;
%% LATIN CAPITAL LETTER H WITH DOT BELOW
to_lower(16#1E24)->
    16#1E25;
%% LATIN CAPITAL LETTER H WITH DIAERESIS
to_lower(16#1E26)->
    16#1E27;
%% LATIN CAPITAL LETTER H WITH CEDILLA
to_lower(16#1E28)->
    16#1E29;
%% LATIN CAPITAL LETTER H WITH BREVE BELOW
to_lower(16#1E2A)->
    16#1E2B;
%% LATIN CAPITAL LETTER I WITH TILDE BELOW
to_lower(16#1E2C)->
    16#1E2D;
%% LATIN CAPITAL LETTER I WITH DIAERESIS AND ACUTE
to_lower(16#1E2E)->
    16#1E2F;
%% LATIN CAPITAL LETTER K WITH ACUTE
to_lower(16#1E30)->
    16#1E31;
%% LATIN CAPITAL LETTER K WITH DOT BELOW
to_lower(16#1E32)->
    16#1E33;
%% LATIN CAPITAL LETTER K WITH LINE BELOW
to_lower(16#1E34)->
    16#1E35;
%% LATIN CAPITAL LETTER L WITH DOT BELOW
to_lower(16#1E36)->
    16#1E37;
%% LATIN CAPITAL LETTER L WITH DOT BELOW AND MACRON
to_lower(16#1E38)->
    16#1E39;
%% LATIN CAPITAL LETTER L WITH LINE BELOW
to_lower(16#1E3A)->
    16#1E3B;
%% LATIN CAPITAL LETTER L WITH CIRCUMFLEX BELOW
to_lower(16#1E3C)->
    16#1E3D;
%% LATIN CAPITAL LETTER M WITH ACUTE
to_lower(16#1E3E)->
    16#1E3F;
%% LATIN CAPITAL LETTER M WITH DOT ABOVE
to_lower(16#1E40)->
    16#1E41;
%% LATIN CAPITAL LETTER M WITH DOT BELOW
to_lower(16#1E42)->
    16#1E43;
%% LATIN CAPITAL LETTER N WITH DOT ABOVE
to_lower(16#1E44)->
    16#1E45;
%% LATIN CAPITAL LETTER N WITH DOT BELOW
to_lower(16#1E46)->
    16#1E47;
%% LATIN CAPITAL LETTER N WITH LINE BELOW
to_lower(16#1E48)->
    16#1E49;
%% LATIN CAPITAL LETTER N WITH CIRCUMFLEX BELOW
to_lower(16#1E4A)->
    16#1E4B;
%% LATIN CAPITAL LETTER O WITH TILDE AND ACUTE
to_lower(16#1E4C)->
    16#1E4D;
%% LATIN CAPITAL LETTER O WITH TILDE AND DIAERESIS
to_lower(16#1E4E)->
    16#1E4F;
%% LATIN CAPITAL LETTER O WITH MACRON AND GRAVE
to_lower(16#1E50)->
    16#1E51;
%% LATIN CAPITAL LETTER O WITH MACRON AND ACUTE
to_lower(16#1E52)->
    16#1E53;
%% LATIN CAPITAL LETTER P WITH ACUTE
to_lower(16#1E54)->
    16#1E55;
%% LATIN CAPITAL LETTER P WITH DOT ABOVE
to_lower(16#1E56)->
    16#1E57;
%% LATIN CAPITAL LETTER R WITH DOT ABOVE
to_lower(16#1E58)->
    16#1E59;
%% LATIN CAPITAL LETTER R WITH DOT BELOW
to_lower(16#1E5A)->
    16#1E5B;
%% LATIN CAPITAL LETTER R WITH DOT BELOW AND MACRON
to_lower(16#1E5C)->
    16#1E5D;
%% LATIN CAPITAL LETTER R WITH LINE BELOW
to_lower(16#1E5E)->
    16#1E5F;
%% LATIN CAPITAL LETTER S WITH DOT ABOVE
to_lower(16#1E60)->
    16#1E61;
%% LATIN CAPITAL LETTER S WITH DOT BELOW
to_lower(16#1E62)->
    16#1E63;
%% LATIN CAPITAL LETTER S WITH ACUTE AND DOT ABOVE
to_lower(16#1E64)->
    16#1E65;
%% LATIN CAPITAL LETTER S WITH CARON AND DOT ABOVE
to_lower(16#1E66)->
    16#1E67;
%% LATIN CAPITAL LETTER S WITH DOT BELOW AND DOT ABOVE
to_lower(16#1E68)->
    16#1E69;
%% LATIN CAPITAL LETTER T WITH DOT ABOVE
to_lower(16#1E6A)->
    16#1E6B;
%% LATIN CAPITAL LETTER T WITH DOT BELOW
to_lower(16#1E6C)->
    16#1E6D;
%% LATIN CAPITAL LETTER T WITH LINE BELOW
to_lower(16#1E6E)->
    16#1E6F;
%% LATIN CAPITAL LETTER T WITH CIRCUMFLEX BELOW
to_lower(16#1E70)->
    16#1E71;
%% LATIN CAPITAL LETTER U WITH DIAERESIS BELOW
to_lower(16#1E72)->
    16#1E73;
%% LATIN CAPITAL LETTER U WITH TILDE BELOW
to_lower(16#1E74)->
    16#1E75;
%% LATIN CAPITAL LETTER U WITH CIRCUMFLEX BELOW
to_lower(16#1E76)->
    16#1E77;
%% LATIN CAPITAL LETTER U WITH TILDE AND ACUTE
to_lower(16#1E78)->
    16#1E79;
%% LATIN CAPITAL LETTER U WITH MACRON AND DIAERESIS
to_lower(16#1E7A)->
    16#1E7B;
%% LATIN CAPITAL LETTER V WITH TILDE
to_lower(16#1E7C)->
    16#1E7D;
%% LATIN CAPITAL LETTER V WITH DOT BELOW
to_lower(16#1E7E)->
    16#1E7F;
%% LATIN CAPITAL LETTER W WITH GRAVE
to_lower(16#1E80)->
    16#1E81;
%% LATIN CAPITAL LETTER W WITH ACUTE
to_lower(16#1E82)->
    16#1E83;
%% LATIN CAPITAL LETTER W WITH DIAERESIS
to_lower(16#1E84)->
    16#1E85;
%% LATIN CAPITAL LETTER W WITH DOT ABOVE
to_lower(16#1E86)->
    16#1E87;
%% LATIN CAPITAL LETTER W WITH DOT BELOW
to_lower(16#1E88)->
    16#1E89;
%% LATIN CAPITAL LETTER X WITH DOT ABOVE
to_lower(16#1E8A)->
    16#1E8B;
%% LATIN CAPITAL LETTER X WITH DIAERESIS
to_lower(16#1E8C)->
    16#1E8D;
%% LATIN CAPITAL LETTER Y WITH DOT ABOVE
to_lower(16#1E8E)->
    16#1E8F;
%% LATIN CAPITAL LETTER Z WITH CIRCUMFLEX
to_lower(16#1E90)->
    16#1E91;
%% LATIN CAPITAL LETTER Z WITH DOT BELOW
to_lower(16#1E92)->
    16#1E93;
%% LATIN CAPITAL LETTER Z WITH LINE BELOW
to_lower(16#1E94)->
    16#1E95;
%% LATIN CAPITAL LETTER SHARP S
to_lower(16#1E9E)->
    16#00DF;
%% LATIN CAPITAL LETTER A WITH DOT BELOW
to_lower(16#1EA0)->
    16#1EA1;
%% LATIN CAPITAL LETTER A WITH HOOK ABOVE
to_lower(16#1EA2)->
    16#1EA3;
%% LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE
to_lower(16#1EA4)->
    16#1EA5;
%% LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE
to_lower(16#1EA6)->
    16#1EA7;
%% LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
to_lower(16#1EA8)->
    16#1EA9;
%% LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE
to_lower(16#1EAA)->
    16#1EAB;
%% LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW
to_lower(16#1EAC)->
    16#1EAD;
%% LATIN CAPITAL LETTER A WITH BREVE AND ACUTE
to_lower(16#1EAE)->
    16#1EAF;
%% LATIN CAPITAL LETTER A WITH BREVE AND GRAVE
to_lower(16#1EB0)->
    16#1EB1;
%% LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE
to_lower(16#1EB2)->
    16#1EB3;
%% LATIN CAPITAL LETTER A WITH BREVE AND TILDE
to_lower(16#1EB4)->
    16#1EB5;
%% LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW
to_lower(16#1EB6)->
    16#1EB7;
%% LATIN CAPITAL LETTER E WITH DOT BELOW
to_lower(16#1EB8)->
    16#1EB9;
%% LATIN CAPITAL LETTER E WITH HOOK ABOVE
to_lower(16#1EBA)->
    16#1EBB;
%% LATIN CAPITAL LETTER E WITH TILDE
to_lower(16#1EBC)->
    16#1EBD;
%% LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE
to_lower(16#1EBE)->
    16#1EBF;
%% LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE
to_lower(16#1EC0)->
    16#1EC1;
%% LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
to_lower(16#1EC2)->
    16#1EC3;
%% LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE
to_lower(16#1EC4)->
    16#1EC5;
%% LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW
to_lower(16#1EC6)->
    16#1EC7;
%% LATIN CAPITAL LETTER I WITH HOOK ABOVE
to_lower(16#1EC8)->
    16#1EC9;
%% LATIN CAPITAL LETTER I WITH DOT BELOW
to_lower(16#1ECA)->
    16#1ECB;
%% LATIN CAPITAL LETTER O WITH DOT BELOW
to_lower(16#1ECC)->
    16#1ECD;
%% LATIN CAPITAL LETTER O WITH HOOK ABOVE
to_lower(16#1ECE)->
    16#1ECF;
%% LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE
to_lower(16#1ED0)->
    16#1ED1;
%% LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE
to_lower(16#1ED2)->
    16#1ED3;
%% LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
to_lower(16#1ED4)->
    16#1ED5;
%% LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE
to_lower(16#1ED6)->
    16#1ED7;
%% LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW
to_lower(16#1ED8)->
    16#1ED9;
%% LATIN CAPITAL LETTER O WITH HORN AND ACUTE
to_lower(16#1EDA)->
    16#1EDB;
%% LATIN CAPITAL LETTER O WITH HORN AND GRAVE
to_lower(16#1EDC)->
    16#1EDD;
%% LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE
to_lower(16#1EDE)->
    16#1EDF;
%% LATIN CAPITAL LETTER O WITH HORN AND TILDE
to_lower(16#1EE0)->
    16#1EE1;
%% LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW
to_lower(16#1EE2)->
    16#1EE3;
%% LATIN CAPITAL LETTER U WITH DOT BELOW
to_lower(16#1EE4)->
    16#1EE5;
%% LATIN CAPITAL LETTER U WITH HOOK ABOVE
to_lower(16#1EE6)->
    16#1EE7;
%% LATIN CAPITAL LETTER U WITH HORN AND ACUTE
to_lower(16#1EE8)->
    16#1EE9;
%% LATIN CAPITAL LETTER U WITH HORN AND GRAVE
to_lower(16#1EEA)->
    16#1EEB;
%% LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE
to_lower(16#1EEC)->
    16#1EED;
%% LATIN CAPITAL LETTER U WITH HORN AND TILDE
to_lower(16#1EEE)->
    16#1EEF;
%% LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW
to_lower(16#1EF0)->
    16#1EF1;
%% LATIN CAPITAL LETTER Y WITH GRAVE
to_lower(16#1EF2)->
    16#1EF3;
%% LATIN CAPITAL LETTER Y WITH DOT BELOW
to_lower(16#1EF4)->
    16#1EF5;
%% LATIN CAPITAL LETTER Y WITH HOOK ABOVE
to_lower(16#1EF6)->
    16#1EF7;
%% LATIN CAPITAL LETTER Y WITH TILDE
to_lower(16#1EF8)->
    16#1EF9;
%% LATIN CAPITAL LETTER MIDDLE-WELSH LL
to_lower(16#1EFA)->
    16#1EFB;
%% LATIN CAPITAL LETTER MIDDLE-WELSH V
to_lower(16#1EFC)->
    16#1EFD;
%% LATIN CAPITAL LETTER Y WITH LOOP
to_lower(16#1EFE)->
    16#1EFF;
%% GREEK CAPITAL LETTER ALPHA WITH PSILI
to_lower(16#1F08)->
    16#1F00;
%% GREEK CAPITAL LETTER ALPHA WITH DASIA
to_lower(16#1F09)->
    16#1F01;
%% GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA
to_lower(16#1F0A)->
    16#1F02;
%% GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA
to_lower(16#1F0B)->
    16#1F03;
%% GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA
to_lower(16#1F0C)->
    16#1F04;
%% GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA
to_lower(16#1F0D)->
    16#1F05;
%% GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI
to_lower(16#1F0E)->
    16#1F06;
%% GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI
to_lower(16#1F0F)->
    16#1F07;
%% GREEK CAPITAL LETTER EPSILON WITH PSILI
to_lower(16#1F18)->
    16#1F10;
%% GREEK CAPITAL LETTER EPSILON WITH DASIA
to_lower(16#1F19)->
    16#1F11;
%% GREEK CAPITAL LETTER EPSILON WITH PSILI AND VARIA
to_lower(16#1F1A)->
    16#1F12;
%% GREEK CAPITAL LETTER EPSILON WITH DASIA AND VARIA
to_lower(16#1F1B)->
    16#1F13;
%% GREEK CAPITAL LETTER EPSILON WITH PSILI AND OXIA
to_lower(16#1F1C)->
    16#1F14;
%% GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
to_lower(16#1F1D)->
    16#1F15;
%% GREEK CAPITAL LETTER ETA WITH PSILI
to_lower(16#1F28)->
    16#1F20;
%% GREEK CAPITAL LETTER ETA WITH DASIA
to_lower(16#1F29)->
    16#1F21;
%% GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA
to_lower(16#1F2A)->
    16#1F22;
%% GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA
to_lower(16#1F2B)->
    16#1F23;
%% GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA
to_lower(16#1F2C)->
    16#1F24;
%% GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA
to_lower(16#1F2D)->
    16#1F25;
%% GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI
to_lower(16#1F2E)->
    16#1F26;
%% GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI
to_lower(16#1F2F)->
    16#1F27;
%% GREEK CAPITAL LETTER IOTA WITH PSILI
to_lower(16#1F38)->
    16#1F30;
%% GREEK CAPITAL LETTER IOTA WITH DASIA
to_lower(16#1F39)->
    16#1F31;
%% GREEK CAPITAL LETTER IOTA WITH PSILI AND VARIA
to_lower(16#1F3A)->
    16#1F32;
%% GREEK CAPITAL LETTER IOTA WITH DASIA AND VARIA
to_lower(16#1F3B)->
    16#1F33;
%% GREEK CAPITAL LETTER IOTA WITH PSILI AND OXIA
to_lower(16#1F3C)->
    16#1F34;
%% GREEK CAPITAL LETTER IOTA WITH DASIA AND OXIA
to_lower(16#1F3D)->
    16#1F35;
%% GREEK CAPITAL LETTER IOTA WITH PSILI AND PERISPOMENI
to_lower(16#1F3E)->
    16#1F36;
%% GREEK CAPITAL LETTER IOTA WITH DASIA AND PERISPOMENI
to_lower(16#1F3F)->
    16#1F37;
%% GREEK CAPITAL LETTER OMICRON WITH PSILI
to_lower(16#1F48)->
    16#1F40;
%% GREEK CAPITAL LETTER OMICRON WITH DASIA
to_lower(16#1F49)->
    16#1F41;
%% GREEK CAPITAL LETTER OMICRON WITH PSILI AND VARIA
to_lower(16#1F4A)->
    16#1F42;
%% GREEK CAPITAL LETTER OMICRON WITH DASIA AND VARIA
to_lower(16#1F4B)->
    16#1F43;
%% GREEK CAPITAL LETTER OMICRON WITH PSILI AND OXIA
to_lower(16#1F4C)->
    16#1F44;
%% GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
to_lower(16#1F4D)->
    16#1F45;
%% GREEK CAPITAL LETTER UPSILON WITH DASIA
to_lower(16#1F59)->
    16#1F51;
%% GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
to_lower(16#1F5B)->
    16#1F53;
%% GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
to_lower(16#1F5D)->
    16#1F55;
%% GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI
to_lower(16#1F5F)->
    16#1F57;
%% GREEK CAPITAL LETTER OMEGA WITH PSILI
to_lower(16#1F68)->
    16#1F60;
%% GREEK CAPITAL LETTER OMEGA WITH DASIA
to_lower(16#1F69)->
    16#1F61;
%% GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA
to_lower(16#1F6A)->
    16#1F62;
%% GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA
to_lower(16#1F6B)->
    16#1F63;
%% GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA
to_lower(16#1F6C)->
    16#1F64;
%% GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA
to_lower(16#1F6D)->
    16#1F65;
%% GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI
to_lower(16#1F6E)->
    16#1F66;
%% GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI
to_lower(16#1F6F)->
    16#1F67;
%% GREEK CAPITAL LETTER ALPHA WITH PSILI AND PROSGEGRAMMENI
to_lower(16#1F88)->
    16#1F80;
%% GREEK CAPITAL LETTER ALPHA WITH DASIA AND PROSGEGRAMMENI
to_lower(16#1F89)->
    16#1F81;
%% GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA AND PROSGEGRAMMENI
to_lower(16#1F8A)->
    16#1F82;
%% GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA AND PROSGEGRAMMENI
to_lower(16#1F8B)->
    16#1F83;
%% GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA AND PROSGEGRAMMENI
to_lower(16#1F8C)->
    16#1F84;
%% GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA AND PROSGEGRAMMENI
to_lower(16#1F8D)->
    16#1F85;
%% GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
to_lower(16#1F8E)->
    16#1F86;
%% GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
to_lower(16#1F8F)->
    16#1F87;
%% GREEK CAPITAL LETTER ETA WITH PSILI AND PROSGEGRAMMENI
to_lower(16#1F98)->
    16#1F90;
%% GREEK CAPITAL LETTER ETA WITH DASIA AND PROSGEGRAMMENI
to_lower(16#1F99)->
    16#1F91;
%% GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA AND PROSGEGRAMMENI
to_lower(16#1F9A)->
    16#1F92;
%% GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA AND PROSGEGRAMMENI
to_lower(16#1F9B)->
    16#1F93;
%% GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA AND PROSGEGRAMMENI
to_lower(16#1F9C)->
    16#1F94;
%% GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA AND PROSGEGRAMMENI
to_lower(16#1F9D)->
    16#1F95;
%% GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
to_lower(16#1F9E)->
    16#1F96;
%% GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
to_lower(16#1F9F)->
    16#1F97;
%% GREEK CAPITAL LETTER OMEGA WITH PSILI AND PROSGEGRAMMENI
to_lower(16#1FA8)->
    16#1FA0;
%% GREEK CAPITAL LETTER OMEGA WITH DASIA AND PROSGEGRAMMENI
to_lower(16#1FA9)->
    16#1FA1;
%% GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA AND PROSGEGRAMMENI
to_lower(16#1FAA)->
    16#1FA2;
%% GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA AND PROSGEGRAMMENI
to_lower(16#1FAB)->
    16#1FA3;
%% GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA AND PROSGEGRAMMENI
to_lower(16#1FAC)->
    16#1FA4;
%% GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA AND PROSGEGRAMMENI
to_lower(16#1FAD)->
    16#1FA5;
%% GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
to_lower(16#1FAE)->
    16#1FA6;
%% GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
to_lower(16#1FAF)->
    16#1FA7;
%% GREEK CAPITAL LETTER ALPHA WITH VRACHY
to_lower(16#1FB8)->
    16#1FB0;
%% GREEK CAPITAL LETTER ALPHA WITH MACRON
to_lower(16#1FB9)->
    16#1FB1;
%% GREEK CAPITAL LETTER ALPHA WITH VARIA
to_lower(16#1FBA)->
    16#1F70;
%% GREEK CAPITAL LETTER ALPHA WITH OXIA
to_lower(16#1FBB)->
    16#1F71;
%% GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
to_lower(16#1FBC)->
    16#1FB3;
%% GREEK CAPITAL LETTER EPSILON WITH VARIA
to_lower(16#1FC8)->
    16#1F72;
%% GREEK CAPITAL LETTER EPSILON WITH OXIA
to_lower(16#1FC9)->
    16#1F73;
%% GREEK CAPITAL LETTER ETA WITH VARIA
to_lower(16#1FCA)->
    16#1F74;
%% GREEK CAPITAL LETTER ETA WITH OXIA
to_lower(16#1FCB)->
    16#1F75;
%% GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
to_lower(16#1FCC)->
    16#1FC3;
%% GREEK CAPITAL LETTER IOTA WITH VRACHY
to_lower(16#1FD8)->
    16#1FD0;
%% GREEK CAPITAL LETTER IOTA WITH MACRON
to_lower(16#1FD9)->
    16#1FD1;
%% GREEK CAPITAL LETTER IOTA WITH VARIA
to_lower(16#1FDA)->
    16#1F76;
%% GREEK CAPITAL LETTER IOTA WITH OXIA
to_lower(16#1FDB)->
    16#1F77;
%% GREEK CAPITAL LETTER UPSILON WITH VRACHY
to_lower(16#1FE8)->
    16#1FE0;
%% GREEK CAPITAL LETTER UPSILON WITH MACRON
to_lower(16#1FE9)->
    16#1FE1;
%% GREEK CAPITAL LETTER UPSILON WITH VARIA
to_lower(16#1FEA)->
    16#1F7A;
%% GREEK CAPITAL LETTER UPSILON WITH OXIA
to_lower(16#1FEB)->
    16#1F7B;
%% GREEK CAPITAL LETTER RHO WITH DASIA
to_lower(16#1FEC)->
    16#1FE5;
%% GREEK CAPITAL LETTER OMICRON WITH VARIA
to_lower(16#1FF8)->
    16#1F78;
%% GREEK CAPITAL LETTER OMICRON WITH OXIA
to_lower(16#1FF9)->
    16#1F79;
%% GREEK CAPITAL LETTER OMEGA WITH VARIA
to_lower(16#1FFA)->
    16#1F7C;
%% GREEK CAPITAL LETTER OMEGA WITH OXIA
to_lower(16#1FFB)->
    16#1F7D;
%% GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
to_lower(16#1FFC)->
    16#1FF3;
%% OHM SIGN
to_lower(16#2126)->
    16#03C9;
%% KELVIN SIGN
to_lower(16#212A)->
    16#006B;
%% ANGSTROM SIGN
to_lower(16#212B)->
    16#00E5;
%% TURNED CAPITAL F
to_lower(16#2132)->
    16#214E;
%% ROMAN NUMERAL ONE
to_lower(16#2160)->
    16#2170;
%% ROMAN NUMERAL TWO
to_lower(16#2161)->
    16#2171;
%% ROMAN NUMERAL THREE
to_lower(16#2162)->
    16#2172;
%% ROMAN NUMERAL FOUR
to_lower(16#2163)->
    16#2173;
%% ROMAN NUMERAL FIVE
to_lower(16#2164)->
    16#2174;
%% ROMAN NUMERAL SIX
to_lower(16#2165)->
    16#2175;
%% ROMAN NUMERAL SEVEN
to_lower(16#2166)->
    16#2176;
%% ROMAN NUMERAL EIGHT
to_lower(16#2167)->
    16#2177;
%% ROMAN NUMERAL NINE
to_lower(16#2168)->
    16#2178;
%% ROMAN NUMERAL TEN
to_lower(16#2169)->
    16#2179;
%% ROMAN NUMERAL ELEVEN
to_lower(16#216A)->
    16#217A;
%% ROMAN NUMERAL TWELVE
to_lower(16#216B)->
    16#217B;
%% ROMAN NUMERAL FIFTY
to_lower(16#216C)->
    16#217C;
%% ROMAN NUMERAL ONE HUNDRED
to_lower(16#216D)->
    16#217D;
%% ROMAN NUMERAL FIVE HUNDRED
to_lower(16#216E)->
    16#217E;
%% ROMAN NUMERAL ONE THOUSAND
to_lower(16#216F)->
    16#217F;
%% ROMAN NUMERAL REVERSED ONE HUNDRED
to_lower(16#2183)->
    16#2184;
%% CIRCLED LATIN CAPITAL LETTER A
to_lower(16#24B6)->
    16#24D0;
%% CIRCLED LATIN CAPITAL LETTER B
to_lower(16#24B7)->
    16#24D1;
%% CIRCLED LATIN CAPITAL LETTER C
to_lower(16#24B8)->
    16#24D2;
%% CIRCLED LATIN CAPITAL LETTER D
to_lower(16#24B9)->
    16#24D3;
%% CIRCLED LATIN CAPITAL LETTER E
to_lower(16#24BA)->
    16#24D4;
%% CIRCLED LATIN CAPITAL LETTER F
to_lower(16#24BB)->
    16#24D5;
%% CIRCLED LATIN CAPITAL LETTER G
to_lower(16#24BC)->
    16#24D6;
%% CIRCLED LATIN CAPITAL LETTER H
to_lower(16#24BD)->
    16#24D7;
%% CIRCLED LATIN CAPITAL LETTER I
to_lower(16#24BE)->
    16#24D8;
%% CIRCLED LATIN CAPITAL LETTER J
to_lower(16#24BF)->
    16#24D9;
%% CIRCLED LATIN CAPITAL LETTER K
to_lower(16#24C0)->
    16#24DA;
%% CIRCLED LATIN CAPITAL LETTER L
to_lower(16#24C1)->
    16#24DB;
%% CIRCLED LATIN CAPITAL LETTER M
to_lower(16#24C2)->
    16#24DC;
%% CIRCLED LATIN CAPITAL LETTER N
to_lower(16#24C3)->
    16#24DD;
%% CIRCLED LATIN CAPITAL LETTER O
to_lower(16#24C4)->
    16#24DE;
%% CIRCLED LATIN CAPITAL LETTER P
to_lower(16#24C5)->
    16#24DF;
%% CIRCLED LATIN CAPITAL LETTER Q
to_lower(16#24C6)->
    16#24E0;
%% CIRCLED LATIN CAPITAL LETTER R
to_lower(16#24C7)->
    16#24E1;
%% CIRCLED LATIN CAPITAL LETTER S
to_lower(16#24C8)->
    16#24E2;
%% CIRCLED LATIN CAPITAL LETTER T
to_lower(16#24C9)->
    16#24E3;
%% CIRCLED LATIN CAPITAL LETTER U
to_lower(16#24CA)->
    16#24E4;
%% CIRCLED LATIN CAPITAL LETTER V
to_lower(16#24CB)->
    16#24E5;
%% CIRCLED LATIN CAPITAL LETTER W
to_lower(16#24CC)->
    16#24E6;
%% CIRCLED LATIN CAPITAL LETTER X
to_lower(16#24CD)->
    16#24E7;
%% CIRCLED LATIN CAPITAL LETTER Y
to_lower(16#24CE)->
    16#24E8;
%% CIRCLED LATIN CAPITAL LETTER Z
to_lower(16#24CF)->
    16#24E9;
%% GLAGOLITIC CAPITAL LETTER AZU
to_lower(16#2C00)->
    16#2C30;
%% GLAGOLITIC CAPITAL LETTER BUKY
to_lower(16#2C01)->
    16#2C31;
%% GLAGOLITIC CAPITAL LETTER VEDE
to_lower(16#2C02)->
    16#2C32;
%% GLAGOLITIC CAPITAL LETTER GLAGOLI
to_lower(16#2C03)->
    16#2C33;
%% GLAGOLITIC CAPITAL LETTER DOBRO
to_lower(16#2C04)->
    16#2C34;
%% GLAGOLITIC CAPITAL LETTER YESTU
to_lower(16#2C05)->
    16#2C35;
%% GLAGOLITIC CAPITAL LETTER ZHIVETE
to_lower(16#2C06)->
    16#2C36;
%% GLAGOLITIC CAPITAL LETTER DZELO
to_lower(16#2C07)->
    16#2C37;
%% GLAGOLITIC CAPITAL LETTER ZEMLJA
to_lower(16#2C08)->
    16#2C38;
%% GLAGOLITIC CAPITAL LETTER IZHE
to_lower(16#2C09)->
    16#2C39;
%% GLAGOLITIC CAPITAL LETTER INITIAL IZHE
to_lower(16#2C0A)->
    16#2C3A;
%% GLAGOLITIC CAPITAL LETTER I
to_lower(16#2C0B)->
    16#2C3B;
%% GLAGOLITIC CAPITAL LETTER DJERVI
to_lower(16#2C0C)->
    16#2C3C;
%% GLAGOLITIC CAPITAL LETTER KAKO
to_lower(16#2C0D)->
    16#2C3D;
%% GLAGOLITIC CAPITAL LETTER LJUDIJE
to_lower(16#2C0E)->
    16#2C3E;
%% GLAGOLITIC CAPITAL LETTER MYSLITE
to_lower(16#2C0F)->
    16#2C3F;
%% GLAGOLITIC CAPITAL LETTER NASHI
to_lower(16#2C10)->
    16#2C40;
%% GLAGOLITIC CAPITAL LETTER ONU
to_lower(16#2C11)->
    16#2C41;
%% GLAGOLITIC CAPITAL LETTER POKOJI
to_lower(16#2C12)->
    16#2C42;
%% GLAGOLITIC CAPITAL LETTER RITSI
to_lower(16#2C13)->
    16#2C43;
%% GLAGOLITIC CAPITAL LETTER SLOVO
to_lower(16#2C14)->
    16#2C44;
%% GLAGOLITIC CAPITAL LETTER TVRIDO
to_lower(16#2C15)->
    16#2C45;
%% GLAGOLITIC CAPITAL LETTER UKU
to_lower(16#2C16)->
    16#2C46;
%% GLAGOLITIC CAPITAL LETTER FRITU
to_lower(16#2C17)->
    16#2C47;
%% GLAGOLITIC CAPITAL LETTER HERU
to_lower(16#2C18)->
    16#2C48;
%% GLAGOLITIC CAPITAL LETTER OTU
to_lower(16#2C19)->
    16#2C49;
%% GLAGOLITIC CAPITAL LETTER PE
to_lower(16#2C1A)->
    16#2C4A;
%% GLAGOLITIC CAPITAL LETTER SHTA
to_lower(16#2C1B)->
    16#2C4B;
%% GLAGOLITIC CAPITAL LETTER TSI
to_lower(16#2C1C)->
    16#2C4C;
%% GLAGOLITIC CAPITAL LETTER CHRIVI
to_lower(16#2C1D)->
    16#2C4D;
%% GLAGOLITIC CAPITAL LETTER SHA
to_lower(16#2C1E)->
    16#2C4E;
%% GLAGOLITIC CAPITAL LETTER YERU
to_lower(16#2C1F)->
    16#2C4F;
%% GLAGOLITIC CAPITAL LETTER YERI
to_lower(16#2C20)->
    16#2C50;
%% GLAGOLITIC CAPITAL LETTER YATI
to_lower(16#2C21)->
    16#2C51;
%% GLAGOLITIC CAPITAL LETTER SPIDERY HA
to_lower(16#2C22)->
    16#2C52;
%% GLAGOLITIC CAPITAL LETTER YU
to_lower(16#2C23)->
    16#2C53;
%% GLAGOLITIC CAPITAL LETTER SMALL YUS
to_lower(16#2C24)->
    16#2C54;
%% GLAGOLITIC CAPITAL LETTER SMALL YUS WITH TAIL
to_lower(16#2C25)->
    16#2C55;
%% GLAGOLITIC CAPITAL LETTER YO
to_lower(16#2C26)->
    16#2C56;
%% GLAGOLITIC CAPITAL LETTER IOTATED SMALL YUS
to_lower(16#2C27)->
    16#2C57;
%% GLAGOLITIC CAPITAL LETTER BIG YUS
to_lower(16#2C28)->
    16#2C58;
%% GLAGOLITIC CAPITAL LETTER IOTATED BIG YUS
to_lower(16#2C29)->
    16#2C59;
%% GLAGOLITIC CAPITAL LETTER FITA
to_lower(16#2C2A)->
    16#2C5A;
%% GLAGOLITIC CAPITAL LETTER IZHITSA
to_lower(16#2C2B)->
    16#2C5B;
%% GLAGOLITIC CAPITAL LETTER SHTAPIC
to_lower(16#2C2C)->
    16#2C5C;
%% GLAGOLITIC CAPITAL LETTER TROKUTASTI A
to_lower(16#2C2D)->
    16#2C5D;
%% GLAGOLITIC CAPITAL LETTER LATINATE MYSLITE
to_lower(16#2C2E)->
    16#2C5E;
%% LATIN CAPITAL LETTER L WITH DOUBLE BAR
to_lower(16#2C60)->
    16#2C61;
%% LATIN CAPITAL LETTER L WITH MIDDLE TILDE
to_lower(16#2C62)->
    16#026B;
%% LATIN CAPITAL LETTER P WITH STROKE
to_lower(16#2C63)->
    16#1D7D;
%% LATIN CAPITAL LETTER R WITH TAIL
to_lower(16#2C64)->
    16#027D;
%% LATIN CAPITAL LETTER H WITH DESCENDER
to_lower(16#2C67)->
    16#2C68;
%% LATIN CAPITAL LETTER K WITH DESCENDER
to_lower(16#2C69)->
    16#2C6A;
%% LATIN CAPITAL LETTER Z WITH DESCENDER
to_lower(16#2C6B)->
    16#2C6C;
%% LATIN CAPITAL LETTER ALPHA
to_lower(16#2C6D)->
    16#0251;
%% LATIN CAPITAL LETTER M WITH HOOK
to_lower(16#2C6E)->
    16#0271;
%% LATIN CAPITAL LETTER TURNED A
to_lower(16#2C6F)->
    16#0250;
%% LATIN CAPITAL LETTER TURNED ALPHA
to_lower(16#2C70)->
    16#0252;
%% LATIN CAPITAL LETTER W WITH HOOK
to_lower(16#2C72)->
    16#2C73;
%% LATIN CAPITAL LETTER HALF H
to_lower(16#2C75)->
    16#2C76;
%% LATIN CAPITAL LETTER S WITH SWASH TAIL
to_lower(16#2C7E)->
    16#023F;
%% LATIN CAPITAL LETTER Z WITH SWASH TAIL
to_lower(16#2C7F)->
    16#0240;
%% COPTIC CAPITAL LETTER ALFA
to_lower(16#2C80)->
    16#2C81;
%% COPTIC CAPITAL LETTER VIDA
to_lower(16#2C82)->
    16#2C83;
%% COPTIC CAPITAL LETTER GAMMA
to_lower(16#2C84)->
    16#2C85;
%% COPTIC CAPITAL LETTER DALDA
to_lower(16#2C86)->
    16#2C87;
%% COPTIC CAPITAL LETTER EIE
to_lower(16#2C88)->
    16#2C89;
%% COPTIC CAPITAL LETTER SOU
to_lower(16#2C8A)->
    16#2C8B;
%% COPTIC CAPITAL LETTER ZATA
to_lower(16#2C8C)->
    16#2C8D;
%% COPTIC CAPITAL LETTER HATE
to_lower(16#2C8E)->
    16#2C8F;
%% COPTIC CAPITAL LETTER THETHE
to_lower(16#2C90)->
    16#2C91;
%% COPTIC CAPITAL LETTER IAUDA
to_lower(16#2C92)->
    16#2C93;
%% COPTIC CAPITAL LETTER KAPA
to_lower(16#2C94)->
    16#2C95;
%% COPTIC CAPITAL LETTER LAULA
to_lower(16#2C96)->
    16#2C97;
%% COPTIC CAPITAL LETTER MI
to_lower(16#2C98)->
    16#2C99;
%% COPTIC CAPITAL LETTER NI
to_lower(16#2C9A)->
    16#2C9B;
%% COPTIC CAPITAL LETTER KSI
to_lower(16#2C9C)->
    16#2C9D;
%% COPTIC CAPITAL LETTER O
to_lower(16#2C9E)->
    16#2C9F;
%% COPTIC CAPITAL LETTER PI
to_lower(16#2CA0)->
    16#2CA1;
%% COPTIC CAPITAL LETTER RO
to_lower(16#2CA2)->
    16#2CA3;
%% COPTIC CAPITAL LETTER SIMA
to_lower(16#2CA4)->
    16#2CA5;
%% COPTIC CAPITAL LETTER TAU
to_lower(16#2CA6)->
    16#2CA7;
%% COPTIC CAPITAL LETTER UA
to_lower(16#2CA8)->
    16#2CA9;
%% COPTIC CAPITAL LETTER FI
to_lower(16#2CAA)->
    16#2CAB;
%% COPTIC CAPITAL LETTER KHI
to_lower(16#2CAC)->
    16#2CAD;
%% COPTIC CAPITAL LETTER PSI
to_lower(16#2CAE)->
    16#2CAF;
%% COPTIC CAPITAL LETTER OOU
to_lower(16#2CB0)->
    16#2CB1;
%% COPTIC CAPITAL LETTER DIALECT-P ALEF
to_lower(16#2CB2)->
    16#2CB3;
%% COPTIC CAPITAL LETTER OLD COPTIC AIN
to_lower(16#2CB4)->
    16#2CB5;
%% COPTIC CAPITAL LETTER CRYPTOGRAMMIC EIE
to_lower(16#2CB6)->
    16#2CB7;
%% COPTIC CAPITAL LETTER DIALECT-P KAPA
to_lower(16#2CB8)->
    16#2CB9;
%% COPTIC CAPITAL LETTER DIALECT-P NI
to_lower(16#2CBA)->
    16#2CBB;
%% COPTIC CAPITAL LETTER CRYPTOGRAMMIC NI
to_lower(16#2CBC)->
    16#2CBD;
%% COPTIC CAPITAL LETTER OLD COPTIC OOU
to_lower(16#2CBE)->
    16#2CBF;
%% COPTIC CAPITAL LETTER SAMPI
to_lower(16#2CC0)->
    16#2CC1;
%% COPTIC CAPITAL LETTER CROSSED SHEI
to_lower(16#2CC2)->
    16#2CC3;
%% COPTIC CAPITAL LETTER OLD COPTIC SHEI
to_lower(16#2CC4)->
    16#2CC5;
%% COPTIC CAPITAL LETTER OLD COPTIC ESH
to_lower(16#2CC6)->
    16#2CC7;
%% COPTIC CAPITAL LETTER AKHMIMIC KHEI
to_lower(16#2CC8)->
    16#2CC9;
%% COPTIC CAPITAL LETTER DIALECT-P HORI
to_lower(16#2CCA)->
    16#2CCB;
%% COPTIC CAPITAL LETTER OLD COPTIC HORI
to_lower(16#2CCC)->
    16#2CCD;
%% COPTIC CAPITAL LETTER OLD COPTIC HA
to_lower(16#2CCE)->
    16#2CCF;
%% COPTIC CAPITAL LETTER L-SHAPED HA
to_lower(16#2CD0)->
    16#2CD1;
%% COPTIC CAPITAL LETTER OLD COPTIC HEI
to_lower(16#2CD2)->
    16#2CD3;
%% COPTIC CAPITAL LETTER OLD COPTIC HAT
to_lower(16#2CD4)->
    16#2CD5;
%% COPTIC CAPITAL LETTER OLD COPTIC GANGIA
to_lower(16#2CD6)->
    16#2CD7;
%% COPTIC CAPITAL LETTER OLD COPTIC DJA
to_lower(16#2CD8)->
    16#2CD9;
%% COPTIC CAPITAL LETTER OLD COPTIC SHIMA
to_lower(16#2CDA)->
    16#2CDB;
%% COPTIC CAPITAL LETTER OLD NUBIAN SHIMA
to_lower(16#2CDC)->
    16#2CDD;
%% COPTIC CAPITAL LETTER OLD NUBIAN NGI
to_lower(16#2CDE)->
    16#2CDF;
%% COPTIC CAPITAL LETTER OLD NUBIAN NYI
to_lower(16#2CE0)->
    16#2CE1;
%% COPTIC CAPITAL LETTER OLD NUBIAN WAU
to_lower(16#2CE2)->
    16#2CE3;
%% COPTIC CAPITAL LETTER CRYPTOGRAMMIC SHEI
to_lower(16#2CEB)->
    16#2CEC;
%% COPTIC CAPITAL LETTER CRYPTOGRAMMIC GANGIA
to_lower(16#2CED)->
    16#2CEE;
%% COPTIC CAPITAL LETTER BOHAIRIC KHEI
to_lower(16#2CF2)->
    16#2CF3;
%% CYRILLIC CAPITAL LETTER ZEMLYA
to_lower(16#A640)->
    16#A641;
%% CYRILLIC CAPITAL LETTER DZELO
to_lower(16#A642)->
    16#A643;
%% CYRILLIC CAPITAL LETTER REVERSED DZE
to_lower(16#A644)->
    16#A645;
%% CYRILLIC CAPITAL LETTER IOTA
to_lower(16#A646)->
    16#A647;
%% CYRILLIC CAPITAL LETTER DJERV
to_lower(16#A648)->
    16#A649;
%% CYRILLIC CAPITAL LETTER MONOGRAPH UK
to_lower(16#A64A)->
    16#A64B;
%% CYRILLIC CAPITAL LETTER BROAD OMEGA
to_lower(16#A64C)->
    16#A64D;
%% CYRILLIC CAPITAL LETTER NEUTRAL YER
to_lower(16#A64E)->
    16#A64F;
%% CYRILLIC CAPITAL LETTER YERU WITH BACK YER
to_lower(16#A650)->
    16#A651;
%% CYRILLIC CAPITAL LETTER IOTIFIED YAT
to_lower(16#A652)->
    16#A653;
%% CYRILLIC CAPITAL LETTER REVERSED YU
to_lower(16#A654)->
    16#A655;
%% CYRILLIC CAPITAL LETTER IOTIFIED A
to_lower(16#A656)->
    16#A657;
%% CYRILLIC CAPITAL LETTER CLOSED LITTLE YUS
to_lower(16#A658)->
    16#A659;
%% CYRILLIC CAPITAL LETTER BLENDED YUS
to_lower(16#A65A)->
    16#A65B;
%% CYRILLIC CAPITAL LETTER IOTIFIED CLOSED LITTLE YUS
to_lower(16#A65C)->
    16#A65D;
%% CYRILLIC CAPITAL LETTER YN
to_lower(16#A65E)->
    16#A65F;
%% CYRILLIC CAPITAL LETTER REVERSED TSE
to_lower(16#A660)->
    16#A661;
%% CYRILLIC CAPITAL LETTER SOFT DE
to_lower(16#A662)->
    16#A663;
%% CYRILLIC CAPITAL LETTER SOFT EL
to_lower(16#A664)->
    16#A665;
%% CYRILLIC CAPITAL LETTER SOFT EM
to_lower(16#A666)->
    16#A667;
%% CYRILLIC CAPITAL LETTER MONOCULAR O
to_lower(16#A668)->
    16#A669;
%% CYRILLIC CAPITAL LETTER BINOCULAR O
to_lower(16#A66A)->
    16#A66B;
%% CYRILLIC CAPITAL LETTER DOUBLE MONOCULAR O
to_lower(16#A66C)->
    16#A66D;
%% CYRILLIC CAPITAL LETTER DWE
to_lower(16#A680)->
    16#A681;
%% CYRILLIC CAPITAL LETTER DZWE
to_lower(16#A682)->
    16#A683;
%% CYRILLIC CAPITAL LETTER ZHWE
to_lower(16#A684)->
    16#A685;
%% CYRILLIC CAPITAL LETTER CCHE
to_lower(16#A686)->
    16#A687;
%% CYRILLIC CAPITAL LETTER DZZE
to_lower(16#A688)->
    16#A689;
%% CYRILLIC CAPITAL LETTER TE WITH MIDDLE HOOK
to_lower(16#A68A)->
    16#A68B;
%% CYRILLIC CAPITAL LETTER TWE
to_lower(16#A68C)->
    16#A68D;
%% CYRILLIC CAPITAL LETTER TSWE
to_lower(16#A68E)->
    16#A68F;
%% CYRILLIC CAPITAL LETTER TSSE
to_lower(16#A690)->
    16#A691;
%% CYRILLIC CAPITAL LETTER TCHE
to_lower(16#A692)->
    16#A693;
%% CYRILLIC CAPITAL LETTER HWE
to_lower(16#A694)->
    16#A695;
%% CYRILLIC CAPITAL LETTER SHWE
to_lower(16#A696)->
    16#A697;
%% LATIN CAPITAL LETTER EGYPTOLOGICAL ALEF
to_lower(16#A722)->
    16#A723;
%% LATIN CAPITAL LETTER EGYPTOLOGICAL AIN
to_lower(16#A724)->
    16#A725;
%% LATIN CAPITAL LETTER HENG
to_lower(16#A726)->
    16#A727;
%% LATIN CAPITAL LETTER TZ
to_lower(16#A728)->
    16#A729;
%% LATIN CAPITAL LETTER TRESILLO
to_lower(16#A72A)->
    16#A72B;
%% LATIN CAPITAL LETTER CUATRILLO
to_lower(16#A72C)->
    16#A72D;
%% LATIN CAPITAL LETTER CUATRILLO WITH COMMA
to_lower(16#A72E)->
    16#A72F;
%% LATIN CAPITAL LETTER AA
to_lower(16#A732)->
    16#A733;
%% LATIN CAPITAL LETTER AO
to_lower(16#A734)->
    16#A735;
%% LATIN CAPITAL LETTER AU
to_lower(16#A736)->
    16#A737;
%% LATIN CAPITAL LETTER AV
to_lower(16#A738)->
    16#A739;
%% LATIN CAPITAL LETTER AV WITH HORIZONTAL BAR
to_lower(16#A73A)->
    16#A73B;
%% LATIN CAPITAL LETTER AY
to_lower(16#A73C)->
    16#A73D;
%% LATIN CAPITAL LETTER REVERSED C WITH DOT
to_lower(16#A73E)->
    16#A73F;
%% LATIN CAPITAL LETTER K WITH STROKE
to_lower(16#A740)->
    16#A741;
%% LATIN CAPITAL LETTER K WITH DIAGONAL STROKE
to_lower(16#A742)->
    16#A743;
%% LATIN CAPITAL LETTER K WITH STROKE AND DIAGONAL STROKE
to_lower(16#A744)->
    16#A745;
%% LATIN CAPITAL LETTER BROKEN L
to_lower(16#A746)->
    16#A747;
%% LATIN CAPITAL LETTER L WITH HIGH STROKE
to_lower(16#A748)->
    16#A749;
%% LATIN CAPITAL LETTER O WITH LONG STROKE OVERLAY
to_lower(16#A74A)->
    16#A74B;
%% LATIN CAPITAL LETTER O WITH LOOP
to_lower(16#A74C)->
    16#A74D;
%% LATIN CAPITAL LETTER OO
to_lower(16#A74E)->
    16#A74F;
%% LATIN CAPITAL LETTER P WITH STROKE THROUGH DESCENDER
to_lower(16#A750)->
    16#A751;
%% LATIN CAPITAL LETTER P WITH FLOURISH
to_lower(16#A752)->
    16#A753;
%% LATIN CAPITAL LETTER P WITH SQUIRREL TAIL
to_lower(16#A754)->
    16#A755;
%% LATIN CAPITAL LETTER Q WITH STROKE THROUGH DESCENDER
to_lower(16#A756)->
    16#A757;
%% LATIN CAPITAL LETTER Q WITH DIAGONAL STROKE
to_lower(16#A758)->
    16#A759;
%% LATIN CAPITAL LETTER R ROTUNDA
to_lower(16#A75A)->
    16#A75B;
%% LATIN CAPITAL LETTER RUM ROTUNDA
to_lower(16#A75C)->
    16#A75D;
%% LATIN CAPITAL LETTER V WITH DIAGONAL STROKE
to_lower(16#A75E)->
    16#A75F;
%% LATIN CAPITAL LETTER VY
to_lower(16#A760)->
    16#A761;
%% LATIN CAPITAL LETTER VISIGOTHIC Z
to_lower(16#A762)->
    16#A763;
%% LATIN CAPITAL LETTER THORN WITH STROKE
to_lower(16#A764)->
    16#A765;
%% LATIN CAPITAL LETTER THORN WITH STROKE THROUGH DESCENDER
to_lower(16#A766)->
    16#A767;
%% LATIN CAPITAL LETTER VEND
to_lower(16#A768)->
    16#A769;
%% LATIN CAPITAL LETTER ET
to_lower(16#A76A)->
    16#A76B;
%% LATIN CAPITAL LETTER IS
to_lower(16#A76C)->
    16#A76D;
%% LATIN CAPITAL LETTER CON
to_lower(16#A76E)->
    16#A76F;
%% LATIN CAPITAL LETTER INSULAR D
to_lower(16#A779)->
    16#A77A;
%% LATIN CAPITAL LETTER INSULAR F
to_lower(16#A77B)->
    16#A77C;
%% LATIN CAPITAL LETTER INSULAR G
to_lower(16#A77D)->
    16#1D79;
%% LATIN CAPITAL LETTER TURNED INSULAR G
to_lower(16#A77E)->
    16#A77F;
%% LATIN CAPITAL LETTER TURNED L
to_lower(16#A780)->
    16#A781;
%% LATIN CAPITAL LETTER INSULAR R
to_lower(16#A782)->
    16#A783;
%% LATIN CAPITAL LETTER INSULAR S
to_lower(16#A784)->
    16#A785;
%% LATIN CAPITAL LETTER INSULAR T
to_lower(16#A786)->
    16#A787;
%% LATIN CAPITAL LETTER SALTILLO
to_lower(16#A78B)->
    16#A78C;
%% LATIN CAPITAL LETTER TURNED H
to_lower(16#A78D)->
    16#0265;
%% LATIN CAPITAL LETTER N WITH DESCENDER
to_lower(16#A790)->
    16#A791;
%% LATIN CAPITAL LETTER C WITH BAR
to_lower(16#A792)->
    16#A793;
%% LATIN CAPITAL LETTER G WITH OBLIQUE STROKE
to_lower(16#A7A0)->
    16#A7A1;
%% LATIN CAPITAL LETTER K WITH OBLIQUE STROKE
to_lower(16#A7A2)->
    16#A7A3;
%% LATIN CAPITAL LETTER N WITH OBLIQUE STROKE
to_lower(16#A7A4)->
    16#A7A5;
%% LATIN CAPITAL LETTER R WITH OBLIQUE STROKE
to_lower(16#A7A6)->
    16#A7A7;
%% LATIN CAPITAL LETTER S WITH OBLIQUE STROKE
to_lower(16#A7A8)->
    16#A7A9;
%% LATIN CAPITAL LETTER H WITH HOOK
to_lower(16#A7AA)->
    16#0266;
%% FULLWIDTH LATIN CAPITAL LETTER A
to_lower(16#FF21)->
    16#FF41;
%% FULLWIDTH LATIN CAPITAL LETTER B
to_lower(16#FF22)->
    16#FF42;
%% FULLWIDTH LATIN CAPITAL LETTER C
to_lower(16#FF23)->
    16#FF43;
%% FULLWIDTH LATIN CAPITAL LETTER D
to_lower(16#FF24)->
    16#FF44;
%% FULLWIDTH LATIN CAPITAL LETTER E
to_lower(16#FF25)->
    16#FF45;
%% FULLWIDTH LATIN CAPITAL LETTER F
to_lower(16#FF26)->
    16#FF46;
%% FULLWIDTH LATIN CAPITAL LETTER G
to_lower(16#FF27)->
    16#FF47;
%% FULLWIDTH LATIN CAPITAL LETTER H
to_lower(16#FF28)->
    16#FF48;
%% FULLWIDTH LATIN CAPITAL LETTER I
to_lower(16#FF29)->
    16#FF49;
%% FULLWIDTH LATIN CAPITAL LETTER J
to_lower(16#FF2A)->
    16#FF4A;
%% FULLWIDTH LATIN CAPITAL LETTER K
to_lower(16#FF2B)->
    16#FF4B;
%% FULLWIDTH LATIN CAPITAL LETTER L
to_lower(16#FF2C)->
    16#FF4C;
%% FULLWIDTH LATIN CAPITAL LETTER M
to_lower(16#FF2D)->
    16#FF4D;
%% FULLWIDTH LATIN CAPITAL LETTER N
to_lower(16#FF2E)->
    16#FF4E;
%% FULLWIDTH LATIN CAPITAL LETTER O
to_lower(16#FF2F)->
    16#FF4F;
%% FULLWIDTH LATIN CAPITAL LETTER P
to_lower(16#FF30)->
    16#FF50;
%% FULLWIDTH LATIN CAPITAL LETTER Q
to_lower(16#FF31)->
    16#FF51;
%% FULLWIDTH LATIN CAPITAL LETTER R
to_lower(16#FF32)->
    16#FF52;
%% FULLWIDTH LATIN CAPITAL LETTER S
to_lower(16#FF33)->
    16#FF53;
%% FULLWIDTH LATIN CAPITAL LETTER T
to_lower(16#FF34)->
    16#FF54;
%% FULLWIDTH LATIN CAPITAL LETTER U
to_lower(16#FF35)->
    16#FF55;
%% FULLWIDTH LATIN CAPITAL LETTER V
to_lower(16#FF36)->
    16#FF56;
%% FULLWIDTH LATIN CAPITAL LETTER W
to_lower(16#FF37)->
    16#FF57;
%% FULLWIDTH LATIN CAPITAL LETTER X
to_lower(16#FF38)->
    16#FF58;
%% FULLWIDTH LATIN CAPITAL LETTER Y
to_lower(16#FF39)->
    16#FF59;
%% FULLWIDTH LATIN CAPITAL LETTER Z
to_lower(16#FF3A)->
    16#FF5A;
%% DESERET CAPITAL LETTER LONG I
to_lower(16#10400)->
    16#10428;
%% DESERET CAPITAL LETTER LONG E
to_lower(16#10401)->
    16#10429;
%% DESERET CAPITAL LETTER LONG A
to_lower(16#10402)->
    16#1042A;
%% DESERET CAPITAL LETTER LONG AH
to_lower(16#10403)->
    16#1042B;
%% DESERET CAPITAL LETTER LONG O
to_lower(16#10404)->
    16#1042C;
%% DESERET CAPITAL LETTER LONG OO
to_lower(16#10405)->
    16#1042D;
%% DESERET CAPITAL LETTER SHORT I
to_lower(16#10406)->
    16#1042E;
%% DESERET CAPITAL LETTER SHORT E
to_lower(16#10407)->
    16#1042F;
%% DESERET CAPITAL LETTER SHORT A
to_lower(16#10408)->
    16#10430;
%% DESERET CAPITAL LETTER SHORT AH
to_lower(16#10409)->
    16#10431;
%% DESERET CAPITAL LETTER SHORT O
to_lower(16#1040A)->
    16#10432;
%% DESERET CAPITAL LETTER SHORT OO
to_lower(16#1040B)->
    16#10433;
%% DESERET CAPITAL LETTER AY
to_lower(16#1040C)->
    16#10434;
%% DESERET CAPITAL LETTER OW
to_lower(16#1040D)->
    16#10435;
%% DESERET CAPITAL LETTER WU
to_lower(16#1040E)->
    16#10436;
%% DESERET CAPITAL LETTER YEE
to_lower(16#1040F)->
    16#10437;
%% DESERET CAPITAL LETTER H
to_lower(16#10410)->
    16#10438;
%% DESERET CAPITAL LETTER PEE
to_lower(16#10411)->
    16#10439;
%% DESERET CAPITAL LETTER BEE
to_lower(16#10412)->
    16#1043A;
%% DESERET CAPITAL LETTER TEE
to_lower(16#10413)->
    16#1043B;
%% DESERET CAPITAL LETTER DEE
to_lower(16#10414)->
    16#1043C;
%% DESERET CAPITAL LETTER CHEE
to_lower(16#10415)->
    16#1043D;
%% DESERET CAPITAL LETTER JEE
to_lower(16#10416)->
    16#1043E;
%% DESERET CAPITAL LETTER KAY
to_lower(16#10417)->
    16#1043F;
%% DESERET CAPITAL LETTER GAY
to_lower(16#10418)->
    16#10440;
%% DESERET CAPITAL LETTER EF
to_lower(16#10419)->
    16#10441;
%% DESERET CAPITAL LETTER VEE
to_lower(16#1041A)->
    16#10442;
%% DESERET CAPITAL LETTER ETH
to_lower(16#1041B)->
    16#10443;
%% DESERET CAPITAL LETTER THEE
to_lower(16#1041C)->
    16#10444;
%% DESERET CAPITAL LETTER ES
to_lower(16#1041D)->
    16#10445;
%% DESERET CAPITAL LETTER ZEE
to_lower(16#1041E)->
    16#10446;
%% DESERET CAPITAL LETTER ESH
to_lower(16#1041F)->
    16#10447;
%% DESERET CAPITAL LETTER ZHEE
to_lower(16#10420)->
    16#10448;
%% DESERET CAPITAL LETTER ER
to_lower(16#10421)->
    16#10449;
%% DESERET CAPITAL LETTER EL
to_lower(16#10422)->
    16#1044A;
%% DESERET CAPITAL LETTER EM
to_lower(16#10423)->
    16#1044B;
%% DESERET CAPITAL LETTER EN
to_lower(16#10424)->
    16#1044C;
%% DESERET CAPITAL LETTER ENG
to_lower(16#10425)->
    16#1044D;
%% DESERET CAPITAL LETTER OI
to_lower(16#10426)->
    16#1044E;
%% DESERET CAPITAL LETTER EW
to_lower(16#10427)->
    16#1044F;
to_lower(C) ->
    C.

%% LATIN SMALL LETTER A
to_title(16#0061)->
    16#0041;
%% LATIN SMALL LETTER B
to_title(16#0062)->
    16#0042;
%% LATIN SMALL LETTER C
to_title(16#0063)->
    16#0043;
%% LATIN SMALL LETTER D
to_title(16#0064)->
    16#0044;
%% LATIN SMALL LETTER E
to_title(16#0065)->
    16#0045;
%% LATIN SMALL LETTER F
to_title(16#0066)->
    16#0046;
%% LATIN SMALL LETTER G
to_title(16#0067)->
    16#0047;
%% LATIN SMALL LETTER H
to_title(16#0068)->
    16#0048;
%% LATIN SMALL LETTER I
to_title(16#0069)->
    16#0049;
%% LATIN SMALL LETTER J
to_title(16#006A)->
    16#004A;
%% LATIN SMALL LETTER K
to_title(16#006B)->
    16#004B;
%% LATIN SMALL LETTER L
to_title(16#006C)->
    16#004C;
%% LATIN SMALL LETTER M
to_title(16#006D)->
    16#004D;
%% LATIN SMALL LETTER N
to_title(16#006E)->
    16#004E;
%% LATIN SMALL LETTER O
to_title(16#006F)->
    16#004F;
%% LATIN SMALL LETTER P
to_title(16#0070)->
    16#0050;
%% LATIN SMALL LETTER Q
to_title(16#0071)->
    16#0051;
%% LATIN SMALL LETTER R
to_title(16#0072)->
    16#0052;
%% LATIN SMALL LETTER S
to_title(16#0073)->
    16#0053;
%% LATIN SMALL LETTER T
to_title(16#0074)->
    16#0054;
%% LATIN SMALL LETTER U
to_title(16#0075)->
    16#0055;
%% LATIN SMALL LETTER V
to_title(16#0076)->
    16#0056;
%% LATIN SMALL LETTER W
to_title(16#0077)->
    16#0057;
%% LATIN SMALL LETTER X
to_title(16#0078)->
    16#0058;
%% LATIN SMALL LETTER Y
to_title(16#0079)->
    16#0059;
%% LATIN SMALL LETTER Z
to_title(16#007A)->
    16#005A;
%% MICRO SIGN
to_title(16#00B5)->
    16#039C;
%% LATIN SMALL LETTER A WITH GRAVE
to_title(16#00E0)->
    16#00C0;
%% LATIN SMALL LETTER A WITH ACUTE
to_title(16#00E1)->
    16#00C1;
%% LATIN SMALL LETTER A WITH CIRCUMFLEX
to_title(16#00E2)->
    16#00C2;
%% LATIN SMALL LETTER A WITH TILDE
to_title(16#00E3)->
    16#00C3;
%% LATIN SMALL LETTER A WITH DIAERESIS
to_title(16#00E4)->
    16#00C4;
%% LATIN SMALL LETTER A WITH RING ABOVE
to_title(16#00E5)->
    16#00C5;
%% LATIN SMALL LETTER AE
to_title(16#00E6)->
    16#00C6;
%% LATIN SMALL LETTER C WITH CEDILLA
to_title(16#00E7)->
    16#00C7;
%% LATIN SMALL LETTER E WITH GRAVE
to_title(16#00E8)->
    16#00C8;
%% LATIN SMALL LETTER E WITH ACUTE
to_title(16#00E9)->
    16#00C9;
%% LATIN SMALL LETTER E WITH CIRCUMFLEX
to_title(16#00EA)->
    16#00CA;
%% LATIN SMALL LETTER E WITH DIAERESIS
to_title(16#00EB)->
    16#00CB;
%% LATIN SMALL LETTER I WITH GRAVE
to_title(16#00EC)->
    16#00CC;
%% LATIN SMALL LETTER I WITH ACUTE
to_title(16#00ED)->
    16#00CD;
%% LATIN SMALL LETTER I WITH CIRCUMFLEX
to_title(16#00EE)->
    16#00CE;
%% LATIN SMALL LETTER I WITH DIAERESIS
to_title(16#00EF)->
    16#00CF;
%% LATIN SMALL LETTER ETH
to_title(16#00F0)->
    16#00D0;
%% LATIN SMALL LETTER N WITH TILDE
to_title(16#00F1)->
    16#00D1;
%% LATIN SMALL LETTER O WITH GRAVE
to_title(16#00F2)->
    16#00D2;
%% LATIN SMALL LETTER O WITH ACUTE
to_title(16#00F3)->
    16#00D3;
%% LATIN SMALL LETTER O WITH CIRCUMFLEX
to_title(16#00F4)->
    16#00D4;
%% LATIN SMALL LETTER O WITH TILDE
to_title(16#00F5)->
    16#00D5;
%% LATIN SMALL LETTER O WITH DIAERESIS
to_title(16#00F6)->
    16#00D6;
%% LATIN SMALL LETTER O WITH STROKE
to_title(16#00F8)->
    16#00D8;
%% LATIN SMALL LETTER U WITH GRAVE
to_title(16#00F9)->
    16#00D9;
%% LATIN SMALL LETTER U WITH ACUTE
to_title(16#00FA)->
    16#00DA;
%% LATIN SMALL LETTER U WITH CIRCUMFLEX
to_title(16#00FB)->
    16#00DB;
%% LATIN SMALL LETTER U WITH DIAERESIS
to_title(16#00FC)->
    16#00DC;
%% LATIN SMALL LETTER Y WITH ACUTE
to_title(16#00FD)->
    16#00DD;
%% LATIN SMALL LETTER THORN
to_title(16#00FE)->
    16#00DE;
%% LATIN SMALL LETTER Y WITH DIAERESIS
to_title(16#00FF)->
    16#0178;
%% LATIN SMALL LETTER A WITH MACRON
to_title(16#0101)->
    16#0100;
%% LATIN SMALL LETTER A WITH BREVE
to_title(16#0103)->
    16#0102;
%% LATIN SMALL LETTER A WITH OGONEK
to_title(16#0105)->
    16#0104;
%% LATIN SMALL LETTER C WITH ACUTE
to_title(16#0107)->
    16#0106;
%% LATIN SMALL LETTER C WITH CIRCUMFLEX
to_title(16#0109)->
    16#0108;
%% LATIN SMALL LETTER C WITH DOT ABOVE
to_title(16#010B)->
    16#010A;
%% LATIN SMALL LETTER C WITH CARON
to_title(16#010D)->
    16#010C;
%% LATIN SMALL LETTER D WITH CARON
to_title(16#010F)->
    16#010E;
%% LATIN SMALL LETTER D WITH STROKE
to_title(16#0111)->
    16#0110;
%% LATIN SMALL LETTER E WITH MACRON
to_title(16#0113)->
    16#0112;
%% LATIN SMALL LETTER E WITH BREVE
to_title(16#0115)->
    16#0114;
%% LATIN SMALL LETTER E WITH DOT ABOVE
to_title(16#0117)->
    16#0116;
%% LATIN SMALL LETTER E WITH OGONEK
to_title(16#0119)->
    16#0118;
%% LATIN SMALL LETTER E WITH CARON
to_title(16#011B)->
    16#011A;
%% LATIN SMALL LETTER G WITH CIRCUMFLEX
to_title(16#011D)->
    16#011C;
%% LATIN SMALL LETTER G WITH BREVE
to_title(16#011F)->
    16#011E;
%% LATIN SMALL LETTER G WITH DOT ABOVE
to_title(16#0121)->
    16#0120;
%% LATIN SMALL LETTER G WITH CEDILLA
to_title(16#0123)->
    16#0122;
%% LATIN SMALL LETTER H WITH CIRCUMFLEX
to_title(16#0125)->
    16#0124;
%% LATIN SMALL LETTER H WITH STROKE
to_title(16#0127)->
    16#0126;
%% LATIN SMALL LETTER I WITH TILDE
to_title(16#0129)->
    16#0128;
%% LATIN SMALL LETTER I WITH MACRON
to_title(16#012B)->
    16#012A;
%% LATIN SMALL LETTER I WITH BREVE
to_title(16#012D)->
    16#012C;
%% LATIN SMALL LETTER I WITH OGONEK
to_title(16#012F)->
    16#012E;
%% LATIN SMALL LETTER DOTLESS I
to_title(16#0131)->
    16#0049;
%% LATIN SMALL LIGATURE IJ
to_title(16#0133)->
    16#0132;
%% LATIN SMALL LETTER J WITH CIRCUMFLEX
to_title(16#0135)->
    16#0134;
%% LATIN SMALL LETTER K WITH CEDILLA
to_title(16#0137)->
    16#0136;
%% LATIN SMALL LETTER L WITH ACUTE
to_title(16#013A)->
    16#0139;
%% LATIN SMALL LETTER L WITH CEDILLA
to_title(16#013C)->
    16#013B;
%% LATIN SMALL LETTER L WITH CARON
to_title(16#013E)->
    16#013D;
%% LATIN SMALL LETTER L WITH MIDDLE DOT
to_title(16#0140)->
    16#013F;
%% LATIN SMALL LETTER L WITH STROKE
to_title(16#0142)->
    16#0141;
%% LATIN SMALL LETTER N WITH ACUTE
to_title(16#0144)->
    16#0143;
%% LATIN SMALL LETTER N WITH CEDILLA
to_title(16#0146)->
    16#0145;
%% LATIN SMALL LETTER N WITH CARON
to_title(16#0148)->
    16#0147;
%% LATIN SMALL LETTER ENG
to_title(16#014B)->
    16#014A;
%% LATIN SMALL LETTER O WITH MACRON
to_title(16#014D)->
    16#014C;
%% LATIN SMALL LETTER O WITH BREVE
to_title(16#014F)->
    16#014E;
%% LATIN SMALL LETTER O WITH DOUBLE ACUTE
to_title(16#0151)->
    16#0150;
%% LATIN SMALL LIGATURE OE
to_title(16#0153)->
    16#0152;
%% LATIN SMALL LETTER R WITH ACUTE
to_title(16#0155)->
    16#0154;
%% LATIN SMALL LETTER R WITH CEDILLA
to_title(16#0157)->
    16#0156;
%% LATIN SMALL LETTER R WITH CARON
to_title(16#0159)->
    16#0158;
%% LATIN SMALL LETTER S WITH ACUTE
to_title(16#015B)->
    16#015A;
%% LATIN SMALL LETTER S WITH CIRCUMFLEX
to_title(16#015D)->
    16#015C;
%% LATIN SMALL LETTER S WITH CEDILLA
to_title(16#015F)->
    16#015E;
%% LATIN SMALL LETTER S WITH CARON
to_title(16#0161)->
    16#0160;
%% LATIN SMALL LETTER T WITH CEDILLA
to_title(16#0163)->
    16#0162;
%% LATIN SMALL LETTER T WITH CARON
to_title(16#0165)->
    16#0164;
%% LATIN SMALL LETTER T WITH STROKE
to_title(16#0167)->
    16#0166;
%% LATIN SMALL LETTER U WITH TILDE
to_title(16#0169)->
    16#0168;
%% LATIN SMALL LETTER U WITH MACRON
to_title(16#016B)->
    16#016A;
%% LATIN SMALL LETTER U WITH BREVE
to_title(16#016D)->
    16#016C;
%% LATIN SMALL LETTER U WITH RING ABOVE
to_title(16#016F)->
    16#016E;
%% LATIN SMALL LETTER U WITH DOUBLE ACUTE
to_title(16#0171)->
    16#0170;
%% LATIN SMALL LETTER U WITH OGONEK
to_title(16#0173)->
    16#0172;
%% LATIN SMALL LETTER W WITH CIRCUMFLEX
to_title(16#0175)->
    16#0174;
%% LATIN SMALL LETTER Y WITH CIRCUMFLEX
to_title(16#0177)->
    16#0176;
%% LATIN SMALL LETTER Z WITH ACUTE
to_title(16#017A)->
    16#0179;
%% LATIN SMALL LETTER Z WITH DOT ABOVE
to_title(16#017C)->
    16#017B;
%% LATIN SMALL LETTER Z WITH CARON
to_title(16#017E)->
    16#017D;
%% LATIN SMALL LETTER LONG S
to_title(16#017F)->
    16#0053;
%% LATIN SMALL LETTER B WITH STROKE
to_title(16#0180)->
    16#0243;
%% LATIN SMALL LETTER B WITH TOPBAR
to_title(16#0183)->
    16#0182;
%% LATIN SMALL LETTER TONE SIX
to_title(16#0185)->
    16#0184;
%% LATIN SMALL LETTER C WITH HOOK
to_title(16#0188)->
    16#0187;
%% LATIN SMALL LETTER D WITH TOPBAR
to_title(16#018C)->
    16#018B;
%% LATIN SMALL LETTER F WITH HOOK
to_title(16#0192)->
    16#0191;
%% LATIN SMALL LETTER HV
to_title(16#0195)->
    16#01F6;
%% LATIN SMALL LETTER K WITH HOOK
to_title(16#0199)->
    16#0198;
%% LATIN SMALL LETTER L WITH BAR
to_title(16#019A)->
    16#023D;
%% LATIN SMALL LETTER N WITH LONG RIGHT LEG
to_title(16#019E)->
    16#0220;
%% LATIN SMALL LETTER O WITH HORN
to_title(16#01A1)->
    16#01A0;
%% LATIN SMALL LETTER OI
to_title(16#01A3)->
    16#01A2;
%% LATIN SMALL LETTER P WITH HOOK
to_title(16#01A5)->
    16#01A4;
%% LATIN SMALL LETTER TONE TWO
to_title(16#01A8)->
    16#01A7;
%% LATIN SMALL LETTER T WITH HOOK
to_title(16#01AD)->
    16#01AC;
%% LATIN SMALL LETTER U WITH HORN
to_title(16#01B0)->
    16#01AF;
%% LATIN SMALL LETTER Y WITH HOOK
to_title(16#01B4)->
    16#01B3;
%% LATIN SMALL LETTER Z WITH STROKE
to_title(16#01B6)->
    16#01B5;
%% LATIN SMALL LETTER EZH REVERSED
to_title(16#01B9)->
    16#01B8;
%% LATIN SMALL LETTER TONE FIVE
to_title(16#01BD)->
    16#01BC;
%% LATIN LETTER WYNN
to_title(16#01BF)->
    16#01F7;
%% LATIN CAPITAL LETTER DZ WITH CARON
to_title(16#01C4)->
    16#01C5;
%% LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON
to_title(16#01C5)->
    16#01C5;
%% LATIN SMALL LETTER DZ WITH CARON
to_title(16#01C6)->
    16#01C5;
%% LATIN CAPITAL LETTER LJ
to_title(16#01C7)->
    16#01C8;
%% LATIN CAPITAL LETTER L WITH SMALL LETTER J
to_title(16#01C8)->
    16#01C8;
%% LATIN SMALL LETTER LJ
to_title(16#01C9)->
    16#01C8;
%% LATIN CAPITAL LETTER NJ
to_title(16#01CA)->
    16#01CB;
%% LATIN CAPITAL LETTER N WITH SMALL LETTER J
to_title(16#01CB)->
    16#01CB;
%% LATIN SMALL LETTER NJ
to_title(16#01CC)->
    16#01CB;
%% LATIN SMALL LETTER A WITH CARON
to_title(16#01CE)->
    16#01CD;
%% LATIN SMALL LETTER I WITH CARON
to_title(16#01D0)->
    16#01CF;
%% LATIN SMALL LETTER O WITH CARON
to_title(16#01D2)->
    16#01D1;
%% LATIN SMALL LETTER U WITH CARON
to_title(16#01D4)->
    16#01D3;
%% LATIN SMALL LETTER U WITH DIAERESIS AND MACRON
to_title(16#01D6)->
    16#01D5;
%% LATIN SMALL LETTER U WITH DIAERESIS AND ACUTE
to_title(16#01D8)->
    16#01D7;
%% LATIN SMALL LETTER U WITH DIAERESIS AND CARON
to_title(16#01DA)->
    16#01D9;
%% LATIN SMALL LETTER U WITH DIAERESIS AND GRAVE
to_title(16#01DC)->
    16#01DB;
%% LATIN SMALL LETTER TURNED E
to_title(16#01DD)->
    16#018E;
%% LATIN SMALL LETTER A WITH DIAERESIS AND MACRON
to_title(16#01DF)->
    16#01DE;
%% LATIN SMALL LETTER A WITH DOT ABOVE AND MACRON
to_title(16#01E1)->
    16#01E0;
%% LATIN SMALL LETTER AE WITH MACRON
to_title(16#01E3)->
    16#01E2;
%% LATIN SMALL LETTER G WITH STROKE
to_title(16#01E5)->
    16#01E4;
%% LATIN SMALL LETTER G WITH CARON
to_title(16#01E7)->
    16#01E6;
%% LATIN SMALL LETTER K WITH CARON
to_title(16#01E9)->
    16#01E8;
%% LATIN SMALL LETTER O WITH OGONEK
to_title(16#01EB)->
    16#01EA;
%% LATIN SMALL LETTER O WITH OGONEK AND MACRON
to_title(16#01ED)->
    16#01EC;
%% LATIN SMALL LETTER EZH WITH CARON
to_title(16#01EF)->
    16#01EE;
%% LATIN CAPITAL LETTER DZ
to_title(16#01F1)->
    16#01F2;
%% LATIN CAPITAL LETTER D WITH SMALL LETTER Z
to_title(16#01F2)->
    16#01F2;
%% LATIN SMALL LETTER DZ
to_title(16#01F3)->
    16#01F2;
%% LATIN SMALL LETTER G WITH ACUTE
to_title(16#01F5)->
    16#01F4;
%% LATIN SMALL LETTER N WITH GRAVE
to_title(16#01F9)->
    16#01F8;
%% LATIN SMALL LETTER A WITH RING ABOVE AND ACUTE
to_title(16#01FB)->
    16#01FA;
%% LATIN SMALL LETTER AE WITH ACUTE
to_title(16#01FD)->
    16#01FC;
%% LATIN SMALL LETTER O WITH STROKE AND ACUTE
to_title(16#01FF)->
    16#01FE;
%% LATIN SMALL LETTER A WITH DOUBLE GRAVE
to_title(16#0201)->
    16#0200;
%% LATIN SMALL LETTER A WITH INVERTED BREVE
to_title(16#0203)->
    16#0202;
%% LATIN SMALL LETTER E WITH DOUBLE GRAVE
to_title(16#0205)->
    16#0204;
%% LATIN SMALL LETTER E WITH INVERTED BREVE
to_title(16#0207)->
    16#0206;
%% LATIN SMALL LETTER I WITH DOUBLE GRAVE
to_title(16#0209)->
    16#0208;
%% LATIN SMALL LETTER I WITH INVERTED BREVE
to_title(16#020B)->
    16#020A;
%% LATIN SMALL LETTER O WITH DOUBLE GRAVE
to_title(16#020D)->
    16#020C;
%% LATIN SMALL LETTER O WITH INVERTED BREVE
to_title(16#020F)->
    16#020E;
%% LATIN SMALL LETTER R WITH DOUBLE GRAVE
to_title(16#0211)->
    16#0210;
%% LATIN SMALL LETTER R WITH INVERTED BREVE
to_title(16#0213)->
    16#0212;
%% LATIN SMALL LETTER U WITH DOUBLE GRAVE
to_title(16#0215)->
    16#0214;
%% LATIN SMALL LETTER U WITH INVERTED BREVE
to_title(16#0217)->
    16#0216;
%% LATIN SMALL LETTER S WITH COMMA BELOW
to_title(16#0219)->
    16#0218;
%% LATIN SMALL LETTER T WITH COMMA BELOW
to_title(16#021B)->
    16#021A;
%% LATIN SMALL LETTER YOGH
to_title(16#021D)->
    16#021C;
%% LATIN SMALL LETTER H WITH CARON
to_title(16#021F)->
    16#021E;
%% LATIN SMALL LETTER OU
to_title(16#0223)->
    16#0222;
%% LATIN SMALL LETTER Z WITH HOOK
to_title(16#0225)->
    16#0224;
%% LATIN SMALL LETTER A WITH DOT ABOVE
to_title(16#0227)->
    16#0226;
%% LATIN SMALL LETTER E WITH CEDILLA
to_title(16#0229)->
    16#0228;
%% LATIN SMALL LETTER O WITH DIAERESIS AND MACRON
to_title(16#022B)->
    16#022A;
%% LATIN SMALL LETTER O WITH TILDE AND MACRON
to_title(16#022D)->
    16#022C;
%% LATIN SMALL LETTER O WITH DOT ABOVE
to_title(16#022F)->
    16#022E;
%% LATIN SMALL LETTER O WITH DOT ABOVE AND MACRON
to_title(16#0231)->
    16#0230;
%% LATIN SMALL LETTER Y WITH MACRON
to_title(16#0233)->
    16#0232;
%% LATIN SMALL LETTER C WITH STROKE
to_title(16#023C)->
    16#023B;
%% LATIN SMALL LETTER S WITH SWASH TAIL
to_title(16#023F)->
    16#2C7E;
%% LATIN SMALL LETTER Z WITH SWASH TAIL
to_title(16#0240)->
    16#2C7F;
%% LATIN SMALL LETTER GLOTTAL STOP
to_title(16#0242)->
    16#0241;
%% LATIN SMALL LETTER E WITH STROKE
to_title(16#0247)->
    16#0246;
%% LATIN SMALL LETTER J WITH STROKE
to_title(16#0249)->
    16#0248;
%% LATIN SMALL LETTER Q WITH HOOK TAIL
to_title(16#024B)->
    16#024A;
%% LATIN SMALL LETTER R WITH STROKE
to_title(16#024D)->
    16#024C;
%% LATIN SMALL LETTER Y WITH STROKE
to_title(16#024F)->
    16#024E;
%% LATIN SMALL LETTER TURNED A
to_title(16#0250)->
    16#2C6F;
%% LATIN SMALL LETTER ALPHA
to_title(16#0251)->
    16#2C6D;
%% LATIN SMALL LETTER TURNED ALPHA
to_title(16#0252)->
    16#2C70;
%% LATIN SMALL LETTER B WITH HOOK
to_title(16#0253)->
    16#0181;
%% LATIN SMALL LETTER OPEN O
to_title(16#0254)->
    16#0186;
%% LATIN SMALL LETTER D WITH TAIL
to_title(16#0256)->
    16#0189;
%% LATIN SMALL LETTER D WITH HOOK
to_title(16#0257)->
    16#018A;
%% LATIN SMALL LETTER SCHWA
to_title(16#0259)->
    16#018F;
%% LATIN SMALL LETTER OPEN E
to_title(16#025B)->
    16#0190;
%% LATIN SMALL LETTER G WITH HOOK
to_title(16#0260)->
    16#0193;
%% LATIN SMALL LETTER GAMMA
to_title(16#0263)->
    16#0194;
%% LATIN SMALL LETTER TURNED H
to_title(16#0265)->
    16#A78D;
%% LATIN SMALL LETTER H WITH HOOK
to_title(16#0266)->
    16#A7AA;
%% LATIN SMALL LETTER I WITH STROKE
to_title(16#0268)->
    16#0197;
%% LATIN SMALL LETTER IOTA
to_title(16#0269)->
    16#0196;
%% LATIN SMALL LETTER L WITH MIDDLE TILDE
to_title(16#026B)->
    16#2C62;
%% LATIN SMALL LETTER TURNED M
to_title(16#026F)->
    16#019C;
%% LATIN SMALL LETTER M WITH HOOK
to_title(16#0271)->
    16#2C6E;
%% LATIN SMALL LETTER N WITH LEFT HOOK
to_title(16#0272)->
    16#019D;
%% LATIN SMALL LETTER BARRED O
to_title(16#0275)->
    16#019F;
%% LATIN SMALL LETTER R WITH TAIL
to_title(16#027D)->
    16#2C64;
%% LATIN LETTER SMALL CAPITAL R
to_title(16#0280)->
    16#01A6;
%% LATIN SMALL LETTER ESH
to_title(16#0283)->
    16#01A9;
%% LATIN SMALL LETTER T WITH RETROFLEX HOOK
to_title(16#0288)->
    16#01AE;
%% LATIN SMALL LETTER U BAR
to_title(16#0289)->
    16#0244;
%% LATIN SMALL LETTER UPSILON
to_title(16#028A)->
    16#01B1;
%% LATIN SMALL LETTER V WITH HOOK
to_title(16#028B)->
    16#01B2;
%% LATIN SMALL LETTER TURNED V
to_title(16#028C)->
    16#0245;
%% LATIN SMALL LETTER EZH
to_title(16#0292)->
    16#01B7;
%% COMBINING GREEK YPOGEGRAMMENI
to_title(16#0345)->
    16#0399;
%% GREEK SMALL LETTER HETA
to_title(16#0371)->
    16#0370;
%% GREEK SMALL LETTER ARCHAIC SAMPI
to_title(16#0373)->
    16#0372;
%% GREEK SMALL LETTER PAMPHYLIAN DIGAMMA
to_title(16#0377)->
    16#0376;
%% GREEK SMALL REVERSED LUNATE SIGMA SYMBOL
to_title(16#037B)->
    16#03FD;
%% GREEK SMALL DOTTED LUNATE SIGMA SYMBOL
to_title(16#037C)->
    16#03FE;
%% GREEK SMALL REVERSED DOTTED LUNATE SIGMA SYMBOL
to_title(16#037D)->
    16#03FF;
%% GREEK SMALL LETTER ALPHA WITH TONOS
to_title(16#03AC)->
    16#0386;
%% GREEK SMALL LETTER EPSILON WITH TONOS
to_title(16#03AD)->
    16#0388;
%% GREEK SMALL LETTER ETA WITH TONOS
to_title(16#03AE)->
    16#0389;
%% GREEK SMALL LETTER IOTA WITH TONOS
to_title(16#03AF)->
    16#038A;
%% GREEK SMALL LETTER ALPHA
to_title(16#03B1)->
    16#0391;
%% GREEK SMALL LETTER BETA
to_title(16#03B2)->
    16#0392;
%% GREEK SMALL LETTER GAMMA
to_title(16#03B3)->
    16#0393;
%% GREEK SMALL LETTER DELTA
to_title(16#03B4)->
    16#0394;
%% GREEK SMALL LETTER EPSILON
to_title(16#03B5)->
    16#0395;
%% GREEK SMALL LETTER ZETA
to_title(16#03B6)->
    16#0396;
%% GREEK SMALL LETTER ETA
to_title(16#03B7)->
    16#0397;
%% GREEK SMALL LETTER THETA
to_title(16#03B8)->
    16#0398;
%% GREEK SMALL LETTER IOTA
to_title(16#03B9)->
    16#0399;
%% GREEK SMALL LETTER KAPPA
to_title(16#03BA)->
    16#039A;
%% GREEK SMALL LETTER LAMDA
to_title(16#03BB)->
    16#039B;
%% GREEK SMALL LETTER MU
to_title(16#03BC)->
    16#039C;
%% GREEK SMALL LETTER NU
to_title(16#03BD)->
    16#039D;
%% GREEK SMALL LETTER XI
to_title(16#03BE)->
    16#039E;
%% GREEK SMALL LETTER OMICRON
to_title(16#03BF)->
    16#039F;
%% GREEK SMALL LETTER PI
to_title(16#03C0)->
    16#03A0;
%% GREEK SMALL LETTER RHO
to_title(16#03C1)->
    16#03A1;
%% GREEK SMALL LETTER FINAL SIGMA
to_title(16#03C2)->
    16#03A3;
%% GREEK SMALL LETTER SIGMA
to_title(16#03C3)->
    16#03A3;
%% GREEK SMALL LETTER TAU
to_title(16#03C4)->
    16#03A4;
%% GREEK SMALL LETTER UPSILON
to_title(16#03C5)->
    16#03A5;
%% GREEK SMALL LETTER PHI
to_title(16#03C6)->
    16#03A6;
%% GREEK SMALL LETTER CHI
to_title(16#03C7)->
    16#03A7;
%% GREEK SMALL LETTER PSI
to_title(16#03C8)->
    16#03A8;
%% GREEK SMALL LETTER OMEGA
to_title(16#03C9)->
    16#03A9;
%% GREEK SMALL LETTER IOTA WITH DIALYTIKA
to_title(16#03CA)->
    16#03AA;
%% GREEK SMALL LETTER UPSILON WITH DIALYTIKA
to_title(16#03CB)->
    16#03AB;
%% GREEK SMALL LETTER OMICRON WITH TONOS
to_title(16#03CC)->
    16#038C;
%% GREEK SMALL LETTER UPSILON WITH TONOS
to_title(16#03CD)->
    16#038E;
%% GREEK SMALL LETTER OMEGA WITH TONOS
to_title(16#03CE)->
    16#038F;
%% GREEK BETA SYMBOL
to_title(16#03D0)->
    16#0392;
%% GREEK THETA SYMBOL
to_title(16#03D1)->
    16#0398;
%% GREEK PHI SYMBOL
to_title(16#03D5)->
    16#03A6;
%% GREEK PI SYMBOL
to_title(16#03D6)->
    16#03A0;
%% GREEK KAI SYMBOL
to_title(16#03D7)->
    16#03CF;
%% GREEK SMALL LETTER ARCHAIC KOPPA
to_title(16#03D9)->
    16#03D8;
%% GREEK SMALL LETTER STIGMA
to_title(16#03DB)->
    16#03DA;
%% GREEK SMALL LETTER DIGAMMA
to_title(16#03DD)->
    16#03DC;
%% GREEK SMALL LETTER KOPPA
to_title(16#03DF)->
    16#03DE;
%% GREEK SMALL LETTER SAMPI
to_title(16#03E1)->
    16#03E0;
%% COPTIC SMALL LETTER SHEI
to_title(16#03E3)->
    16#03E2;
%% COPTIC SMALL LETTER FEI
to_title(16#03E5)->
    16#03E4;
%% COPTIC SMALL LETTER KHEI
to_title(16#03E7)->
    16#03E6;
%% COPTIC SMALL LETTER HORI
to_title(16#03E9)->
    16#03E8;
%% COPTIC SMALL LETTER GANGIA
to_title(16#03EB)->
    16#03EA;
%% COPTIC SMALL LETTER SHIMA
to_title(16#03ED)->
    16#03EC;
%% COPTIC SMALL LETTER DEI
to_title(16#03EF)->
    16#03EE;
%% GREEK KAPPA SYMBOL
to_title(16#03F0)->
    16#039A;
%% GREEK RHO SYMBOL
to_title(16#03F1)->
    16#03A1;
%% GREEK LUNATE SIGMA SYMBOL
to_title(16#03F2)->
    16#03F9;
%% GREEK LUNATE EPSILON SYMBOL
to_title(16#03F5)->
    16#0395;
%% GREEK SMALL LETTER SHO
to_title(16#03F8)->
    16#03F7;
%% GREEK SMALL LETTER SAN
to_title(16#03FB)->
    16#03FA;
%% CYRILLIC SMALL LETTER A
to_title(16#0430)->
    16#0410;
%% CYRILLIC SMALL LETTER BE
to_title(16#0431)->
    16#0411;
%% CYRILLIC SMALL LETTER VE
to_title(16#0432)->
    16#0412;
%% CYRILLIC SMALL LETTER GHE
to_title(16#0433)->
    16#0413;
%% CYRILLIC SMALL LETTER DE
to_title(16#0434)->
    16#0414;
%% CYRILLIC SMALL LETTER IE
to_title(16#0435)->
    16#0415;
%% CYRILLIC SMALL LETTER ZHE
to_title(16#0436)->
    16#0416;
%% CYRILLIC SMALL LETTER ZE
to_title(16#0437)->
    16#0417;
%% CYRILLIC SMALL LETTER I
to_title(16#0438)->
    16#0418;
%% CYRILLIC SMALL LETTER SHORT I
to_title(16#0439)->
    16#0419;
%% CYRILLIC SMALL LETTER KA
to_title(16#043A)->
    16#041A;
%% CYRILLIC SMALL LETTER EL
to_title(16#043B)->
    16#041B;
%% CYRILLIC SMALL LETTER EM
to_title(16#043C)->
    16#041C;
%% CYRILLIC SMALL LETTER EN
to_title(16#043D)->
    16#041D;
%% CYRILLIC SMALL LETTER O
to_title(16#043E)->
    16#041E;
%% CYRILLIC SMALL LETTER PE
to_title(16#043F)->
    16#041F;
%% CYRILLIC SMALL LETTER ER
to_title(16#0440)->
    16#0420;
%% CYRILLIC SMALL LETTER ES
to_title(16#0441)->
    16#0421;
%% CYRILLIC SMALL LETTER TE
to_title(16#0442)->
    16#0422;
%% CYRILLIC SMALL LETTER U
to_title(16#0443)->
    16#0423;
%% CYRILLIC SMALL LETTER EF
to_title(16#0444)->
    16#0424;
%% CYRILLIC SMALL LETTER HA
to_title(16#0445)->
    16#0425;
%% CYRILLIC SMALL LETTER TSE
to_title(16#0446)->
    16#0426;
%% CYRILLIC SMALL LETTER CHE
to_title(16#0447)->
    16#0427;
%% CYRILLIC SMALL LETTER SHA
to_title(16#0448)->
    16#0428;
%% CYRILLIC SMALL LETTER SHCHA
to_title(16#0449)->
    16#0429;
%% CYRILLIC SMALL LETTER HARD SIGN
to_title(16#044A)->
    16#042A;
%% CYRILLIC SMALL LETTER YERU
to_title(16#044B)->
    16#042B;
%% CYRILLIC SMALL LETTER SOFT SIGN
to_title(16#044C)->
    16#042C;
%% CYRILLIC SMALL LETTER E
to_title(16#044D)->
    16#042D;
%% CYRILLIC SMALL LETTER YU
to_title(16#044E)->
    16#042E;
%% CYRILLIC SMALL LETTER YA
to_title(16#044F)->
    16#042F;
%% CYRILLIC SMALL LETTER IE WITH GRAVE
to_title(16#0450)->
    16#0400;
%% CYRILLIC SMALL LETTER IO
to_title(16#0451)->
    16#0401;
%% CYRILLIC SMALL LETTER DJE
to_title(16#0452)->
    16#0402;
%% CYRILLIC SMALL LETTER GJE
to_title(16#0453)->
    16#0403;
%% CYRILLIC SMALL LETTER UKRAINIAN IE
to_title(16#0454)->
    16#0404;
%% CYRILLIC SMALL LETTER DZE
to_title(16#0455)->
    16#0405;
%% CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
to_title(16#0456)->
    16#0406;
%% CYRILLIC SMALL LETTER YI
to_title(16#0457)->
    16#0407;
%% CYRILLIC SMALL LETTER JE
to_title(16#0458)->
    16#0408;
%% CYRILLIC SMALL LETTER LJE
to_title(16#0459)->
    16#0409;
%% CYRILLIC SMALL LETTER NJE
to_title(16#045A)->
    16#040A;
%% CYRILLIC SMALL LETTER TSHE
to_title(16#045B)->
    16#040B;
%% CYRILLIC SMALL LETTER KJE
to_title(16#045C)->
    16#040C;
%% CYRILLIC SMALL LETTER I WITH GRAVE
to_title(16#045D)->
    16#040D;
%% CYRILLIC SMALL LETTER SHORT U
to_title(16#045E)->
    16#040E;
%% CYRILLIC SMALL LETTER DZHE
to_title(16#045F)->
    16#040F;
%% CYRILLIC SMALL LETTER OMEGA
to_title(16#0461)->
    16#0460;
%% CYRILLIC SMALL LETTER YAT
to_title(16#0463)->
    16#0462;
%% CYRILLIC SMALL LETTER IOTIFIED E
to_title(16#0465)->
    16#0464;
%% CYRILLIC SMALL LETTER LITTLE YUS
to_title(16#0467)->
    16#0466;
%% CYRILLIC SMALL LETTER IOTIFIED LITTLE YUS
to_title(16#0469)->
    16#0468;
%% CYRILLIC SMALL LETTER BIG YUS
to_title(16#046B)->
    16#046A;
%% CYRILLIC SMALL LETTER IOTIFIED BIG YUS
to_title(16#046D)->
    16#046C;
%% CYRILLIC SMALL LETTER KSI
to_title(16#046F)->
    16#046E;
%% CYRILLIC SMALL LETTER PSI
to_title(16#0471)->
    16#0470;
%% CYRILLIC SMALL LETTER FITA
to_title(16#0473)->
    16#0472;
%% CYRILLIC SMALL LETTER IZHITSA
to_title(16#0475)->
    16#0474;
%% CYRILLIC SMALL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT
to_title(16#0477)->
    16#0476;
%% CYRILLIC SMALL LETTER UK
to_title(16#0479)->
    16#0478;
%% CYRILLIC SMALL LETTER ROUND OMEGA
to_title(16#047B)->
    16#047A;
%% CYRILLIC SMALL LETTER OMEGA WITH TITLO
to_title(16#047D)->
    16#047C;
%% CYRILLIC SMALL LETTER OT
to_title(16#047F)->
    16#047E;
%% CYRILLIC SMALL LETTER KOPPA
to_title(16#0481)->
    16#0480;
%% CYRILLIC SMALL LETTER SHORT I WITH TAIL
to_title(16#048B)->
    16#048A;
%% CYRILLIC SMALL LETTER SEMISOFT SIGN
to_title(16#048D)->
    16#048C;
%% CYRILLIC SMALL LETTER ER WITH TICK
to_title(16#048F)->
    16#048E;
%% CYRILLIC SMALL LETTER GHE WITH UPTURN
to_title(16#0491)->
    16#0490;
%% CYRILLIC SMALL LETTER GHE WITH STROKE
to_title(16#0493)->
    16#0492;
%% CYRILLIC SMALL LETTER GHE WITH MIDDLE HOOK
to_title(16#0495)->
    16#0494;
%% CYRILLIC SMALL LETTER ZHE WITH DESCENDER
to_title(16#0497)->
    16#0496;
%% CYRILLIC SMALL LETTER ZE WITH DESCENDER
to_title(16#0499)->
    16#0498;
%% CYRILLIC SMALL LETTER KA WITH DESCENDER
to_title(16#049B)->
    16#049A;
%% CYRILLIC SMALL LETTER KA WITH VERTICAL STROKE
to_title(16#049D)->
    16#049C;
%% CYRILLIC SMALL LETTER KA WITH STROKE
to_title(16#049F)->
    16#049E;
%% CYRILLIC SMALL LETTER BASHKIR KA
to_title(16#04A1)->
    16#04A0;
%% CYRILLIC SMALL LETTER EN WITH DESCENDER
to_title(16#04A3)->
    16#04A2;
%% CYRILLIC SMALL LIGATURE EN GHE
to_title(16#04A5)->
    16#04A4;
%% CYRILLIC SMALL LETTER PE WITH MIDDLE HOOK
to_title(16#04A7)->
    16#04A6;
%% CYRILLIC SMALL LETTER ABKHASIAN HA
to_title(16#04A9)->
    16#04A8;
%% CYRILLIC SMALL LETTER ES WITH DESCENDER
to_title(16#04AB)->
    16#04AA;
%% CYRILLIC SMALL LETTER TE WITH DESCENDER
to_title(16#04AD)->
    16#04AC;
%% CYRILLIC SMALL LETTER STRAIGHT U
to_title(16#04AF)->
    16#04AE;
%% CYRILLIC SMALL LETTER STRAIGHT U WITH STROKE
to_title(16#04B1)->
    16#04B0;
%% CYRILLIC SMALL LETTER HA WITH DESCENDER
to_title(16#04B3)->
    16#04B2;
%% CYRILLIC SMALL LIGATURE TE TSE
to_title(16#04B5)->
    16#04B4;
%% CYRILLIC SMALL LETTER CHE WITH DESCENDER
to_title(16#04B7)->
    16#04B6;
%% CYRILLIC SMALL LETTER CHE WITH VERTICAL STROKE
to_title(16#04B9)->
    16#04B8;
%% CYRILLIC SMALL LETTER SHHA
to_title(16#04BB)->
    16#04BA;
%% CYRILLIC SMALL LETTER ABKHASIAN CHE
to_title(16#04BD)->
    16#04BC;
%% CYRILLIC SMALL LETTER ABKHASIAN CHE WITH DESCENDER
to_title(16#04BF)->
    16#04BE;
%% CYRILLIC SMALL LETTER ZHE WITH BREVE
to_title(16#04C2)->
    16#04C1;
%% CYRILLIC SMALL LETTER KA WITH HOOK
to_title(16#04C4)->
    16#04C3;
%% CYRILLIC SMALL LETTER EL WITH TAIL
to_title(16#04C6)->
    16#04C5;
%% CYRILLIC SMALL LETTER EN WITH HOOK
to_title(16#04C8)->
    16#04C7;
%% CYRILLIC SMALL LETTER EN WITH TAIL
to_title(16#04CA)->
    16#04C9;
%% CYRILLIC SMALL LETTER KHAKASSIAN CHE
to_title(16#04CC)->
    16#04CB;
%% CYRILLIC SMALL LETTER EM WITH TAIL
to_title(16#04CE)->
    16#04CD;
%% CYRILLIC SMALL LETTER PALOCHKA
to_title(16#04CF)->
    16#04C0;
%% CYRILLIC SMALL LETTER A WITH BREVE
to_title(16#04D1)->
    16#04D0;
%% CYRILLIC SMALL LETTER A WITH DIAERESIS
to_title(16#04D3)->
    16#04D2;
%% CYRILLIC SMALL LIGATURE A IE
to_title(16#04D5)->
    16#04D4;
%% CYRILLIC SMALL LETTER IE WITH BREVE
to_title(16#04D7)->
    16#04D6;
%% CYRILLIC SMALL LETTER SCHWA
to_title(16#04D9)->
    16#04D8;
%% CYRILLIC SMALL LETTER SCHWA WITH DIAERESIS
to_title(16#04DB)->
    16#04DA;
%% CYRILLIC SMALL LETTER ZHE WITH DIAERESIS
to_title(16#04DD)->
    16#04DC;
%% CYRILLIC SMALL LETTER ZE WITH DIAERESIS
to_title(16#04DF)->
    16#04DE;
%% CYRILLIC SMALL LETTER ABKHASIAN DZE
to_title(16#04E1)->
    16#04E0;
%% CYRILLIC SMALL LETTER I WITH MACRON
to_title(16#04E3)->
    16#04E2;
%% CYRILLIC SMALL LETTER I WITH DIAERESIS
to_title(16#04E5)->
    16#04E4;
%% CYRILLIC SMALL LETTER O WITH DIAERESIS
to_title(16#04E7)->
    16#04E6;
%% CYRILLIC SMALL LETTER BARRED O
to_title(16#04E9)->
    16#04E8;
%% CYRILLIC SMALL LETTER BARRED O WITH DIAERESIS
to_title(16#04EB)->
    16#04EA;
%% CYRILLIC SMALL LETTER E WITH DIAERESIS
to_title(16#04ED)->
    16#04EC;
%% CYRILLIC SMALL LETTER U WITH MACRON
to_title(16#04EF)->
    16#04EE;
%% CYRILLIC SMALL LETTER U WITH DIAERESIS
to_title(16#04F1)->
    16#04F0;
%% CYRILLIC SMALL LETTER U WITH DOUBLE ACUTE
to_title(16#04F3)->
    16#04F2;
%% CYRILLIC SMALL LETTER CHE WITH DIAERESIS
to_title(16#04F5)->
    16#04F4;
%% CYRILLIC SMALL LETTER GHE WITH DESCENDER
to_title(16#04F7)->
    16#04F6;
%% CYRILLIC SMALL LETTER YERU WITH DIAERESIS
to_title(16#04F9)->
    16#04F8;
%% CYRILLIC SMALL LETTER GHE WITH STROKE AND HOOK
to_title(16#04FB)->
    16#04FA;
%% CYRILLIC SMALL LETTER HA WITH HOOK
to_title(16#04FD)->
    16#04FC;
%% CYRILLIC SMALL LETTER HA WITH STROKE
to_title(16#04FF)->
    16#04FE;
%% CYRILLIC SMALL LETTER KOMI DE
to_title(16#0501)->
    16#0500;
%% CYRILLIC SMALL LETTER KOMI DJE
to_title(16#0503)->
    16#0502;
%% CYRILLIC SMALL LETTER KOMI ZJE
to_title(16#0505)->
    16#0504;
%% CYRILLIC SMALL LETTER KOMI DZJE
to_title(16#0507)->
    16#0506;
%% CYRILLIC SMALL LETTER KOMI LJE
to_title(16#0509)->
    16#0508;
%% CYRILLIC SMALL LETTER KOMI NJE
to_title(16#050B)->
    16#050A;
%% CYRILLIC SMALL LETTER KOMI SJE
to_title(16#050D)->
    16#050C;
%% CYRILLIC SMALL LETTER KOMI TJE
to_title(16#050F)->
    16#050E;
%% CYRILLIC SMALL LETTER REVERSED ZE
to_title(16#0511)->
    16#0510;
%% CYRILLIC SMALL LETTER EL WITH HOOK
to_title(16#0513)->
    16#0512;
%% CYRILLIC SMALL LETTER LHA
to_title(16#0515)->
    16#0514;
%% CYRILLIC SMALL LETTER RHA
to_title(16#0517)->
    16#0516;
%% CYRILLIC SMALL LETTER YAE
to_title(16#0519)->
    16#0518;
%% CYRILLIC SMALL LETTER QA
to_title(16#051B)->
    16#051A;
%% CYRILLIC SMALL LETTER WE
to_title(16#051D)->
    16#051C;
%% CYRILLIC SMALL LETTER ALEUT KA
to_title(16#051F)->
    16#051E;
%% CYRILLIC SMALL LETTER EL WITH MIDDLE HOOK
to_title(16#0521)->
    16#0520;
%% CYRILLIC SMALL LETTER EN WITH MIDDLE HOOK
to_title(16#0523)->
    16#0522;
%% CYRILLIC SMALL LETTER PE WITH DESCENDER
to_title(16#0525)->
    16#0524;
%% CYRILLIC SMALL LETTER SHHA WITH DESCENDER
to_title(16#0527)->
    16#0526;
%% ARMENIAN SMALL LETTER AYB
to_title(16#0561)->
    16#0531;
%% ARMENIAN SMALL LETTER BEN
to_title(16#0562)->
    16#0532;
%% ARMENIAN SMALL LETTER GIM
to_title(16#0563)->
    16#0533;
%% ARMENIAN SMALL LETTER DA
to_title(16#0564)->
    16#0534;
%% ARMENIAN SMALL LETTER ECH
to_title(16#0565)->
    16#0535;
%% ARMENIAN SMALL LETTER ZA
to_title(16#0566)->
    16#0536;
%% ARMENIAN SMALL LETTER EH
to_title(16#0567)->
    16#0537;
%% ARMENIAN SMALL LETTER ET
to_title(16#0568)->
    16#0538;
%% ARMENIAN SMALL LETTER TO
to_title(16#0569)->
    16#0539;
%% ARMENIAN SMALL LETTER ZHE
to_title(16#056A)->
    16#053A;
%% ARMENIAN SMALL LETTER INI
to_title(16#056B)->
    16#053B;
%% ARMENIAN SMALL LETTER LIWN
to_title(16#056C)->
    16#053C;
%% ARMENIAN SMALL LETTER XEH
to_title(16#056D)->
    16#053D;
%% ARMENIAN SMALL LETTER CA
to_title(16#056E)->
    16#053E;
%% ARMENIAN SMALL LETTER KEN
to_title(16#056F)->
    16#053F;
%% ARMENIAN SMALL LETTER HO
to_title(16#0570)->
    16#0540;
%% ARMENIAN SMALL LETTER JA
to_title(16#0571)->
    16#0541;
%% ARMENIAN SMALL LETTER GHAD
to_title(16#0572)->
    16#0542;
%% ARMENIAN SMALL LETTER CHEH
to_title(16#0573)->
    16#0543;
%% ARMENIAN SMALL LETTER MEN
to_title(16#0574)->
    16#0544;
%% ARMENIAN SMALL LETTER YI
to_title(16#0575)->
    16#0545;
%% ARMENIAN SMALL LETTER NOW
to_title(16#0576)->
    16#0546;
%% ARMENIAN SMALL LETTER SHA
to_title(16#0577)->
    16#0547;
%% ARMENIAN SMALL LETTER VO
to_title(16#0578)->
    16#0548;
%% ARMENIAN SMALL LETTER CHA
to_title(16#0579)->
    16#0549;
%% ARMENIAN SMALL LETTER PEH
to_title(16#057A)->
    16#054A;
%% ARMENIAN SMALL LETTER JHEH
to_title(16#057B)->
    16#054B;
%% ARMENIAN SMALL LETTER RA
to_title(16#057C)->
    16#054C;
%% ARMENIAN SMALL LETTER SEH
to_title(16#057D)->
    16#054D;
%% ARMENIAN SMALL LETTER VEW
to_title(16#057E)->
    16#054E;
%% ARMENIAN SMALL LETTER TIWN
to_title(16#057F)->
    16#054F;
%% ARMENIAN SMALL LETTER REH
to_title(16#0580)->
    16#0550;
%% ARMENIAN SMALL LETTER CO
to_title(16#0581)->
    16#0551;
%% ARMENIAN SMALL LETTER YIWN
to_title(16#0582)->
    16#0552;
%% ARMENIAN SMALL LETTER PIWR
to_title(16#0583)->
    16#0553;
%% ARMENIAN SMALL LETTER KEH
to_title(16#0584)->
    16#0554;
%% ARMENIAN SMALL LETTER OH
to_title(16#0585)->
    16#0555;
%% ARMENIAN SMALL LETTER FEH
to_title(16#0586)->
    16#0556;
%% LATIN SMALL LETTER INSULAR G
to_title(16#1D79)->
    16#A77D;
%% LATIN SMALL LETTER P WITH STROKE
to_title(16#1D7D)->
    16#2C63;
%% LATIN SMALL LETTER A WITH RING BELOW
to_title(16#1E01)->
    16#1E00;
%% LATIN SMALL LETTER B WITH DOT ABOVE
to_title(16#1E03)->
    16#1E02;
%% LATIN SMALL LETTER B WITH DOT BELOW
to_title(16#1E05)->
    16#1E04;
%% LATIN SMALL LETTER B WITH LINE BELOW
to_title(16#1E07)->
    16#1E06;
%% LATIN SMALL LETTER C WITH CEDILLA AND ACUTE
to_title(16#1E09)->
    16#1E08;
%% LATIN SMALL LETTER D WITH DOT ABOVE
to_title(16#1E0B)->
    16#1E0A;
%% LATIN SMALL LETTER D WITH DOT BELOW
to_title(16#1E0D)->
    16#1E0C;
%% LATIN SMALL LETTER D WITH LINE BELOW
to_title(16#1E0F)->
    16#1E0E;
%% LATIN SMALL LETTER D WITH CEDILLA
to_title(16#1E11)->
    16#1E10;
%% LATIN SMALL LETTER D WITH CIRCUMFLEX BELOW
to_title(16#1E13)->
    16#1E12;
%% LATIN SMALL LETTER E WITH MACRON AND GRAVE
to_title(16#1E15)->
    16#1E14;
%% LATIN SMALL LETTER E WITH MACRON AND ACUTE
to_title(16#1E17)->
    16#1E16;
%% LATIN SMALL LETTER E WITH CIRCUMFLEX BELOW
to_title(16#1E19)->
    16#1E18;
%% LATIN SMALL LETTER E WITH TILDE BELOW
to_title(16#1E1B)->
    16#1E1A;
%% LATIN SMALL LETTER E WITH CEDILLA AND BREVE
to_title(16#1E1D)->
    16#1E1C;
%% LATIN SMALL LETTER F WITH DOT ABOVE
to_title(16#1E1F)->
    16#1E1E;
%% LATIN SMALL LETTER G WITH MACRON
to_title(16#1E21)->
    16#1E20;
%% LATIN SMALL LETTER H WITH DOT ABOVE
to_title(16#1E23)->
    16#1E22;
%% LATIN SMALL LETTER H WITH DOT BELOW
to_title(16#1E25)->
    16#1E24;
%% LATIN SMALL LETTER H WITH DIAERESIS
to_title(16#1E27)->
    16#1E26;
%% LATIN SMALL LETTER H WITH CEDILLA
to_title(16#1E29)->
    16#1E28;
%% LATIN SMALL LETTER H WITH BREVE BELOW
to_title(16#1E2B)->
    16#1E2A;
%% LATIN SMALL LETTER I WITH TILDE BELOW
to_title(16#1E2D)->
    16#1E2C;
%% LATIN SMALL LETTER I WITH DIAERESIS AND ACUTE
to_title(16#1E2F)->
    16#1E2E;
%% LATIN SMALL LETTER K WITH ACUTE
to_title(16#1E31)->
    16#1E30;
%% LATIN SMALL LETTER K WITH DOT BELOW
to_title(16#1E33)->
    16#1E32;
%% LATIN SMALL LETTER K WITH LINE BELOW
to_title(16#1E35)->
    16#1E34;
%% LATIN SMALL LETTER L WITH DOT BELOW
to_title(16#1E37)->
    16#1E36;
%% LATIN SMALL LETTER L WITH DOT BELOW AND MACRON
to_title(16#1E39)->
    16#1E38;
%% LATIN SMALL LETTER L WITH LINE BELOW
to_title(16#1E3B)->
    16#1E3A;
%% LATIN SMALL LETTER L WITH CIRCUMFLEX BELOW
to_title(16#1E3D)->
    16#1E3C;
%% LATIN SMALL LETTER M WITH ACUTE
to_title(16#1E3F)->
    16#1E3E;
%% LATIN SMALL LETTER M WITH DOT ABOVE
to_title(16#1E41)->
    16#1E40;
%% LATIN SMALL LETTER M WITH DOT BELOW
to_title(16#1E43)->
    16#1E42;
%% LATIN SMALL LETTER N WITH DOT ABOVE
to_title(16#1E45)->
    16#1E44;
%% LATIN SMALL LETTER N WITH DOT BELOW
to_title(16#1E47)->
    16#1E46;
%% LATIN SMALL LETTER N WITH LINE BELOW
to_title(16#1E49)->
    16#1E48;
%% LATIN SMALL LETTER N WITH CIRCUMFLEX BELOW
to_title(16#1E4B)->
    16#1E4A;
%% LATIN SMALL LETTER O WITH TILDE AND ACUTE
to_title(16#1E4D)->
    16#1E4C;
%% LATIN SMALL LETTER O WITH TILDE AND DIAERESIS
to_title(16#1E4F)->
    16#1E4E;
%% LATIN SMALL LETTER O WITH MACRON AND GRAVE
to_title(16#1E51)->
    16#1E50;
%% LATIN SMALL LETTER O WITH MACRON AND ACUTE
to_title(16#1E53)->
    16#1E52;
%% LATIN SMALL LETTER P WITH ACUTE
to_title(16#1E55)->
    16#1E54;
%% LATIN SMALL LETTER P WITH DOT ABOVE
to_title(16#1E57)->
    16#1E56;
%% LATIN SMALL LETTER R WITH DOT ABOVE
to_title(16#1E59)->
    16#1E58;
%% LATIN SMALL LETTER R WITH DOT BELOW
to_title(16#1E5B)->
    16#1E5A;
%% LATIN SMALL LETTER R WITH DOT BELOW AND MACRON
to_title(16#1E5D)->
    16#1E5C;
%% LATIN SMALL LETTER R WITH LINE BELOW
to_title(16#1E5F)->
    16#1E5E;
%% LATIN SMALL LETTER S WITH DOT ABOVE
to_title(16#1E61)->
    16#1E60;
%% LATIN SMALL LETTER S WITH DOT BELOW
to_title(16#1E63)->
    16#1E62;
%% LATIN SMALL LETTER S WITH ACUTE AND DOT ABOVE
to_title(16#1E65)->
    16#1E64;
%% LATIN SMALL LETTER S WITH CARON AND DOT ABOVE
to_title(16#1E67)->
    16#1E66;
%% LATIN SMALL LETTER S WITH DOT BELOW AND DOT ABOVE
to_title(16#1E69)->
    16#1E68;
%% LATIN SMALL LETTER T WITH DOT ABOVE
to_title(16#1E6B)->
    16#1E6A;
%% LATIN SMALL LETTER T WITH DOT BELOW
to_title(16#1E6D)->
    16#1E6C;
%% LATIN SMALL LETTER T WITH LINE BELOW
to_title(16#1E6F)->
    16#1E6E;
%% LATIN SMALL LETTER T WITH CIRCUMFLEX BELOW
to_title(16#1E71)->
    16#1E70;
%% LATIN SMALL LETTER U WITH DIAERESIS BELOW
to_title(16#1E73)->
    16#1E72;
%% LATIN SMALL LETTER U WITH TILDE BELOW
to_title(16#1E75)->
    16#1E74;
%% LATIN SMALL LETTER U WITH CIRCUMFLEX BELOW
to_title(16#1E77)->
    16#1E76;
%% LATIN SMALL LETTER U WITH TILDE AND ACUTE
to_title(16#1E79)->
    16#1E78;
%% LATIN SMALL LETTER U WITH MACRON AND DIAERESIS
to_title(16#1E7B)->
    16#1E7A;
%% LATIN SMALL LETTER V WITH TILDE
to_title(16#1E7D)->
    16#1E7C;
%% LATIN SMALL LETTER V WITH DOT BELOW
to_title(16#1E7F)->
    16#1E7E;
%% LATIN SMALL LETTER W WITH GRAVE
to_title(16#1E81)->
    16#1E80;
%% LATIN SMALL LETTER W WITH ACUTE
to_title(16#1E83)->
    16#1E82;
%% LATIN SMALL LETTER W WITH DIAERESIS
to_title(16#1E85)->
    16#1E84;
%% LATIN SMALL LETTER W WITH DOT ABOVE
to_title(16#1E87)->
    16#1E86;
%% LATIN SMALL LETTER W WITH DOT BELOW
to_title(16#1E89)->
    16#1E88;
%% LATIN SMALL LETTER X WITH DOT ABOVE
to_title(16#1E8B)->
    16#1E8A;
%% LATIN SMALL LETTER X WITH DIAERESIS
to_title(16#1E8D)->
    16#1E8C;
%% LATIN SMALL LETTER Y WITH DOT ABOVE
to_title(16#1E8F)->
    16#1E8E;
%% LATIN SMALL LETTER Z WITH CIRCUMFLEX
to_title(16#1E91)->
    16#1E90;
%% LATIN SMALL LETTER Z WITH DOT BELOW
to_title(16#1E93)->
    16#1E92;
%% LATIN SMALL LETTER Z WITH LINE BELOW
to_title(16#1E95)->
    16#1E94;
%% LATIN SMALL LETTER LONG S WITH DOT ABOVE
to_title(16#1E9B)->
    16#1E60;
%% LATIN SMALL LETTER A WITH DOT BELOW
to_title(16#1EA1)->
    16#1EA0;
%% LATIN SMALL LETTER A WITH HOOK ABOVE
to_title(16#1EA3)->
    16#1EA2;
%% LATIN SMALL LETTER A WITH CIRCUMFLEX AND ACUTE
to_title(16#1EA5)->
    16#1EA4;
%% LATIN SMALL LETTER A WITH CIRCUMFLEX AND GRAVE
to_title(16#1EA7)->
    16#1EA6;
%% LATIN SMALL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
to_title(16#1EA9)->
    16#1EA8;
%% LATIN SMALL LETTER A WITH CIRCUMFLEX AND TILDE
to_title(16#1EAB)->
    16#1EAA;
%% LATIN SMALL LETTER A WITH CIRCUMFLEX AND DOT BELOW
to_title(16#1EAD)->
    16#1EAC;
%% LATIN SMALL LETTER A WITH BREVE AND ACUTE
to_title(16#1EAF)->
    16#1EAE;
%% LATIN SMALL LETTER A WITH BREVE AND GRAVE
to_title(16#1EB1)->
    16#1EB0;
%% LATIN SMALL LETTER A WITH BREVE AND HOOK ABOVE
to_title(16#1EB3)->
    16#1EB2;
%% LATIN SMALL LETTER A WITH BREVE AND TILDE
to_title(16#1EB5)->
    16#1EB4;
%% LATIN SMALL LETTER A WITH BREVE AND DOT BELOW
to_title(16#1EB7)->
    16#1EB6;
%% LATIN SMALL LETTER E WITH DOT BELOW
to_title(16#1EB9)->
    16#1EB8;
%% LATIN SMALL LETTER E WITH HOOK ABOVE
to_title(16#1EBB)->
    16#1EBA;
%% LATIN SMALL LETTER E WITH TILDE
to_title(16#1EBD)->
    16#1EBC;
%% LATIN SMALL LETTER E WITH CIRCUMFLEX AND ACUTE
to_title(16#1EBF)->
    16#1EBE;
%% LATIN SMALL LETTER E WITH CIRCUMFLEX AND GRAVE
to_title(16#1EC1)->
    16#1EC0;
%% LATIN SMALL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
to_title(16#1EC3)->
    16#1EC2;
%% LATIN SMALL LETTER E WITH CIRCUMFLEX AND TILDE
to_title(16#1EC5)->
    16#1EC4;
%% LATIN SMALL LETTER E WITH CIRCUMFLEX AND DOT BELOW
to_title(16#1EC7)->
    16#1EC6;
%% LATIN SMALL LETTER I WITH HOOK ABOVE
to_title(16#1EC9)->
    16#1EC8;
%% LATIN SMALL LETTER I WITH DOT BELOW
to_title(16#1ECB)->
    16#1ECA;
%% LATIN SMALL LETTER O WITH DOT BELOW
to_title(16#1ECD)->
    16#1ECC;
%% LATIN SMALL LETTER O WITH HOOK ABOVE
to_title(16#1ECF)->
    16#1ECE;
%% LATIN SMALL LETTER O WITH CIRCUMFLEX AND ACUTE
to_title(16#1ED1)->
    16#1ED0;
%% LATIN SMALL LETTER O WITH CIRCUMFLEX AND GRAVE
to_title(16#1ED3)->
    16#1ED2;
%% LATIN SMALL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
to_title(16#1ED5)->
    16#1ED4;
%% LATIN SMALL LETTER O WITH CIRCUMFLEX AND TILDE
to_title(16#1ED7)->
    16#1ED6;
%% LATIN SMALL LETTER O WITH CIRCUMFLEX AND DOT BELOW
to_title(16#1ED9)->
    16#1ED8;
%% LATIN SMALL LETTER O WITH HORN AND ACUTE
to_title(16#1EDB)->
    16#1EDA;
%% LATIN SMALL LETTER O WITH HORN AND GRAVE
to_title(16#1EDD)->
    16#1EDC;
%% LATIN SMALL LETTER O WITH HORN AND HOOK ABOVE
to_title(16#1EDF)->
    16#1EDE;
%% LATIN SMALL LETTER O WITH HORN AND TILDE
to_title(16#1EE1)->
    16#1EE0;
%% LATIN SMALL LETTER O WITH HORN AND DOT BELOW
to_title(16#1EE3)->
    16#1EE2;
%% LATIN SMALL LETTER U WITH DOT BELOW
to_title(16#1EE5)->
    16#1EE4;
%% LATIN SMALL LETTER U WITH HOOK ABOVE
to_title(16#1EE7)->
    16#1EE6;
%% LATIN SMALL LETTER U WITH HORN AND ACUTE
to_title(16#1EE9)->
    16#1EE8;
%% LATIN SMALL LETTER U WITH HORN AND GRAVE
to_title(16#1EEB)->
    16#1EEA;
%% LATIN SMALL LETTER U WITH HORN AND HOOK ABOVE
to_title(16#1EED)->
    16#1EEC;
%% LATIN SMALL LETTER U WITH HORN AND TILDE
to_title(16#1EEF)->
    16#1EEE;
%% LATIN SMALL LETTER U WITH HORN AND DOT BELOW
to_title(16#1EF1)->
    16#1EF0;
%% LATIN SMALL LETTER Y WITH GRAVE
to_title(16#1EF3)->
    16#1EF2;
%% LATIN SMALL LETTER Y WITH DOT BELOW
to_title(16#1EF5)->
    16#1EF4;
%% LATIN SMALL LETTER Y WITH HOOK ABOVE
to_title(16#1EF7)->
    16#1EF6;
%% LATIN SMALL LETTER Y WITH TILDE
to_title(16#1EF9)->
    16#1EF8;
%% LATIN SMALL LETTER MIDDLE-WELSH LL
to_title(16#1EFB)->
    16#1EFA;
%% LATIN SMALL LETTER MIDDLE-WELSH V
to_title(16#1EFD)->
    16#1EFC;
%% LATIN SMALL LETTER Y WITH LOOP
to_title(16#1EFF)->
    16#1EFE;
%% GREEK SMALL LETTER ALPHA WITH PSILI
to_title(16#1F00)->
    16#1F08;
%% GREEK SMALL LETTER ALPHA WITH DASIA
to_title(16#1F01)->
    16#1F09;
%% GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA
to_title(16#1F02)->
    16#1F0A;
%% GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA
to_title(16#1F03)->
    16#1F0B;
%% GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA
to_title(16#1F04)->
    16#1F0C;
%% GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA
to_title(16#1F05)->
    16#1F0D;
%% GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI
to_title(16#1F06)->
    16#1F0E;
%% GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI
to_title(16#1F07)->
    16#1F0F;
%% GREEK SMALL LETTER EPSILON WITH PSILI
to_title(16#1F10)->
    16#1F18;
%% GREEK SMALL LETTER EPSILON WITH DASIA
to_title(16#1F11)->
    16#1F19;
%% GREEK SMALL LETTER EPSILON WITH PSILI AND VARIA
to_title(16#1F12)->
    16#1F1A;
%% GREEK SMALL LETTER EPSILON WITH DASIA AND VARIA
to_title(16#1F13)->
    16#1F1B;
%% GREEK SMALL LETTER EPSILON WITH PSILI AND OXIA
to_title(16#1F14)->
    16#1F1C;
%% GREEK SMALL LETTER EPSILON WITH DASIA AND OXIA
to_title(16#1F15)->
    16#1F1D;
%% GREEK SMALL LETTER ETA WITH PSILI
to_title(16#1F20)->
    16#1F28;
%% GREEK SMALL LETTER ETA WITH DASIA
to_title(16#1F21)->
    16#1F29;
%% GREEK SMALL LETTER ETA WITH PSILI AND VARIA
to_title(16#1F22)->
    16#1F2A;
%% GREEK SMALL LETTER ETA WITH DASIA AND VARIA
to_title(16#1F23)->
    16#1F2B;
%% GREEK SMALL LETTER ETA WITH PSILI AND OXIA
to_title(16#1F24)->
    16#1F2C;
%% GREEK SMALL LETTER ETA WITH DASIA AND OXIA
to_title(16#1F25)->
    16#1F2D;
%% GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI
to_title(16#1F26)->
    16#1F2E;
%% GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI
to_title(16#1F27)->
    16#1F2F;
%% GREEK SMALL LETTER IOTA WITH PSILI
to_title(16#1F30)->
    16#1F38;
%% GREEK SMALL LETTER IOTA WITH DASIA
to_title(16#1F31)->
    16#1F39;
%% GREEK SMALL LETTER IOTA WITH PSILI AND VARIA
to_title(16#1F32)->
    16#1F3A;
%% GREEK SMALL LETTER IOTA WITH DASIA AND VARIA
to_title(16#1F33)->
    16#1F3B;
%% GREEK SMALL LETTER IOTA WITH PSILI AND OXIA
to_title(16#1F34)->
    16#1F3C;
%% GREEK SMALL LETTER IOTA WITH DASIA AND OXIA
to_title(16#1F35)->
    16#1F3D;
%% GREEK SMALL LETTER IOTA WITH PSILI AND PERISPOMENI
to_title(16#1F36)->
    16#1F3E;
%% GREEK SMALL LETTER IOTA WITH DASIA AND PERISPOMENI
to_title(16#1F37)->
    16#1F3F;
%% GREEK SMALL LETTER OMICRON WITH PSILI
to_title(16#1F40)->
    16#1F48;
%% GREEK SMALL LETTER OMICRON WITH DASIA
to_title(16#1F41)->
    16#1F49;
%% GREEK SMALL LETTER OMICRON WITH PSILI AND VARIA
to_title(16#1F42)->
    16#1F4A;
%% GREEK SMALL LETTER OMICRON WITH DASIA AND VARIA
to_title(16#1F43)->
    16#1F4B;
%% GREEK SMALL LETTER OMICRON WITH PSILI AND OXIA
to_title(16#1F44)->
    16#1F4C;
%% GREEK SMALL LETTER OMICRON WITH DASIA AND OXIA
to_title(16#1F45)->
    16#1F4D;
%% GREEK SMALL LETTER UPSILON WITH DASIA
to_title(16#1F51)->
    16#1F59;
%% GREEK SMALL LETTER UPSILON WITH DASIA AND VARIA
to_title(16#1F53)->
    16#1F5B;
%% GREEK SMALL LETTER UPSILON WITH DASIA AND OXIA
to_title(16#1F55)->
    16#1F5D;
%% GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI
to_title(16#1F57)->
    16#1F5F;
%% GREEK SMALL LETTER OMEGA WITH PSILI
to_title(16#1F60)->
    16#1F68;
%% GREEK SMALL LETTER OMEGA WITH DASIA
to_title(16#1F61)->
    16#1F69;
%% GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA
to_title(16#1F62)->
    16#1F6A;
%% GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA
to_title(16#1F63)->
    16#1F6B;
%% GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA
to_title(16#1F64)->
    16#1F6C;
%% GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA
to_title(16#1F65)->
    16#1F6D;
%% GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI
to_title(16#1F66)->
    16#1F6E;
%% GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI
to_title(16#1F67)->
    16#1F6F;
%% GREEK SMALL LETTER ALPHA WITH VARIA
to_title(16#1F70)->
    16#1FBA;
%% GREEK SMALL LETTER ALPHA WITH OXIA
to_title(16#1F71)->
    16#1FBB;
%% GREEK SMALL LETTER EPSILON WITH VARIA
to_title(16#1F72)->
    16#1FC8;
%% GREEK SMALL LETTER EPSILON WITH OXIA
to_title(16#1F73)->
    16#1FC9;
%% GREEK SMALL LETTER ETA WITH VARIA
to_title(16#1F74)->
    16#1FCA;
%% GREEK SMALL LETTER ETA WITH OXIA
to_title(16#1F75)->
    16#1FCB;
%% GREEK SMALL LETTER IOTA WITH VARIA
to_title(16#1F76)->
    16#1FDA;
%% GREEK SMALL LETTER IOTA WITH OXIA
to_title(16#1F77)->
    16#1FDB;
%% GREEK SMALL LETTER OMICRON WITH VARIA
to_title(16#1F78)->
    16#1FF8;
%% GREEK SMALL LETTER OMICRON WITH OXIA
to_title(16#1F79)->
    16#1FF9;
%% GREEK SMALL LETTER UPSILON WITH VARIA
to_title(16#1F7A)->
    16#1FEA;
%% GREEK SMALL LETTER UPSILON WITH OXIA
to_title(16#1F7B)->
    16#1FEB;
%% GREEK SMALL LETTER OMEGA WITH VARIA
to_title(16#1F7C)->
    16#1FFA;
%% GREEK SMALL LETTER OMEGA WITH OXIA
to_title(16#1F7D)->
    16#1FFB;
%% GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI
to_title(16#1F80)->
    16#1F88;
%% GREEK SMALL LETTER ALPHA WITH DASIA AND YPOGEGRAMMENI
to_title(16#1F81)->
    16#1F89;
%% GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA AND YPOGEGRAMMENI
to_title(16#1F82)->
    16#1F8A;
%% GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA AND YPOGEGRAMMENI
to_title(16#1F83)->
    16#1F8B;
%% GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA AND YPOGEGRAMMENI
to_title(16#1F84)->
    16#1F8C;
%% GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA AND YPOGEGRAMMENI
to_title(16#1F85)->
    16#1F8D;
%% GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
to_title(16#1F86)->
    16#1F8E;
%% GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
to_title(16#1F87)->
    16#1F8F;
%% GREEK SMALL LETTER ETA WITH PSILI AND YPOGEGRAMMENI
to_title(16#1F90)->
    16#1F98;
%% GREEK SMALL LETTER ETA WITH DASIA AND YPOGEGRAMMENI
to_title(16#1F91)->
    16#1F99;
%% GREEK SMALL LETTER ETA WITH PSILI AND VARIA AND YPOGEGRAMMENI
to_title(16#1F92)->
    16#1F9A;
%% GREEK SMALL LETTER ETA WITH DASIA AND VARIA AND YPOGEGRAMMENI
to_title(16#1F93)->
    16#1F9B;
%% GREEK SMALL LETTER ETA WITH PSILI AND OXIA AND YPOGEGRAMMENI
to_title(16#1F94)->
    16#1F9C;
%% GREEK SMALL LETTER ETA WITH DASIA AND OXIA AND YPOGEGRAMMENI
to_title(16#1F95)->
    16#1F9D;
%% GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
to_title(16#1F96)->
    16#1F9E;
%% GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
to_title(16#1F97)->
    16#1F9F;
%% GREEK SMALL LETTER OMEGA WITH PSILI AND YPOGEGRAMMENI
to_title(16#1FA0)->
    16#1FA8;
%% GREEK SMALL LETTER OMEGA WITH DASIA AND YPOGEGRAMMENI
to_title(16#1FA1)->
    16#1FA9;
%% GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA AND YPOGEGRAMMENI
to_title(16#1FA2)->
    16#1FAA;
%% GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA AND YPOGEGRAMMENI
to_title(16#1FA3)->
    16#1FAB;
%% GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA AND YPOGEGRAMMENI
to_title(16#1FA4)->
    16#1FAC;
%% GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA AND YPOGEGRAMMENI
to_title(16#1FA5)->
    16#1FAD;
%% GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
to_title(16#1FA6)->
    16#1FAE;
%% GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
to_title(16#1FA7)->
    16#1FAF;
%% GREEK SMALL LETTER ALPHA WITH VRACHY
to_title(16#1FB0)->
    16#1FB8;
%% GREEK SMALL LETTER ALPHA WITH MACRON
to_title(16#1FB1)->
    16#1FB9;
%% GREEK SMALL LETTER ALPHA WITH YPOGEGRAMMENI
to_title(16#1FB3)->
    16#1FBC;
%% GREEK PROSGEGRAMMENI
to_title(16#1FBE)->
    16#0399;
%% GREEK SMALL LETTER ETA WITH YPOGEGRAMMENI
to_title(16#1FC3)->
    16#1FCC;
%% GREEK SMALL LETTER IOTA WITH VRACHY
to_title(16#1FD0)->
    16#1FD8;
%% GREEK SMALL LETTER IOTA WITH MACRON
to_title(16#1FD1)->
    16#1FD9;
%% GREEK SMALL LETTER UPSILON WITH VRACHY
to_title(16#1FE0)->
    16#1FE8;
%% GREEK SMALL LETTER UPSILON WITH MACRON
to_title(16#1FE1)->
    16#1FE9;
%% GREEK SMALL LETTER RHO WITH DASIA
to_title(16#1FE5)->
    16#1FEC;
%% GREEK SMALL LETTER OMEGA WITH YPOGEGRAMMENI
to_title(16#1FF3)->
    16#1FFC;
%% TURNED SMALL F
to_title(16#214E)->
    16#2132;
%% SMALL ROMAN NUMERAL ONE
to_title(16#2170)->
    16#2160;
%% SMALL ROMAN NUMERAL TWO
to_title(16#2171)->
    16#2161;
%% SMALL ROMAN NUMERAL THREE
to_title(16#2172)->
    16#2162;
%% SMALL ROMAN NUMERAL FOUR
to_title(16#2173)->
    16#2163;
%% SMALL ROMAN NUMERAL FIVE
to_title(16#2174)->
    16#2164;
%% SMALL ROMAN NUMERAL SIX
to_title(16#2175)->
    16#2165;
%% SMALL ROMAN NUMERAL SEVEN
to_title(16#2176)->
    16#2166;
%% SMALL ROMAN NUMERAL EIGHT
to_title(16#2177)->
    16#2167;
%% SMALL ROMAN NUMERAL NINE
to_title(16#2178)->
    16#2168;
%% SMALL ROMAN NUMERAL TEN
to_title(16#2179)->
    16#2169;
%% SMALL ROMAN NUMERAL ELEVEN
to_title(16#217A)->
    16#216A;
%% SMALL ROMAN NUMERAL TWELVE
to_title(16#217B)->
    16#216B;
%% SMALL ROMAN NUMERAL FIFTY
to_title(16#217C)->
    16#216C;
%% SMALL ROMAN NUMERAL ONE HUNDRED
to_title(16#217D)->
    16#216D;
%% SMALL ROMAN NUMERAL FIVE HUNDRED
to_title(16#217E)->
    16#216E;
%% SMALL ROMAN NUMERAL ONE THOUSAND
to_title(16#217F)->
    16#216F;
%% LATIN SMALL LETTER REVERSED C
to_title(16#2184)->
    16#2183;
%% CIRCLED LATIN SMALL LETTER A
to_title(16#24D0)->
    16#24B6;
%% CIRCLED LATIN SMALL LETTER B
to_title(16#24D1)->
    16#24B7;
%% CIRCLED LATIN SMALL LETTER C
to_title(16#24D2)->
    16#24B8;
%% CIRCLED LATIN SMALL LETTER D
to_title(16#24D3)->
    16#24B9;
%% CIRCLED LATIN SMALL LETTER E
to_title(16#24D4)->
    16#24BA;
%% CIRCLED LATIN SMALL LETTER F
to_title(16#24D5)->
    16#24BB;
%% CIRCLED LATIN SMALL LETTER G
to_title(16#24D6)->
    16#24BC;
%% CIRCLED LATIN SMALL LETTER H
to_title(16#24D7)->
    16#24BD;
%% CIRCLED LATIN SMALL LETTER I
to_title(16#24D8)->
    16#24BE;
%% CIRCLED LATIN SMALL LETTER J
to_title(16#24D9)->
    16#24BF;
%% CIRCLED LATIN SMALL LETTER K
to_title(16#24DA)->
    16#24C0;
%% CIRCLED LATIN SMALL LETTER L
to_title(16#24DB)->
    16#24C1;
%% CIRCLED LATIN SMALL LETTER M
to_title(16#24DC)->
    16#24C2;
%% CIRCLED LATIN SMALL LETTER N
to_title(16#24DD)->
    16#24C3;
%% CIRCLED LATIN SMALL LETTER O
to_title(16#24DE)->
    16#24C4;
%% CIRCLED LATIN SMALL LETTER P
to_title(16#24DF)->
    16#24C5;
%% CIRCLED LATIN SMALL LETTER Q
to_title(16#24E0)->
    16#24C6;
%% CIRCLED LATIN SMALL LETTER R
to_title(16#24E1)->
    16#24C7;
%% CIRCLED LATIN SMALL LETTER S
to_title(16#24E2)->
    16#24C8;
%% CIRCLED LATIN SMALL LETTER T
to_title(16#24E3)->
    16#24C9;
%% CIRCLED LATIN SMALL LETTER U
to_title(16#24E4)->
    16#24CA;
%% CIRCLED LATIN SMALL LETTER V
to_title(16#24E5)->
    16#24CB;
%% CIRCLED LATIN SMALL LETTER W
to_title(16#24E6)->
    16#24CC;
%% CIRCLED LATIN SMALL LETTER X
to_title(16#24E7)->
    16#24CD;
%% CIRCLED LATIN SMALL LETTER Y
to_title(16#24E8)->
    16#24CE;
%% CIRCLED LATIN SMALL LETTER Z
to_title(16#24E9)->
    16#24CF;
%% GLAGOLITIC SMALL LETTER AZU
to_title(16#2C30)->
    16#2C00;
%% GLAGOLITIC SMALL LETTER BUKY
to_title(16#2C31)->
    16#2C01;
%% GLAGOLITIC SMALL LETTER VEDE
to_title(16#2C32)->
    16#2C02;
%% GLAGOLITIC SMALL LETTER GLAGOLI
to_title(16#2C33)->
    16#2C03;
%% GLAGOLITIC SMALL LETTER DOBRO
to_title(16#2C34)->
    16#2C04;
%% GLAGOLITIC SMALL LETTER YESTU
to_title(16#2C35)->
    16#2C05;
%% GLAGOLITIC SMALL LETTER ZHIVETE
to_title(16#2C36)->
    16#2C06;
%% GLAGOLITIC SMALL LETTER DZELO
to_title(16#2C37)->
    16#2C07;
%% GLAGOLITIC SMALL LETTER ZEMLJA
to_title(16#2C38)->
    16#2C08;
%% GLAGOLITIC SMALL LETTER IZHE
to_title(16#2C39)->
    16#2C09;
%% GLAGOLITIC SMALL LETTER INITIAL IZHE
to_title(16#2C3A)->
    16#2C0A;
%% GLAGOLITIC SMALL LETTER I
to_title(16#2C3B)->
    16#2C0B;
%% GLAGOLITIC SMALL LETTER DJERVI
to_title(16#2C3C)->
    16#2C0C;
%% GLAGOLITIC SMALL LETTER KAKO
to_title(16#2C3D)->
    16#2C0D;
%% GLAGOLITIC SMALL LETTER LJUDIJE
to_title(16#2C3E)->
    16#2C0E;
%% GLAGOLITIC SMALL LETTER MYSLITE
to_title(16#2C3F)->
    16#2C0F;
%% GLAGOLITIC SMALL LETTER NASHI
to_title(16#2C40)->
    16#2C10;
%% GLAGOLITIC SMALL LETTER ONU
to_title(16#2C41)->
    16#2C11;
%% GLAGOLITIC SMALL LETTER POKOJI
to_title(16#2C42)->
    16#2C12;
%% GLAGOLITIC SMALL LETTER RITSI
to_title(16#2C43)->
    16#2C13;
%% GLAGOLITIC SMALL LETTER SLOVO
to_title(16#2C44)->
    16#2C14;
%% GLAGOLITIC SMALL LETTER TVRIDO
to_title(16#2C45)->
    16#2C15;
%% GLAGOLITIC SMALL LETTER UKU
to_title(16#2C46)->
    16#2C16;
%% GLAGOLITIC SMALL LETTER FRITU
to_title(16#2C47)->
    16#2C17;
%% GLAGOLITIC SMALL LETTER HERU
to_title(16#2C48)->
    16#2C18;
%% GLAGOLITIC SMALL LETTER OTU
to_title(16#2C49)->
    16#2C19;
%% GLAGOLITIC SMALL LETTER PE
to_title(16#2C4A)->
    16#2C1A;
%% GLAGOLITIC SMALL LETTER SHTA
to_title(16#2C4B)->
    16#2C1B;
%% GLAGOLITIC SMALL LETTER TSI
to_title(16#2C4C)->
    16#2C1C;
%% GLAGOLITIC SMALL LETTER CHRIVI
to_title(16#2C4D)->
    16#2C1D;
%% GLAGOLITIC SMALL LETTER SHA
to_title(16#2C4E)->
    16#2C1E;
%% GLAGOLITIC SMALL LETTER YERU
to_title(16#2C4F)->
    16#2C1F;
%% GLAGOLITIC SMALL LETTER YERI
to_title(16#2C50)->
    16#2C20;
%% GLAGOLITIC SMALL LETTER YATI
to_title(16#2C51)->
    16#2C21;
%% GLAGOLITIC SMALL LETTER SPIDERY HA
to_title(16#2C52)->
    16#2C22;
%% GLAGOLITIC SMALL LETTER YU
to_title(16#2C53)->
    16#2C23;
%% GLAGOLITIC SMALL LETTER SMALL YUS
to_title(16#2C54)->
    16#2C24;
%% GLAGOLITIC SMALL LETTER SMALL YUS WITH TAIL
to_title(16#2C55)->
    16#2C25;
%% GLAGOLITIC SMALL LETTER YO
to_title(16#2C56)->
    16#2C26;
%% GLAGOLITIC SMALL LETTER IOTATED SMALL YUS
to_title(16#2C57)->
    16#2C27;
%% GLAGOLITIC SMALL LETTER BIG YUS
to_title(16#2C58)->
    16#2C28;
%% GLAGOLITIC SMALL LETTER IOTATED BIG YUS
to_title(16#2C59)->
    16#2C29;
%% GLAGOLITIC SMALL LETTER FITA
to_title(16#2C5A)->
    16#2C2A;
%% GLAGOLITIC SMALL LETTER IZHITSA
to_title(16#2C5B)->
    16#2C2B;
%% GLAGOLITIC SMALL LETTER SHTAPIC
to_title(16#2C5C)->
    16#2C2C;
%% GLAGOLITIC SMALL LETTER TROKUTASTI A
to_title(16#2C5D)->
    16#2C2D;
%% GLAGOLITIC SMALL LETTER LATINATE MYSLITE
to_title(16#2C5E)->
    16#2C2E;
%% LATIN SMALL LETTER L WITH DOUBLE BAR
to_title(16#2C61)->
    16#2C60;
%% LATIN SMALL LETTER A WITH STROKE
to_title(16#2C65)->
    16#023A;
%% LATIN SMALL LETTER T WITH DIAGONAL STROKE
to_title(16#2C66)->
    16#023E;
%% LATIN SMALL LETTER H WITH DESCENDER
to_title(16#2C68)->
    16#2C67;
%% LATIN SMALL LETTER K WITH DESCENDER
to_title(16#2C6A)->
    16#2C69;
%% LATIN SMALL LETTER Z WITH DESCENDER
to_title(16#2C6C)->
    16#2C6B;
%% LATIN SMALL LETTER W WITH HOOK
to_title(16#2C73)->
    16#2C72;
%% LATIN SMALL LETTER HALF H
to_title(16#2C76)->
    16#2C75;
%% COPTIC SMALL LETTER ALFA
to_title(16#2C81)->
    16#2C80;
%% COPTIC SMALL LETTER VIDA
to_title(16#2C83)->
    16#2C82;
%% COPTIC SMALL LETTER GAMMA
to_title(16#2C85)->
    16#2C84;
%% COPTIC SMALL LETTER DALDA
to_title(16#2C87)->
    16#2C86;
%% COPTIC SMALL LETTER EIE
to_title(16#2C89)->
    16#2C88;
%% COPTIC SMALL LETTER SOU
to_title(16#2C8B)->
    16#2C8A;
%% COPTIC SMALL LETTER ZATA
to_title(16#2C8D)->
    16#2C8C;
%% COPTIC SMALL LETTER HATE
to_title(16#2C8F)->
    16#2C8E;
%% COPTIC SMALL LETTER THETHE
to_title(16#2C91)->
    16#2C90;
%% COPTIC SMALL LETTER IAUDA
to_title(16#2C93)->
    16#2C92;
%% COPTIC SMALL LETTER KAPA
to_title(16#2C95)->
    16#2C94;
%% COPTIC SMALL LETTER LAULA
to_title(16#2C97)->
    16#2C96;
%% COPTIC SMALL LETTER MI
to_title(16#2C99)->
    16#2C98;
%% COPTIC SMALL LETTER NI
to_title(16#2C9B)->
    16#2C9A;
%% COPTIC SMALL LETTER KSI
to_title(16#2C9D)->
    16#2C9C;
%% COPTIC SMALL LETTER O
to_title(16#2C9F)->
    16#2C9E;
%% COPTIC SMALL LETTER PI
to_title(16#2CA1)->
    16#2CA0;
%% COPTIC SMALL LETTER RO
to_title(16#2CA3)->
    16#2CA2;
%% COPTIC SMALL LETTER SIMA
to_title(16#2CA5)->
    16#2CA4;
%% COPTIC SMALL LETTER TAU
to_title(16#2CA7)->
    16#2CA6;
%% COPTIC SMALL LETTER UA
to_title(16#2CA9)->
    16#2CA8;
%% COPTIC SMALL LETTER FI
to_title(16#2CAB)->
    16#2CAA;
%% COPTIC SMALL LETTER KHI
to_title(16#2CAD)->
    16#2CAC;
%% COPTIC SMALL LETTER PSI
to_title(16#2CAF)->
    16#2CAE;
%% COPTIC SMALL LETTER OOU
to_title(16#2CB1)->
    16#2CB0;
%% COPTIC SMALL LETTER DIALECT-P ALEF
to_title(16#2CB3)->
    16#2CB2;
%% COPTIC SMALL LETTER OLD COPTIC AIN
to_title(16#2CB5)->
    16#2CB4;
%% COPTIC SMALL LETTER CRYPTOGRAMMIC EIE
to_title(16#2CB7)->
    16#2CB6;
%% COPTIC SMALL LETTER DIALECT-P KAPA
to_title(16#2CB9)->
    16#2CB8;
%% COPTIC SMALL LETTER DIALECT-P NI
to_title(16#2CBB)->
    16#2CBA;
%% COPTIC SMALL LETTER CRYPTOGRAMMIC NI
to_title(16#2CBD)->
    16#2CBC;
%% COPTIC SMALL LETTER OLD COPTIC OOU
to_title(16#2CBF)->
    16#2CBE;
%% COPTIC SMALL LETTER SAMPI
to_title(16#2CC1)->
    16#2CC0;
%% COPTIC SMALL LETTER CROSSED SHEI
to_title(16#2CC3)->
    16#2CC2;
%% COPTIC SMALL LETTER OLD COPTIC SHEI
to_title(16#2CC5)->
    16#2CC4;
%% COPTIC SMALL LETTER OLD COPTIC ESH
to_title(16#2CC7)->
    16#2CC6;
%% COPTIC SMALL LETTER AKHMIMIC KHEI
to_title(16#2CC9)->
    16#2CC8;
%% COPTIC SMALL LETTER DIALECT-P HORI
to_title(16#2CCB)->
    16#2CCA;
%% COPTIC SMALL LETTER OLD COPTIC HORI
to_title(16#2CCD)->
    16#2CCC;
%% COPTIC SMALL LETTER OLD COPTIC HA
to_title(16#2CCF)->
    16#2CCE;
%% COPTIC SMALL LETTER L-SHAPED HA
to_title(16#2CD1)->
    16#2CD0;
%% COPTIC SMALL LETTER OLD COPTIC HEI
to_title(16#2CD3)->
    16#2CD2;
%% COPTIC SMALL LETTER OLD COPTIC HAT
to_title(16#2CD5)->
    16#2CD4;
%% COPTIC SMALL LETTER OLD COPTIC GANGIA
to_title(16#2CD7)->
    16#2CD6;
%% COPTIC SMALL LETTER OLD COPTIC DJA
to_title(16#2CD9)->
    16#2CD8;
%% COPTIC SMALL LETTER OLD COPTIC SHIMA
to_title(16#2CDB)->
    16#2CDA;
%% COPTIC SMALL LETTER OLD NUBIAN SHIMA
to_title(16#2CDD)->
    16#2CDC;
%% COPTIC SMALL LETTER OLD NUBIAN NGI
to_title(16#2CDF)->
    16#2CDE;
%% COPTIC SMALL LETTER OLD NUBIAN NYI
to_title(16#2CE1)->
    16#2CE0;
%% COPTIC SMALL LETTER OLD NUBIAN WAU
to_title(16#2CE3)->
    16#2CE2;
%% COPTIC SMALL LETTER CRYPTOGRAMMIC SHEI
to_title(16#2CEC)->
    16#2CEB;
%% COPTIC SMALL LETTER CRYPTOGRAMMIC GANGIA
to_title(16#2CEE)->
    16#2CED;
%% COPTIC SMALL LETTER BOHAIRIC KHEI
to_title(16#2CF3)->
    16#2CF2;
%% GEORGIAN SMALL LETTER AN
to_title(16#2D00)->
    16#10A0;
%% GEORGIAN SMALL LETTER BAN
to_title(16#2D01)->
    16#10A1;
%% GEORGIAN SMALL LETTER GAN
to_title(16#2D02)->
    16#10A2;
%% GEORGIAN SMALL LETTER DON
to_title(16#2D03)->
    16#10A3;
%% GEORGIAN SMALL LETTER EN
to_title(16#2D04)->
    16#10A4;
%% GEORGIAN SMALL LETTER VIN
to_title(16#2D05)->
    16#10A5;
%% GEORGIAN SMALL LETTER ZEN
to_title(16#2D06)->
    16#10A6;
%% GEORGIAN SMALL LETTER TAN
to_title(16#2D07)->
    16#10A7;
%% GEORGIAN SMALL LETTER IN
to_title(16#2D08)->
    16#10A8;
%% GEORGIAN SMALL LETTER KAN
to_title(16#2D09)->
    16#10A9;
%% GEORGIAN SMALL LETTER LAS
to_title(16#2D0A)->
    16#10AA;
%% GEORGIAN SMALL LETTER MAN
to_title(16#2D0B)->
    16#10AB;
%% GEORGIAN SMALL LETTER NAR
to_title(16#2D0C)->
    16#10AC;
%% GEORGIAN SMALL LETTER ON
to_title(16#2D0D)->
    16#10AD;
%% GEORGIAN SMALL LETTER PAR
to_title(16#2D0E)->
    16#10AE;
%% GEORGIAN SMALL LETTER ZHAR
to_title(16#2D0F)->
    16#10AF;
%% GEORGIAN SMALL LETTER RAE
to_title(16#2D10)->
    16#10B0;
%% GEORGIAN SMALL LETTER SAN
to_title(16#2D11)->
    16#10B1;
%% GEORGIAN SMALL LETTER TAR
to_title(16#2D12)->
    16#10B2;
%% GEORGIAN SMALL LETTER UN
to_title(16#2D13)->
    16#10B3;
%% GEORGIAN SMALL LETTER PHAR
to_title(16#2D14)->
    16#10B4;
%% GEORGIAN SMALL LETTER KHAR
to_title(16#2D15)->
    16#10B5;
%% GEORGIAN SMALL LETTER GHAN
to_title(16#2D16)->
    16#10B6;
%% GEORGIAN SMALL LETTER QAR
to_title(16#2D17)->
    16#10B7;
%% GEORGIAN SMALL LETTER SHIN
to_title(16#2D18)->
    16#10B8;
%% GEORGIAN SMALL LETTER CHIN
to_title(16#2D19)->
    16#10B9;
%% GEORGIAN SMALL LETTER CAN
to_title(16#2D1A)->
    16#10BA;
%% GEORGIAN SMALL LETTER JIL
to_title(16#2D1B)->
    16#10BB;
%% GEORGIAN SMALL LETTER CIL
to_title(16#2D1C)->
    16#10BC;
%% GEORGIAN SMALL LETTER CHAR
to_title(16#2D1D)->
    16#10BD;
%% GEORGIAN SMALL LETTER XAN
to_title(16#2D1E)->
    16#10BE;
%% GEORGIAN SMALL LETTER JHAN
to_title(16#2D1F)->
    16#10BF;
%% GEORGIAN SMALL LETTER HAE
to_title(16#2D20)->
    16#10C0;
%% GEORGIAN SMALL LETTER HE
to_title(16#2D21)->
    16#10C1;
%% GEORGIAN SMALL LETTER HIE
to_title(16#2D22)->
    16#10C2;
%% GEORGIAN SMALL LETTER WE
to_title(16#2D23)->
    16#10C3;
%% GEORGIAN SMALL LETTER HAR
to_title(16#2D24)->
    16#10C4;
%% GEORGIAN SMALL LETTER HOE
to_title(16#2D25)->
    16#10C5;
%% GEORGIAN SMALL LETTER YN
to_title(16#2D27)->
    16#10C7;
%% GEORGIAN SMALL LETTER AEN
to_title(16#2D2D)->
    16#10CD;
%% CYRILLIC SMALL LETTER ZEMLYA
to_title(16#A641)->
    16#A640;
%% CYRILLIC SMALL LETTER DZELO
to_title(16#A643)->
    16#A642;
%% CYRILLIC SMALL LETTER REVERSED DZE
to_title(16#A645)->
    16#A644;
%% CYRILLIC SMALL LETTER IOTA
to_title(16#A647)->
    16#A646;
%% CYRILLIC SMALL LETTER DJERV
to_title(16#A649)->
    16#A648;
%% CYRILLIC SMALL LETTER MONOGRAPH UK
to_title(16#A64B)->
    16#A64A;
%% CYRILLIC SMALL LETTER BROAD OMEGA
to_title(16#A64D)->
    16#A64C;
%% CYRILLIC SMALL LETTER NEUTRAL YER
to_title(16#A64F)->
    16#A64E;
%% CYRILLIC SMALL LETTER YERU WITH BACK YER
to_title(16#A651)->
    16#A650;
%% CYRILLIC SMALL LETTER IOTIFIED YAT
to_title(16#A653)->
    16#A652;
%% CYRILLIC SMALL LETTER REVERSED YU
to_title(16#A655)->
    16#A654;
%% CYRILLIC SMALL LETTER IOTIFIED A
to_title(16#A657)->
    16#A656;
%% CYRILLIC SMALL LETTER CLOSED LITTLE YUS
to_title(16#A659)->
    16#A658;
%% CYRILLIC SMALL LETTER BLENDED YUS
to_title(16#A65B)->
    16#A65A;
%% CYRILLIC SMALL LETTER IOTIFIED CLOSED LITTLE YUS
to_title(16#A65D)->
    16#A65C;
%% CYRILLIC SMALL LETTER YN
to_title(16#A65F)->
    16#A65E;
%% CYRILLIC SMALL LETTER REVERSED TSE
to_title(16#A661)->
    16#A660;
%% CYRILLIC SMALL LETTER SOFT DE
to_title(16#A663)->
    16#A662;
%% CYRILLIC SMALL LETTER SOFT EL
to_title(16#A665)->
    16#A664;
%% CYRILLIC SMALL LETTER SOFT EM
to_title(16#A667)->
    16#A666;
%% CYRILLIC SMALL LETTER MONOCULAR O
to_title(16#A669)->
    16#A668;
%% CYRILLIC SMALL LETTER BINOCULAR O
to_title(16#A66B)->
    16#A66A;
%% CYRILLIC SMALL LETTER DOUBLE MONOCULAR O
to_title(16#A66D)->
    16#A66C;
%% CYRILLIC SMALL LETTER DWE
to_title(16#A681)->
    16#A680;
%% CYRILLIC SMALL LETTER DZWE
to_title(16#A683)->
    16#A682;
%% CYRILLIC SMALL LETTER ZHWE
to_title(16#A685)->
    16#A684;
%% CYRILLIC SMALL LETTER CCHE
to_title(16#A687)->
    16#A686;
%% CYRILLIC SMALL LETTER DZZE
to_title(16#A689)->
    16#A688;
%% CYRILLIC SMALL LETTER TE WITH MIDDLE HOOK
to_title(16#A68B)->
    16#A68A;
%% CYRILLIC SMALL LETTER TWE
to_title(16#A68D)->
    16#A68C;
%% CYRILLIC SMALL LETTER TSWE
to_title(16#A68F)->
    16#A68E;
%% CYRILLIC SMALL LETTER TSSE
to_title(16#A691)->
    16#A690;
%% CYRILLIC SMALL LETTER TCHE
to_title(16#A693)->
    16#A692;
%% CYRILLIC SMALL LETTER HWE
to_title(16#A695)->
    16#A694;
%% CYRILLIC SMALL LETTER SHWE
to_title(16#A697)->
    16#A696;
%% LATIN SMALL LETTER EGYPTOLOGICAL ALEF
to_title(16#A723)->
    16#A722;
%% LATIN SMALL LETTER EGYPTOLOGICAL AIN
to_title(16#A725)->
    16#A724;
%% LATIN SMALL LETTER HENG
to_title(16#A727)->
    16#A726;
%% LATIN SMALL LETTER TZ
to_title(16#A729)->
    16#A728;
%% LATIN SMALL LETTER TRESILLO
to_title(16#A72B)->
    16#A72A;
%% LATIN SMALL LETTER CUATRILLO
to_title(16#A72D)->
    16#A72C;
%% LATIN SMALL LETTER CUATRILLO WITH COMMA
to_title(16#A72F)->
    16#A72E;
%% LATIN SMALL LETTER AA
to_title(16#A733)->
    16#A732;
%% LATIN SMALL LETTER AO
to_title(16#A735)->
    16#A734;
%% LATIN SMALL LETTER AU
to_title(16#A737)->
    16#A736;
%% LATIN SMALL LETTER AV
to_title(16#A739)->
    16#A738;
%% LATIN SMALL LETTER AV WITH HORIZONTAL BAR
to_title(16#A73B)->
    16#A73A;
%% LATIN SMALL LETTER AY
to_title(16#A73D)->
    16#A73C;
%% LATIN SMALL LETTER REVERSED C WITH DOT
to_title(16#A73F)->
    16#A73E;
%% LATIN SMALL LETTER K WITH STROKE
to_title(16#A741)->
    16#A740;
%% LATIN SMALL LETTER K WITH DIAGONAL STROKE
to_title(16#A743)->
    16#A742;
%% LATIN SMALL LETTER K WITH STROKE AND DIAGONAL STROKE
to_title(16#A745)->
    16#A744;
%% LATIN SMALL LETTER BROKEN L
to_title(16#A747)->
    16#A746;
%% LATIN SMALL LETTER L WITH HIGH STROKE
to_title(16#A749)->
    16#A748;
%% LATIN SMALL LETTER O WITH LONG STROKE OVERLAY
to_title(16#A74B)->
    16#A74A;
%% LATIN SMALL LETTER O WITH LOOP
to_title(16#A74D)->
    16#A74C;
%% LATIN SMALL LETTER OO
to_title(16#A74F)->
    16#A74E;
%% LATIN SMALL LETTER P WITH STROKE THROUGH DESCENDER
to_title(16#A751)->
    16#A750;
%% LATIN SMALL LETTER P WITH FLOURISH
to_title(16#A753)->
    16#A752;
%% LATIN SMALL LETTER P WITH SQUIRREL TAIL
to_title(16#A755)->
    16#A754;
%% LATIN SMALL LETTER Q WITH STROKE THROUGH DESCENDER
to_title(16#A757)->
    16#A756;
%% LATIN SMALL LETTER Q WITH DIAGONAL STROKE
to_title(16#A759)->
    16#A758;
%% LATIN SMALL LETTER R ROTUNDA
to_title(16#A75B)->
    16#A75A;
%% LATIN SMALL LETTER RUM ROTUNDA
to_title(16#A75D)->
    16#A75C;
%% LATIN SMALL LETTER V WITH DIAGONAL STROKE
to_title(16#A75F)->
    16#A75E;
%% LATIN SMALL LETTER VY
to_title(16#A761)->
    16#A760;
%% LATIN SMALL LETTER VISIGOTHIC Z
to_title(16#A763)->
    16#A762;
%% LATIN SMALL LETTER THORN WITH STROKE
to_title(16#A765)->
    16#A764;
%% LATIN SMALL LETTER THORN WITH STROKE THROUGH DESCENDER
to_title(16#A767)->
    16#A766;
%% LATIN SMALL LETTER VEND
to_title(16#A769)->
    16#A768;
%% LATIN SMALL LETTER ET
to_title(16#A76B)->
    16#A76A;
%% LATIN SMALL LETTER IS
to_title(16#A76D)->
    16#A76C;
%% LATIN SMALL LETTER CON
to_title(16#A76F)->
    16#A76E;
%% LATIN SMALL LETTER INSULAR D
to_title(16#A77A)->
    16#A779;
%% LATIN SMALL LETTER INSULAR F
to_title(16#A77C)->
    16#A77B;
%% LATIN SMALL LETTER TURNED INSULAR G
to_title(16#A77F)->
    16#A77E;
%% LATIN SMALL LETTER TURNED L
to_title(16#A781)->
    16#A780;
%% LATIN SMALL LETTER INSULAR R
to_title(16#A783)->
    16#A782;
%% LATIN SMALL LETTER INSULAR S
to_title(16#A785)->
    16#A784;
%% LATIN SMALL LETTER INSULAR T
to_title(16#A787)->
    16#A786;
%% LATIN SMALL LETTER SALTILLO
to_title(16#A78C)->
    16#A78B;
%% LATIN SMALL LETTER N WITH DESCENDER
to_title(16#A791)->
    16#A790;
%% LATIN SMALL LETTER C WITH BAR
to_title(16#A793)->
    16#A792;
%% LATIN SMALL LETTER G WITH OBLIQUE STROKE
to_title(16#A7A1)->
    16#A7A0;
%% LATIN SMALL LETTER K WITH OBLIQUE STROKE
to_title(16#A7A3)->
    16#A7A2;
%% LATIN SMALL LETTER N WITH OBLIQUE STROKE
to_title(16#A7A5)->
    16#A7A4;
%% LATIN SMALL LETTER R WITH OBLIQUE STROKE
to_title(16#A7A7)->
    16#A7A6;
%% LATIN SMALL LETTER S WITH OBLIQUE STROKE
to_title(16#A7A9)->
    16#A7A8;
%% FULLWIDTH LATIN SMALL LETTER A
to_title(16#FF41)->
    16#FF21;
%% FULLWIDTH LATIN SMALL LETTER B
to_title(16#FF42)->
    16#FF22;
%% FULLWIDTH LATIN SMALL LETTER C
to_title(16#FF43)->
    16#FF23;
%% FULLWIDTH LATIN SMALL LETTER D
to_title(16#FF44)->
    16#FF24;
%% FULLWIDTH LATIN SMALL LETTER E
to_title(16#FF45)->
    16#FF25;
%% FULLWIDTH LATIN SMALL LETTER F
to_title(16#FF46)->
    16#FF26;
%% FULLWIDTH LATIN SMALL LETTER G
to_title(16#FF47)->
    16#FF27;
%% FULLWIDTH LATIN SMALL LETTER H
to_title(16#FF48)->
    16#FF28;
%% FULLWIDTH LATIN SMALL LETTER I
to_title(16#FF49)->
    16#FF29;
%% FULLWIDTH LATIN SMALL LETTER J
to_title(16#FF4A)->
    16#FF2A;
%% FULLWIDTH LATIN SMALL LETTER K
to_title(16#FF4B)->
    16#FF2B;
%% FULLWIDTH LATIN SMALL LETTER L
to_title(16#FF4C)->
    16#FF2C;
%% FULLWIDTH LATIN SMALL LETTER M
to_title(16#FF4D)->
    16#FF2D;
%% FULLWIDTH LATIN SMALL LETTER N
to_title(16#FF4E)->
    16#FF2E;
%% FULLWIDTH LATIN SMALL LETTER O
to_title(16#FF4F)->
    16#FF2F;
%% FULLWIDTH LATIN SMALL LETTER P
to_title(16#FF50)->
    16#FF30;
%% FULLWIDTH LATIN SMALL LETTER Q
to_title(16#FF51)->
    16#FF31;
%% FULLWIDTH LATIN SMALL LETTER R
to_title(16#FF52)->
    16#FF32;
%% FULLWIDTH LATIN SMALL LETTER S
to_title(16#FF53)->
    16#FF33;
%% FULLWIDTH LATIN SMALL LETTER T
to_title(16#FF54)->
    16#FF34;
%% FULLWIDTH LATIN SMALL LETTER U
to_title(16#FF55)->
    16#FF35;
%% FULLWIDTH LATIN SMALL LETTER V
to_title(16#FF56)->
    16#FF36;
%% FULLWIDTH LATIN SMALL LETTER W
to_title(16#FF57)->
    16#FF37;
%% FULLWIDTH LATIN SMALL LETTER X
to_title(16#FF58)->
    16#FF38;
%% FULLWIDTH LATIN SMALL LETTER Y
to_title(16#FF59)->
    16#FF39;
%% FULLWIDTH LATIN SMALL LETTER Z
to_title(16#FF5A)->
    16#FF3A;
%% DESERET SMALL LETTER LONG I
to_title(16#10428)->
    16#10400;
%% DESERET SMALL LETTER LONG E
to_title(16#10429)->
    16#10401;
%% DESERET SMALL LETTER LONG A
to_title(16#1042A)->
    16#10402;
%% DESERET SMALL LETTER LONG AH
to_title(16#1042B)->
    16#10403;
%% DESERET SMALL LETTER LONG O
to_title(16#1042C)->
    16#10404;
%% DESERET SMALL LETTER LONG OO
to_title(16#1042D)->
    16#10405;
%% DESERET SMALL LETTER SHORT I
to_title(16#1042E)->
    16#10406;
%% DESERET SMALL LETTER SHORT E
to_title(16#1042F)->
    16#10407;
%% DESERET SMALL LETTER SHORT A
to_title(16#10430)->
    16#10408;
%% DESERET SMALL LETTER SHORT AH
to_title(16#10431)->
    16#10409;
%% DESERET SMALL LETTER SHORT O
to_title(16#10432)->
    16#1040A;
%% DESERET SMALL LETTER SHORT OO
to_title(16#10433)->
    16#1040B;
%% DESERET SMALL LETTER AY
to_title(16#10434)->
    16#1040C;
%% DESERET SMALL LETTER OW
to_title(16#10435)->
    16#1040D;
%% DESERET SMALL LETTER WU
to_title(16#10436)->
    16#1040E;
%% DESERET SMALL LETTER YEE
to_title(16#10437)->
    16#1040F;
%% DESERET SMALL LETTER H
to_title(16#10438)->
    16#10410;
%% DESERET SMALL LETTER PEE
to_title(16#10439)->
    16#10411;
%% DESERET SMALL LETTER BEE
to_title(16#1043A)->
    16#10412;
%% DESERET SMALL LETTER TEE
to_title(16#1043B)->
    16#10413;
%% DESERET SMALL LETTER DEE
to_title(16#1043C)->
    16#10414;
%% DESERET SMALL LETTER CHEE
to_title(16#1043D)->
    16#10415;
%% DESERET SMALL LETTER JEE
to_title(16#1043E)->
    16#10416;
%% DESERET SMALL LETTER KAY
to_title(16#1043F)->
    16#10417;
%% DESERET SMALL LETTER GAY
to_title(16#10440)->
    16#10418;
%% DESERET SMALL LETTER EF
to_title(16#10441)->
    16#10419;
%% DESERET SMALL LETTER VEE
to_title(16#10442)->
    16#1041A;
%% DESERET SMALL LETTER ETH
to_title(16#10443)->
    16#1041B;
%% DESERET SMALL LETTER THEE
to_title(16#10444)->
    16#1041C;
%% DESERET SMALL LETTER ES
to_title(16#10445)->
    16#1041D;
%% DESERET SMALL LETTER ZEE
to_title(16#10446)->
    16#1041E;
%% DESERET SMALL LETTER ESH
to_title(16#10447)->
    16#1041F;
%% DESERET SMALL LETTER ZHEE
to_title(16#10448)->
    16#10420;
%% DESERET SMALL LETTER ER
to_title(16#10449)->
    16#10421;
%% DESERET SMALL LETTER EL
to_title(16#1044A)->
    16#10422;
%% DESERET SMALL LETTER EM
to_title(16#1044B)->
    16#10423;
%% DESERET SMALL LETTER EN
to_title(16#1044C)->
    16#10424;
%% DESERET SMALL LETTER ENG
to_title(16#1044D)->
    16#10425;
%% DESERET SMALL LETTER OI
to_title(16#1044E)->
    16#10426;
%% DESERET SMALL LETTER EW
to_title(16#1044F)->
    16#10427;
to_title(C)->
    C.