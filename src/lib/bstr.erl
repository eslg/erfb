%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <jcomellas@novamens.com>
%%% @author Mahesh Paolini-Subramanya <mahesh@aptela.com>
%%% @copyright 2008 Novamens S.A.
%%% @doc String implemented over an Erlang binary.
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(bstr).
-author('Juan Jose Comellas <jcomellas@novamens.com>').
-author('Mahesh Paolini-Subramanya <mahesh@aptela.com>').

-export([len/1, equal/2, concat/2, nth/2, index/2, rindex/2,
         member/2, prefix/2, suffix/2,
         is_alpha/1, is_alpha_char/1, is_alnum/1, is_alnum_char/1, 
         is_lower/1, is_lower_char/1, is_upper/1, is_upper_char/1, 
         is_digit/1, is_digit_char/1, is_xdigit/1, is_xdigit_char/1, 
         is_blank/1, is_blank_char/1, is_space/1, is_space_char/1,
         is_atom_as_binary/1, is_atom_char/1,
         insert/3, duplicate/2, substr/2, substr/3, left/2, right/2,
         pad/2, pad/3, lpad/2, lpad/3, rpad/2, rpad/3,
         strip/1, strip/2, lstrip/1, lstrip/2, rstrip/1, rstrip/2, chomp/1,
         split/2, join/1, join/2, join/3, lower/1, upper/1, bstr/1,
         from_atom/1, to_atom/1, to_existing_atom/1, from_list/1, to_list/1,
         from_integer/1, from_integer/2, from_integer/3,
         to_integer/1, to_integer/2, to_boolean/1,
         integer_to_hex_char/1, integer_to_hex_char/2, hex_char_to_integer/1, 
         get_line/1, urlencode/1, urldecode/1, xmlencode/1, xmldecode/1,
         hexencode/1, hexdecode/1]).

%% @type bstr(). Our main type
-type(bstr() :: binary()).

%%--------------------------------------------------------------------
%% @spec len(bstr()) -> non_neg_integer()
%% @doc  Return the length of a string.
%% @end
%%--------------------------------------------------------------------
-spec len(bstr()) -> non_neg_integer().
len(Str) when is_binary(Str) ->
    size(Str).


%%--------------------------------------------------------------------
%% @spec equal(bstr(), bstr()) -> boolean()
%% @doc  Test if 2 strings are equal.
%% @end
%%--------------------------------------------------------------------
-spec equal(bstr(), bstr()) -> boolean().
equal(Str, Str) when is_binary(Str) ->
    true;
equal(Str, _) when is_binary(Str) ->
    false.


%%--------------------------------------------------------------------
%% @spec concat(bstr(), bstr()) -> bstr()
%% @doc  Concatenate 2 strings.
%% @end
%%--------------------------------------------------------------------
-spec concat(bstr(), bstr()) -> bstr().
concat(Str1, Str2) when is_binary(Str1), is_binary(Str2) ->
    <<Str1/binary, Str2/binary>>.


%%--------------------------------------------------------------------
%% @spec nth(bstr(), pos_integer()) -> char()
%% @doc  Return the character in the nth position of the string.
%% @end
%%--------------------------------------------------------------------
-spec nth(bstr(), pos_integer()) -> char().
nth(Str, Pos) when is_binary(Str), Pos > 0, Pos =< size(Str) ->
    Offset = Pos - 1,
    <<_Head:Offset/binary, Char, _Tail/binary>> = Str,
    Char.


%%--------------------------------------------------------------------
%% @spec index(bstr(), char()) -> integer()
%% @doc  Return the index of the first appearance of a character in a string.
%% @end
%%--------------------------------------------------------------------
-spec index(bstr(), char()) -> integer().
index(Str, Char) when is_binary(Str), is_integer(Char) ->
   index(Str, Char, 0).
index(<<Char, _Tail/binary>>, Char, N) ->
   N;
index(<<_Char, Tail/binary>>, Char, N) ->
   index(Tail, Char, N + 1);
index(<<>>, _Char, _N) ->
    -1.


%%--------------------------------------------------------------------
%% @spec rindex(bstr(), char()) -> integer()
%% @doc  Return the index of the last appearance of a character in a string.
%% @end
%%--------------------------------------------------------------------
-spec rindex(bstr(), char()) -> integer().
rindex(Str, Char) when is_binary(Str), is_integer(Char) ->
    rindex(Str, Char, size(Str) - 1).
rindex(Str, Char, Offset) ->
    case Str of
        <<_Head:Offset/binary, Char, _Tail/binary>> ->
            Offset;
        <<_Head:Offset/binary, _Char, _Tail/binary>> ->
            rindex(Str, Char, Offset - 1);
        _ ->
            -1
    end.


%%--------------------------------------------------------------------
%% @spec member(bstr(), char()) -> boolean()
%% @doc  Return whether the character is present in the string.
%% @end
%%--------------------------------------------------------------------
-spec member(bstr(), char()) -> boolean().
member(Str, Char) ->
    index(Str, Char) >= 0.


%%--------------------------------------------------------------------
%% @spec prefix(bstr(), bstr()) -> boolean()
%% @doc  Indicates whether a string is a prefix of another one.
%% @end
%%--------------------------------------------------------------------
-spec prefix(bstr(), bstr()) -> boolean().
prefix(Str, Prefix) when is_binary(Str), is_binary(Prefix) ->
    N = size(Prefix),
    case Str of
        <<Prefix:N/binary, _Tail/binary>> ->
            true;
        _ ->
            false
    end.


%%--------------------------------------------------------------------
%% @spec suffix(bstr(), bstr()) -> boolean()
%% @doc  Indicates whether a string is a suffix of another one.
%% @end
%%--------------------------------------------------------------------
-spec suffix(bstr(), bstr()) -> boolean().
suffix(Str, Suffix) when is_binary(Str), is_binary(Suffix) ->
    N = size(Str) - size(Suffix),
    case Str of
        <<_Head:N/binary, Suffix/binary>> ->
            true;
        _ ->
            false
    end.


%%--------------------------------------------------------------------
%% @spec is_alpha(bstr()) -> boolean()
%% @doc  Determines if a string is composed of alphabetic characters.
%% @end
%%--------------------------------------------------------------------
-spec is_alpha(bstr()) -> boolean().
is_alpha(<<>>) ->
    false;
is_alpha(Str) when is_binary(Str) ->
    is_x(Str, fun is_alpha_char/1);
is_alpha(Char) when is_integer(Char) ->
    is_alpha_char(Char).

%% @doc Determine if a character is alphabetic.
is_alpha_char(Char) ->
    ((Char >= $A) andalso (Char =< $Z)) orelse
    ((Char >= $a) andalso (Char =< $z)).


%%--------------------------------------------------------------------
%% @spec is_alnum(bstr()) -> boolean()
%% @doc  Determines if a string is composed of alphanumeric characters.
%% @end
%%--------------------------------------------------------------------
-spec is_alnum(bstr()) -> boolean().
is_alnum(<<>>) ->
    false;
is_alnum(Str) when is_binary(Str) ->
    is_x(Str, fun is_alnum_char/1);
is_alnum(Char) when is_integer(Char) ->
    is_alnum_char(Char).

%% @doc Determine if a character is alphanumeric.
-spec is_alnum_char(char()) -> boolean().
is_alnum_char(Char) ->
    ((Char >= $A) andalso (Char =< $Z)) orelse
    ((Char >= $a) andalso (Char =< $z)) orelse
    ((Char >= $0) andalso (Char =< $9)).


%%--------------------------------------------------------------------
%% @spec is_lower(bstr()) -> boolean()
%% @doc  Determines if a string is composed of lower-case alphabetic characters.
%% @end
%%--------------------------------------------------------------------
-spec is_lower(bstr()) -> boolean().
is_lower(<<>>) ->
    false;
is_lower(Str) when is_binary(Str) ->
    is_x(Str, fun is_lower_char/1);
is_lower(Char) when is_integer(Char) ->
    is_lower_char(Char).

%% @doc Determine if a character is lower-case.
-spec is_lower_char(char()) -> boolean().
is_lower_char(Char) ->
    ((Char >= $a) andalso (Char =< $z)).


%%--------------------------------------------------------------------
%% @spec is_upper(bstr()) -> boolean()
%% @doc  Determines if a string is composed of upper-case alphabetic characters.
%% @end
%%--------------------------------------------------------------------
-spec is_upper(bstr()) -> boolean().
is_upper(<<>>) ->
    false;
is_upper(Str) when is_binary(Str) ->
    is_x(Str, fun is_upper_char/1);
is_upper(Char) when is_integer(Char) ->
    is_upper_char(Char).

%% @doc Determine if a character is upper-case.
-spec is_upper_char(char()) -> boolean().
is_upper_char(Char) ->
    ((Char >= $A) andalso (Char =< $Z)).


%%--------------------------------------------------------------------
%% @spec is_digit(bstr()) -> boolean()
%% @doc  Determines if a string is composed of digits.
%% @end
%%--------------------------------------------------------------------
-spec is_digit(bstr()) -> boolean().
is_digit(<<>>) ->
    false;
is_digit(Str) when is_binary(Str) ->
    is_x(Str, fun is_digit_char/1);
is_digit(Char) when is_integer(Char) ->
    is_digit_char(Char).

%% @doc Determine if a character is a digit.
-spec is_digit_char(char()) -> boolean().
is_digit_char(Char) ->
    ((Char >= $0) andalso (Char =< $9)).


%%--------------------------------------------------------------------
%% @spec is_xdigit(bstr()) -> boolean()
%% @doc  Determines if a string is composed of hexadecimal digits.
%% @end
%%--------------------------------------------------------------------
-spec is_xdigit(bstr()) -> boolean().
is_xdigit(<<>>) ->
    false;
is_xdigit(Str) when is_binary(Str) ->
    is_x(Str, fun is_xdigit_char/1);
is_xdigit(Char) when is_integer(Char) ->
    is_xdigit_char(Char).

%% @doc Determine if a character is a digit.
-spec is_xdigit_char(char()) -> boolean().
is_xdigit_char(Char) ->
    ((Char >= $0) andalso (Char =< $9)) orelse
    ((Char >= $A) andalso (Char =< $F)) orelse
    ((Char >= $a) andalso (Char =< $f)).


%%--------------------------------------------------------------------
%% @spec is_blank(bstr()) -> boolean()
%% @doc  Determines if a string is composed of blank characters.
%% @end
%%--------------------------------------------------------------------
-spec is_blank(bstr()) -> boolean().
is_blank(<<>>) ->
    false;
is_blank(Str) when is_binary(Str) ->
    is_x(Str, fun is_blank_char/1);
is_blank(Char) when is_integer(Char) ->
    is_blank_char(Char).

%% @doc Determine if a character is blank (\n, \r, \t, \f, \v).
-spec is_blank_char(char()) -> boolean().
is_blank_char(Char) ->
    ((Char =:= $\s) orelse (Char =:= $\t)).


%%--------------------------------------------------------------------
%% @spec is_space(bstr()) -> boolean()
%% @doc  Determines if a string is composed of spaces or tabs.
%% @end
%%--------------------------------------------------------------------
-spec is_space(bstr()) -> boolean().
is_space(<<>>) ->
    false;
is_space(Str) when is_binary(Str) ->
    is_x(Str, fun is_space_char/1);
is_space(Char) when is_integer(Char) ->
    is_space_char(Char).

%% @doc Determine if a character is a space or a tab.
-spec is_space_char(char()) -> boolean().
is_space_char(Char) ->
    ((Char =:= $\s) orelse (Char =:= $\n) orelse (Char =:= $\r) orelse
     (Char =:= $\t) orelse (Char =:= $\f) orelse (Char =:= $\v)).


%%--------------------------------------------------------------------
%% @spec is_atom_as_binary(bstr()) -> boolean()
%% @doc  Determines if a string is an unquoted atom.
%% @end
%%--------------------------------------------------------------------
-spec is_atom_as_binary(bstr()) -> boolean().
is_atom_as_binary(<<>>) ->
    false;
is_atom_as_binary(<<Char, Tail/binary>>) ->
    is_lower_char(Char) andalso is_x(Tail, fun is_atom_char/1);
is_atom_as_binary(Char) when is_integer(Char) ->
    is_atom_char(Char).

%% @doc Determine if a character is lower case, numeric, '_' or '@'.
-spec is_atom_char(char()) -> boolean().
is_atom_char(Char) ->
    ((Char >= $a) andalso (Char =< $z)) orelse
    ((Char >= $0) andalso (Char =< $9)) orelse
    (Char =:= $_) orelse
    (Char =:= $@).


%%--------------------------------------------------------------------
%% @spec is_x(Str::bstr(), Fun::fun((char()) -> boolean())) -> boolean()
%% @doc  Helper function used to check whether all the characters in a string
%%       meet a specific criteria that is passed as a function to it.
%% @end
%%--------------------------------------------------------------------
%% is_x(<<>>, _Fun, _Offset) ->
%%     false;
%% is_x(Str, Fun, Offset) ->
%%     case Str of
%%         <<_Head:Offset/binary, Char, _Tail/binary>> ->
%%             case Fun(Char) of
%%                 true ->
%%                     is_x(Str, Fun, Offset + 1);
%%                 false ->
%%                     false
%%             end;
%%         %% If we reach the end we have a string composed entirely of characters
%%         %% that meet the criteria specified in the Fun().
%%         _ ->
%%             true
%%     end.
%% This version is about 5% faster than the one above. Re-test once Erlang R12B
%% is released.
-spec is_x(Str::bstr(), Fun::fun((char()) -> boolean())) -> boolean().
is_x(<<Char, Tail/binary>>, Fun) ->
    case Fun(Char) of
        true ->
            is_x(Tail, Fun);
        false ->
            false
    end;
is_x(<<>>, _Fun) ->
    true.


%%--------------------------------------------------------------------
%% @spec insert(bstr(), pos_integer(), bstr()) -> bstr()
%% @doc  Insert a string into another one at the indicated position.
%% @end
%%---------------------------------------------------------------------
-spec insert(bstr(), pos_integer(), bstr()) -> bstr().
insert(Str, Pos, Str1) when is_binary(Str), is_integer(Pos) ->
    N = Pos - 1,
    case Str of
        <<Head:N/binary, Tail/binary>> ->
            <<Head/binary, Str1/binary, Tail/binary>>;
        _ ->
            erlang:error(badarg)
    end.


%%--------------------------------------------------------------------
%% @spec duplicate(bstr(), integer()) -> bstr()
%% @doc  Return 'Number' copies of a string.
%% @end
%%--------------------------------------------------------------------
-spec duplicate(bstr(), integer()) -> bstr().
duplicate(Str, Num) ->
    duplicate(Str, Num, []).
duplicate(Str, Num, Acc) when Num > 0 ->
    duplicate(Str, Num - 1, [Str | Acc]);
duplicate(_Str, _Num, Acc) ->
    erlang:list_to_binary(Acc).


%%--------------------------------------------------------------------
%% @spec substr(bstr(), integer()) -> bstr()
%% @doc  Return a substring starting at position 'Pos'.
%% @end
%%--------------------------------------------------------------------
-spec substr(bstr(), integer()) -> bstr().
substr(Str, 1) when is_binary(Str) ->
    Str;
substr(Str, Pos) when is_binary(Str) ->
    N = Pos - 1,
    case Str of
        <<_Head:N/binary, Substr/binary>> ->
            Substr;
        _ ->
            <<>>
    end.

%%--------------------------------------------------------------------
%% @spec substr(bstr(), Pos::integer(), Len::integer()) -> bstr()
%% @doc  Return a substring starting at position 'Pos' with a length of 'Len' bytes.
%% @end
%%--------------------------------------------------------------------
-spec substr(bstr(), Pos::integer(), Len::integer()) -> bstr().
substr(Str, 1, Len) when is_binary(Str), Len =:= size(Str) ->
    Str;
substr(Str, Pos, Len) when is_binary(Str) ->
    N = Pos - 1,
    case Str of
        <<_Head:N/binary, Substr:Len/binary, _Tail/binary>> ->
            Substr;
        <<_Head:N/binary, Substr/binary>> ->
            Substr;
        _ ->
            <<>>
    end.


%%--------------------------------------------------------------------
%% @spec left(bstr(), integer()) -> bstr()
%% @doc  Return a substring of 'Len' bytes starting from the beginning of the 
%%       string. If the string does not have enough characters, the original 
%%       string is returned.
%% @end
%%--------------------------------------------------------------------
-spec left(bstr(), integer()) -> bstr().
left(Str, Len) when is_binary(Str), Len >= size(Str) ->
    Str;
left(Str, Len) when is_binary(Str), Len >= 0 ->
    <<Left:Len/binary, _Tail/binary>> = Str,
    Left.


%%--------------------------------------------------------------------
%% @spec right(bstr(), integer()) -> bstr()
%% @doc  Return a substring of 'Len' bytes starting from the end of the string.
%%       If the string does not have enough characters, the original string is
%%       returned.
%% @end
%%--------------------------------------------------------------------
-spec right(bstr(), integer()) -> bstr().
right(Str, Len) when is_binary(Str), Len >= size(Str) ->
    Str;
right(Str, Len) when is_binary(Str), Len >= 0 ->
    Offset = size(Str) - Len,
    <<_Head:Offset/binary, Right/binary>> = Str,
    Right.


%%--------------------------------------------------------------------
%% @spec pad(bstr(), non_neg_integer()) -> bstr()
%% @doc  Return a string of 'Len' bytes padded with spaces to the left and to the right.
%% @end
%%--------------------------------------------------------------------
-spec pad(bstr(), non_neg_integer()) -> bstr().
pad(Str, Len) when is_binary(Str), Len >= 0 ->
    pad(Str, Len, $\s).

%%--------------------------------------------------------------------
%% @spec pad(bstr(), integer(), char()) -> bstr()
%% @doc  Return a string of 'Len' bytes padded with 'Chars' to the left and to the right.
%% @end
%%--------------------------------------------------------------------
-spec pad(bstr(), integer(), char()) -> bstr().
pad(Str, Len, Char) when is_binary(Str), Len >= 0 ->
    PadLen = Len - size(Str),
    if
        PadLen > 0 ->
            LeftPadLen = PadLen div 2,
            RightPadLen = PadLen - LeftPadLen,
            Padding = duplicate(Char, LeftPadLen),
            if
                RightPadLen > 0 ->
                    if
                        LeftPadLen =:= RightPadLen ->
                            <<Padding/binary, Str/binary, Padding/binary>>;
                        true ->
                            <<Padding/binary, Str/binary, Padding/binary, Char/binary>>
                    end;
                true ->
                    <<Padding/binary, Str/binary>>
            end;
        true ->
            Str
    end.


%%--------------------------------------------------------------------
%% @spec lpad(bstr(), non_neg_integer()) -> bstr()
%% @doc  Return a string of 'Len' bytes left-padded with spaces.
%% @end
%%--------------------------------------------------------------------
-spec lpad(bstr(), non_neg_integer()) -> bstr().
lpad(Str, Len) when is_binary(Str), Len >= 0 ->
    lpad(Str, Len, $\s).

%%--------------------------------------------------------------------
%% @spec lpad(bstr(), non_neg_integer(), char()) -> bstr()
%% @doc  Return a string of 'Len' bytes left-padded with 'Chars'.
%% @end
%%--------------------------------------------------------------------
-spec lpad(bstr(), non_neg_integer(), char()) -> bstr().
lpad(Str, Len, Char) when is_binary(Str), Len >= 0 ->
    PadLen = Len - size(Str),
    if
        PadLen > 0 ->
            Padding = duplicate(Char, PadLen),
            <<Padding/binary, Str/binary>>;
        true ->
            Str
    end.


%%--------------------------------------------------------------------
%% @spec rpad(bstr(), non_neg_integer()) -> bstr()
%% @doc  Return a string of 'Len' bytes right-padded with spaces.
%% @end
%%--------------------------------------------------------------------
-spec rpad(bstr(), non_neg_integer()) -> bstr().
rpad(Str, Len) when is_binary(Str), Len >= 0 ->
    rpad(Str, Len, $\s).

%%--------------------------------------------------------------------
%% @spec rpad(bstr(), non_neg_integer(), char()) -> bstr()
%% @doc  Return a string of 'Len' bytes right-padded with 'Chars'.
%% @end
%%--------------------------------------------------------------------
-spec rpad(bstr(), non_neg_integer(), char()) -> bstr().
rpad(Str, Len, Char) ->
    PadLen = Len - size(Str),
    if
        PadLen > 0 ->
            Padding = duplicate(Char, PadLen),
            <<Str/binary, Padding/binary>>;
        true ->
            Str
    end.


%%--------------------------------------------------------------------
%% @spec strip(bstr()) -> bstr()
%% @doc  Remove all the spaces present both to the left and to the right of the string.
%% @end
%%--------------------------------------------------------------------
-spec strip(bstr()) -> bstr().
strip(Str) when is_binary(Str) ->
    strip(Str, $\s).

%%--------------------------------------------------------------------
%% @spec strip(bstr(), char()) -> bstr()
%% @doc  Remove all the 'Chars' present both to the left and to the right of the string.
%% @end
%%--------------------------------------------------------------------
-spec strip(bstr(), char()) -> bstr().
strip(Str, Char) when is_binary(Str), is_integer(Char) ->
    rstrip(lstrip(Str, Char), Char).


%%--------------------------------------------------------------------
%% @spec lstrip(bstr()) -> bstr()
%% @doc  Remove all the spaces present to the left of the string.
%% @end
%%--------------------------------------------------------------------
-spec lstrip(bstr()) -> bstr().
lstrip(Str) when is_binary(Str) ->
    lstrip(Str, $\s).

%%--------------------------------------------------------------------
%% @spec lstrip(bstr(), char()) -> bstr()
%% @doc  Remove all the 'Chars' present to the left of the string.
%% @end
%%--------------------------------------------------------------------
-spec lstrip(bstr(), char()) -> bstr().
lstrip(Str, Char) when is_binary(Str), is_integer(Char) ->
    lstrip(Str, Char, 0).
%% @hidden
lstrip(Str, Char, Pos) ->
    case Str of
        <<_Head:Pos/binary, Char, _Tail/binary>> ->
            lstrip(Str, Char, Pos + 1);
        <<_Head:Pos/binary, Tail/binary>> ->
            Tail;
        _ ->
            <<>>
    end.


%%--------------------------------------------------------------------
%% @spec rstrip(bstr()) -> bstr()
%% @doc  Remove all the spaces present to the right of the string.
%% @end
%%--------------------------------------------------------------------
-spec rstrip(bstr()) -> bstr().
rstrip(Str) when is_binary(Str) ->
    rstrip(Str, $\s).

%%--------------------------------------------------------------------
%% @spec rstrip(bstr(), char()) -> bstr()
%% @doc  Remove all the 'Chars' present to the right of the string.
%% @end
%%--------------------------------------------------------------------
-spec rstrip(bstr(), char()) -> bstr().
rstrip(Str, Char) when is_binary(Str), is_integer(Char) ->
    rstrip(Str, Char, size(Str) - 1).
rstrip(Str, Char, Pos) ->
    case Str of
        <<_Head:Pos/binary, Char, _Tail/binary>> ->
            rstrip(Str, Char, Pos - 1);
        <<_Head:Pos/binary, _Tail/binary>> ->
            N = Pos + 1,
            <<Stripped:N/binary, _Dummy/binary>> = Str,
            Stripped;
        _ ->
            <<>>
    end.


%%--------------------------------------------------------------------
%% @spec chomp(bstr()) -> bstr()
%% @doc  Remove all the newlines (\r and \n) present at the end of the string.
%% @end
%%--------------------------------------------------------------------
-spec chomp(bstr()) -> bstr().
chomp(Str) when is_binary(Str) ->
    chomp(Str, size(Str) - 1).
chomp(Str, Pos) ->
    case Str of
        <<_Head:Pos/binary, $\n, _Tail/binary>> ->
            chomp(Str, Pos - 1);
        <<_Head:Pos/binary, $\r, _Tail/binary>> ->
            chomp(Str, Pos - 1);
        <<_Head:Pos/binary, _Tail/binary>> ->
            N = Pos + 1,
            <<Stripped:N/binary, _Whitespace/binary>> = Str,
            Stripped;
        _ ->
            <<>>
    end.


%%--------------------------------------------------------------------
%% @spec split(bstr(), Sep::char() | bstr()) -> list(bstr())
%% @doc  Divide a string into a list of tokens that were originally separated
%%       by the character 'Sep'.
%% @end
%%--------------------------------------------------------------------
-spec split(bstr(), Sep::char() | bstr()) -> list(bstr()).
split(<<>>, _Sep) ->
    [];
split(Str, Sep) when is_binary(Str), is_integer(Sep) ->
    lists:reverse(split_char_sep(Str, Sep, 0, []));
split(Str, Sep) when is_binary(Str), is_binary(Sep) ->
    Tokens = 
        case Sep of
            <<Char>> ->
                split_char_sep(Str, Char, 0, []);
            _ ->
                split_str_sep(Str, Sep, 0, [])
        end,
    lists:reverse(Tokens).

%% @doc Helper function used to tokenize a string when the separator is a character.
-spec split_char_sep(bstr(), char(), integer(), [bstr()]) -> [bstr()].
split_char_sep(Str, Sep, Pos, Tokens) ->
    case Str of
        <<Token:Pos/binary, Sep, Tail/binary>> ->
            split_char_sep(Tail, Sep, 0, [Token | Tokens]);
        <<_Head:Pos/binary, _Tail/binary>> ->
            split_char_sep(Str, Sep, Pos + 1, Tokens);
        _ ->
            [Str | Tokens]
    end.

%% @doc Helper function used to tokenize a string when there are multiple separators.
-spec split_str_sep(bstr(), bstr(), non_neg_integer(), [bstr()]) -> [bstr(),...].
split_str_sep(Str, Sep, Pos, Tokens) ->
    case Str of
        <<Token:Pos/binary, Char, Tail/binary>> ->
            Index = index(Sep, Char),
            if
                Index >= 0 ->
                    split_str_sep(Tail, Sep, 0, [Token | Tokens]);
                true ->
                    split_str_sep(Str, Sep, Pos + 1, Tokens)
            end;
        _ ->
            [Str|Tokens]
    end.


%%--------------------------------------------------------------------
%% @spec join(list(bstr())) -> bstr()
%% @doc  Join a a list of strings into one string.
%% @end
%%--------------------------------------------------------------------
% join(List) when is_list(List) ->
%     join_list(List, <<>>).
% join_list([Head|Tail], Acc) ->
%     Value = bstr(Head),
%     join_list(Tail, <<Acc/binary, Value/binary>>);
% join_list([], Acc) ->
%     Acc.
%% This version is about 30% faster than the one above. Re-test once Erlang R12B
%% is released. This test was performed before adding support for deep lists.
-spec join(list(bstr())) -> bstr().
join(List) when is_list(List) ->
    list_to_binary(join_list(List, [])).
join_list([Head | Tail], Acc) when is_atom(Head) ->
    join_list(Tail, [atom_to_list(Head) | Acc]);
join_list([Head | Tail], Acc) when is_list(Head) ->
    join_list(Tail, [join_list(Head, []) | Acc]);
join_list([Head | Tail], Acc) ->
    join_list(Tail, [Head | Acc]);
join_list([], Acc) ->
    lists:reverse(Acc).


%%--------------------------------------------------------------------
%% @spec join(list(bstr()), Sep::char() | bstr()) -> bstr()
%% @doc  Join a a list of strings into one string, adding a separator between
%%       each string.
%% @end
%%--------------------------------------------------------------------
-spec join(list(bstr()), Sep::char() | bstr()) -> bstr().
join(List, Sep) when is_list(List) ->
    list_to_binary(join_list_sep(List, Sep)).

-spec join_list_sep([any()], char() | bstr()) -> [any()].
join_list_sep([Head | Tail], Sep) when is_atom(Head) ->
    join_list_sep(Tail, Sep, [atom_to_list(Head)]);
join_list_sep([Head | Tail], Sep) when is_list(Head) ->
    join_list_sep(Tail, Sep, [join_list(Head, [])]);
join_list_sep([Head | Tail], Sep) ->
    join_list_sep(Tail, Sep, [Head]);
join_list_sep([], _Sep) ->
    [].
join_list_sep([Head | Tail], Sep, Acc) when is_atom(Head) ->
    join_list_sep(Tail, Sep, [atom_to_list(Head), Sep | Acc]);
join_list_sep([Head | Tail], Sep, Acc) when is_list(Head) ->
    join_list_sep(Tail, Sep, [join_list(Head, []), Sep | Acc]);
join_list_sep([Head | Tail], Sep, Acc) ->
    join_list_sep(Tail, Sep, [Head, Sep | Acc]);
join_list_sep([], _Sep, Acc) ->
    lists:reverse(Acc).


%%--------------------------------------------------------------------
%% @spec join(Members :: list(bstr()),
%%            Sep     :: char() | bstr(),
%%            Esc     :: char() ) -> bstr()
%% @doc  Join a a list of strings into one string, adding a separator between
%%       each string and escaping both the separator and the escape char itself
%%       with the escape char.
%% @end
%% E.g.:
%%       `bstr:join([<<"1">>, <<",">>, <<"\1">>, <<"2,3">>], $,, $\) ->`
%%       `       <<"1,\,,\\1,2\,3">>.`
%%--------------------------------------------------------------------
-spec join([bstr()], char() | bstr(), char()) -> bstr().
join(Members, Sep, Esc) ->
    EscStr =
        case Esc of
            $\\ ->
                "\\\\";
            $& ->
                "\\&";
            OtherEsc ->
                [OtherEsc]
        end,
    SepStr =
        case Sep of
            $\\ ->
                "\\\\";
            $& ->
                "\\&";
            OtherSep ->
                [OtherSep]
        end,
    {ok, Find}  = re:compile([$[ | SepStr ++ "]|[" ++ EscStr ++ "]"]), %% "[sep]|[esc]"
    Replace     = EscStr ++ "&",
    bstr:join(
      lists:map(fun(Member) when is_atom(Member) ->
                        re:replace(atom_to_list(Member),
                                   Find,
                                   Replace,
                                   [{return, binary}, global]);
                   (Member) ->
                        re:replace(Member,
                                   Find,
                                   Replace,
                                   [{return, binary}, global])
                end, Members),
      Sep).

%%--------------------------------------------------------------------
%% @spec lower(bstr() | char()) -> bstr() | char()
%% @doc  Convert all the characters in a bstr to lowercase.
%% @end
%%--------------------------------------------------------------------
-spec lower(bstr() | char()) -> bstr() | char().
lower(Str) when is_binary(Str) ->
    lower_nocopy(Str, 0);
lower(Char) when is_integer(Char) ->
    case is_upper_char(Char) of
        true ->
            Char - $A + $a;
        false ->
            Char
    end.

%% The first part scans the string to see if it finds an upper-case character.
%% If it finds one, it then switches to the version of the function that copies
%% each character.
lower_nocopy(Str, N) ->
    case Str of
        <<Head:N/binary, Char, Tail/binary>> ->
            case is_upper_char(Char) of
                true ->
                    Lower = Char - $A + $a,
                    lower_copy(Tail, [Lower, Head]);
                false ->
                    lower_nocopy(Str, N + 1)
            end;
        _ ->
            Str
    end.
%% This part accumulates each of the characters in a list.
lower_copy(<<Char, Tail/binary>>, Acc) ->
    case is_upper_char(Char) of
        true ->
            lower_copy(Tail, [(Char - $A + $a) | Acc]);
        false ->
            lower_copy(Tail, [Char | Acc])
    end;
lower_copy(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc)).


%%--------------------------------------------------------------------
%% @spec upper(bstr() | char()) -> bstr() | char()
%% @doc  Convert all the characters in a bstr to uppercase.
%% @end
%%--------------------------------------------------------------------
-spec upper(bstr() | char()) -> bstr() | char().
upper(Str) when is_binary(Str) ->
    upper_nocopy(Str, 0);
upper(Char) when is_integer(Char) ->
    case is_lower_char(Char) of
        true ->
            Char - $a + $A;
        false ->
            Char
    end.
%% The first part scans the string to see if it finds a lower-case character.
%% If it finds one, it then switches to the version of the function that copies
%% each character.
upper_nocopy(Str, N) ->
    case Str of
        <<Head:N/binary, Char, Tail/binary>> ->
            case is_lower_char(Char) of
                true ->
                    Upper = Char - $a + $A,
                    upper_copy(Tail, [Upper, Head]);
                false ->
                    upper_nocopy(Str, N + 1)
            end;
        _ ->
            Str
    end.
%% This part accumulates each of the characters in a list.
upper_copy(<<Char, Tail/binary>>, Acc) ->
    case is_lower_char(Char) of
        true ->
            upper_copy(Tail, [(Char - $a + $A) | Acc]);
        false ->
            upper_copy(Tail, [Char | Acc])
    end;
upper_copy(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc)).



%%--------------------------------------------------------------------
%% @spec bstr(binary() | atom() | list() | char()) -> bstr()
%% @doc  Convert an "object" to a bstr.
%% @end
%%--------------------------------------------------------------------
-spec bstr(binary() | atom() | list() | char()) -> bstr().
bstr(Bin) when is_binary(Bin) ->
    Bin;
bstr(Pid) when is_pid(Pid) ->
    list_to_binary(pid_to_list(Pid));
bstr(Atom) when is_atom(Atom) ->
    list_to_binary(atom_to_list(Atom));
bstr(Integer) when is_integer(Integer) ->
    <<Integer>>;
bstr(List) when is_list(List) ->
    list_to_binary(List).


%%--------------------------------------------------------------------
%% @spec from_atom(atom()) -> bstr()
%% @doc  Convert an atom to a bstr.
%% @end
%%--------------------------------------------------------------------
-spec from_atom(atom()) -> bstr().
from_atom(Atom) when is_atom(Atom) ->
    list_to_binary(atom_to_list(Atom)).

%%--------------------------------------------------------------------
%% @spec to_atom(bstr()) -> atom()
%% @doc  Convert a bstr containing a string to an Erlang atom.
%% @end
%%--------------------------------------------------------------------
-spec to_atom(bstr()) -> atom().
to_atom(Str) when is_binary(Str) ->
    list_to_atom(binary_to_list(Str)).

%%--------------------------------------------------------------------
%% @spec to_existing_atom(bstr()) -> atom()
%% @doc  Convert a bstr containing a string to an Erlang atom only if the atom
%%       already existed (i.e. had been previously defined).
%% @end
%%--------------------------------------------------------------------
-spec to_existing_atom(bstr()) -> atom().
to_existing_atom(Str) when is_binary(Str) ->
    list_to_existing_atom(binary_to_list(Str)).


%%--------------------------------------------------------------------
%% @spec from_list(list()) -> bstr()
%% @doc  Convert a list containing a string to a bstr.
%% @end
%%--------------------------------------------------------------------
-spec from_list(list()) -> bstr().
from_list(List) when is_list(List) ->
    list_to_binary(List).


%%--------------------------------------------------------------------
%% @spec to_list(bstr()) -> string()
%% @doc  Convert a bstr containing a string to an Erlang list/string.
%% @end
%%--------------------------------------------------------------------
-spec to_list(bstr()) -> [byte()].
to_list(Str) when is_binary(Str) ->
    binary_to_list(Str).


%%--------------------------------------------------------------------
%% @spec to_boolean(bstr()) -> boolean()
%% @doc  Convert a bstr containing a string to an Erlang list/string.
%% @end
%%--------------------------------------------------------------------
-spec to_boolean(bstr()) -> boolean().
to_boolean(<<"true">>) ->
    true;
to_boolean(<<"false">>) ->
    false.


%%--------------------------------------------------------------------
%% @spec from_integer(integer()) -> bstr()
%% @doc  Convert an integer to a bstr in base 10 format.
%% @end
%%--------------------------------------------------------------------
-spec from_integer(integer()) -> bstr().
from_integer(I) ->
    from_integer(I, 10, upper).


%%--------------------------------------------------------------------
%% @spec from_integer(integer(), pos_integer()) -> bstr()
%% @doc  Convert an integer to a bstr in base 'n' format.
%% @end
%%--------------------------------------------------------------------
-spec from_integer(integer(), 1..255) -> bstr().
from_integer(I, Base) ->
    from_integer(I, Base, upper).

%%--------------------------------------------------------------------
%% @spec from_integer(integer(), pos_integer(), upper | lower) -> bstr()
%% @doc  Convert an integer to a bstr in base 'n' format in the specified case.
%% @end
%%--------------------------------------------------------------------
-spec from_integer(integer(), 1..255, upper | lower) -> bstr().
from_integer(I, Base, Case) when is_integer(I), is_integer(Base), Base >= 2, Base =< 1 + $Z - $A + 10 ->
    BaseLetter = case Case of
                     upper ->
                         $A;
                     lower ->
                         $a
                 end,

    list_to_binary(
      if
          I < 0 ->
              [$- | from_integer(-I, Base, BaseLetter, [])];
          true ->
              from_integer(I, Base, BaseLetter, [])
      end
     );
from_integer(I, Base, Case) ->
    erlang:error(badarg, [I, Base, Case]).

%% Helper function to convert an integer to a base 'n' representation.
-spec from_integer(integer(), pos_integer(), 65 | 97, [integer()]) -> [integer(),...].
from_integer(I0, Base, BaseLetter, Acc) ->
    I1 = I0 div Base,
    Digit = I0 rem Base,

    Acc1 = [
            if 
                (Digit >= 10) -> 
                    Digit - 10 + BaseLetter; 
                true ->
                    Digit + $0 
            end | Acc
           ],
    if
        I1 =:= 0 ->
            Acc1;
        true ->
            from_integer(I1, Base, BaseLetter, Acc1)
    end.


%%--------------------------------------------------------------------
%% @spec to_integer(bstr()) -> integer()
%% @doc  Convert a bstr containing a string representing a decimal number
%%       to an integer.
%% @end
%%--------------------------------------------------------------------
-spec to_integer(bstr()) -> integer().
to_integer(<<$-, Str/binary>>) ->
    -to_decimal_integer(Str, 0, 0);
to_integer(<<$+, Str/binary>>) ->
    to_decimal_integer(Str, 0, 0);
to_integer(<<>>) ->
    erlang:error(badarg, [<<>>]);
to_integer(Str) when is_binary(Str) ->
    to_decimal_integer(Str, 0, 0).
%% Part of the function converts the string into a base-10 number
to_decimal_integer(Str, Offset, Acc) ->
    case Str of
        <<_Head:Offset/binary, Char, _Tail/binary>> when (Char >= $0) and (Char =< $9) ->
            to_decimal_integer(Str, Offset + 1, Acc * 10 + (Char - $0));
        <<_Head:Offset/binary>> ->
            Acc;
        _Number ->
            %% We throw the same exception thrown by list_to_integer() if a
            %% non-numeric character is found
            erlang:error(badarg, [Str])
    end.


%%--------------------------------------------------------------------
%% @spec to_integer(bstr(), pos_integer()) -> integer()
%% @doc  Convert a bstr containing a string representing a positive number
%%       in the specified 'Base' to an integer. 'Base' must be an integer
%%       between 2 and 32.
%% @end
%%--------------------------------------------------------------------
% Optimized version for base 10
-spec to_integer(bstr(), 1..255) -> integer().
to_integer(Str, 10) ->
    to_integer(Str);
to_integer(Str, Base) when is_integer(Base), Base >= 2, Base =< 1 + $Z - $A + 10 ->
    case Str of
        <<$-, Number/binary>> ->
            -to_base_n_integer(Number, 0, Base, 0);
        <<$+, Number/binary>> ->
            to_base_n_integer(Number, 0, Base, 0);
        <<>> ->
            erlang:error(badarg, [<<>>]);
        Number ->
            to_base_n_integer(Number, 0, Base, 0)
    end;
to_integer(Str, Base) ->
    erlang:error(badarg, [Str, Base]).
% Generic version for the rest of the bases
to_base_n_integer(Str, Offset, Base, Acc) ->
    case Str of
        <<_Head:Offset/binary, Char, _Tail/binary>> ->
            N = 
                if
                    (Char >= $0) and (Char =< $9) and (Char < Base + $0) ->
                        (Char - $0);
                    (Char >= $A) and (Char =< $Z) and (Char < Base + $A) ->
                        10 + (Char - $A);
                    (Char >= $a) and (Char =< $z) and (Char < Base + $a) ->
                        10 + (Char - $a);
                    true ->
                        %% We throw the same exception thrown by list_to_integer() if an
                        %% invalid character is found
                        erlang:error(badarg, [Str, Base]),
                        %% To avoid compiler warning
                        0
                end,
            to_base_n_integer(Str, Offset + 1, Base, Acc * Base + N);

        <<_Head:Offset/binary>> ->
            Acc;

        _Number ->
            %% We throw the same exception thrown by list_to_integer() if a
            %% non-numeric character is found
            erlang:error(badarg, [Str])
    end.


%%--------------------------------------------------------------------
%% @spec get_line(bstr()) -> {bstr(), bstr()}
%% @doc  Get the first text line from a binary string. It returns a tuple with
%%       the first text line as the first element and the rest of the string as
%%       the last element.
%% @end
%%--------------------------------------------------------------------
-spec get_line(bstr()) -> {bstr(), bstr()}.
get_line(Str) ->
    get_line(Str, 0).
get_line(Str, Offset) ->
    case Str of
        <<Line:Offset/binary, $\n, Tail/binary>> ->
            {Line, Tail};
        <<Line:Offset/binary, $\r, $\n, Tail/binary>> ->
            {Line, Tail};
        Line when Offset >= size(Line) ->
            {Str, <<>>};
        _ ->
            get_line(Str, Offset + 1)
    end.


%%--------------------------------------------------------------------
%% @spec urlencode(bstr()) -> bstr()
%% @doc  Encode a bstr using the URL-encoding scheme.
%% @end
%%--------------------------------------------------------------------
-spec urlencode(bstr()) -> bstr().
urlencode(Str) when is_binary(Str) ->
    urlencode(Str, 0).
%% This part of the function iterates over the bstr without copying any data
%% and once it finds a character that has to be URL-encoded it switches to
%% the version of the function that accumulates the converted bstr.
urlencode(Str, N) ->
    case Str of
        <<Head:N/binary, Char, _Tail/binary>> ->
            case is_urlencoded(Char) of
                true ->
                    Hi = integer_to_hex_char((Char band 16#f0) bsr 4),
                    Lo = integer_to_hex_char((Char band 16#0f)),
                    urlencode(Str, N + 1, [Lo, Hi, $%, Head]);

                false ->
                    urlencode(Str, N + 1)
            end;
        _ ->
            Str
    end.
urlencode(Str, N, Acc) ->
    case Str of
        <<_Head:N/binary, Char, _Tail/binary>> ->
            case is_urlencoded(Char) of
                true ->
                    Hi = integer_to_hex_char((Char band 16#f0) bsr 4),
                    Lo = integer_to_hex_char((Char band 16#0f)),
                    urlencode(Str, N + 1, [Lo, Hi, $% | Acc]);

                false ->
                    urlencode(Str, N + 1, [Char | Acc])
            end;
        _ ->
            list_to_binary(lists:reverse(Acc))
    end.

%% @doc Determine whether a character has to be URL-encoded.
is_urlencoded(Char) ->
    not (((Char >= $0) andalso (Char =< $9)) orelse
         ((Char >= $A) andalso (Char =< $Z)) orelse
         ((Char >= $a) andalso (Char =< $z)) orelse
         (Char =:= $-) orelse (Char =:= $_) orelse
         (Char =:= $.) orelse (Char =:= $~)).

%% @doc Convert an integer between 0 and 15 to an hexadecimal character.
integer_to_hex_char(N) when N >= 0 ->
    if
        N =< 9 ->
            $0 + N;
        N =< 15 ->
            $A - 10 + N;
        true ->
            erlang:error(badarg)
    end.
%% @hidden
integer_to_hex_char(N, lower) when N >= 0 ->
    if
        N =< 9 ->
            $0 + N;
        N =< 15 ->
            $a - 10 + N;
        true ->
            erlang:error(badarg)
    end;
integer_to_hex_char(N, upper) ->
    integer_to_hex_char(N).


%%--------------------------------------------------------------------
%% @spec urldecode(bstr()) -> bstr()
%% @doc  Decode a bstr using the URL-encoding scheme.
%% @end
%%--------------------------------------------------------------------
-spec urldecode(bstr()) -> bstr().
urldecode(Str) ->
    urldecode(Str, 0).
%% This part of the function iterates over the bstr without copying any data
%% and once it finds a character that is URL-encoded it switches to
%% the version of the function that accumulates the converted bstr.
urldecode(Str, N) ->
    case Str of
        <<Head:N/binary, $%, Hi, Lo, _Tail/binary>> ->
            Char = ((hex_char_to_integer(Hi) bsl 4) bor hex_char_to_integer(Lo)),
            urldecode(Str, N + 3, <<Head/binary, Char>>);
        <<_Head:N/binary, Char, _Tail/binary>> ->
            if
                Char =:= $% ->
                    erlang:error(badarg);
                true ->
                    urldecode(Str, N + 1)
            end;
        _ ->
            Str
    end.
urldecode(Str, N, Acc) ->
    case Str of
        <<_Head:N/binary, $%, Hi, Lo, _Tail/binary>> ->
            Char = ((hex_char_to_integer(Hi) bsl 4) bor hex_char_to_integer(Lo)),
            urldecode(Str, N + 3, <<Acc/binary, Char>>);
        <<_Head:N/binary, Char, _Tail/binary>> ->
            if
                Char =:= $% ->
                    erlang:error(badarg);
                true ->
                    urldecode(Str, N + 1, <<Acc/binary, Char>>)
            end;
        _ ->
            Acc
    end.

%% @doc Convert an hexadecimal character to an integer. If the character is not an
%% hexadecimal character we return a 'badarg' exception.
hex_char_to_integer(Char) ->
    if
        (Char >= $0) and (Char =< $9) ->
            Char - $0;
        (Char >= $A) and (Char =< $F) ->
            Char - $A + 10;
        (Char >= $a) and (Char =< $f) ->
            Char - $a + 10;
        true ->
            erlang:error(badarg)
    end.



%%--------------------------------------------------------------------
%% @spec xmlencode(bstr()) -> bstr()
%% @doc  Encode a bstr using the XML-encoding scheme.
%%       WARNING: This function assumes that the input is a valid UTF-8 string
%%                and supports non-printable characters in the ASCII range 
%%                00h-1Fh. Bytes that are not part of a valid UTF-8 character 
%%                are not converted at all.
%% @end
%%--------------------------------------------------------------------
-spec xmlencode(bstr()) -> bstr().
xmlencode(Str) when is_binary(Str) ->
    xmlencode(Str, 0).
%% This part of the function iterates over the bstr() without copying any data
%% and once it finds a character that has to be XML-encoded it switches to
%% the version of the function that accumulates the converted bstr.
xmlencode(Str, Offset) ->
    case Str of
        <<Head:Offset/binary, Char, _Tail/binary>> ->
            case is_xmlencoded(Char) of
                true ->
                    Encoded = xmlencode_char(Char),
                    xmlencode(Str, Offset + 1, [$;, Encoded, $&, Head]);

                false ->
                    xmlencode(Str, Offset + 1)
            end;
        _ ->
            Str
    end.
xmlencode(Str, Offset, Acc) ->
    case Str of
        <<_Head:Offset/binary, Char, _Tail/binary>> ->
            case is_xmlencoded(Char) of
                true ->
                    Encoded = xmlencode_char(Char),
                    xmlencode(Str, Offset + 1, [$;, Encoded, $& | Acc]);

                false ->
                    xmlencode(Str, Offset + 1, [Char | Acc])
            end;
        _ ->
            list_to_binary(lists:reverse(Acc))
    end.

%% @doc Determine whether a character has to be XML-encoded. See
%% <a href="http://en.wikipedia.org/wiki/UTF-8#Description">Wikipedia</a>, 
%% <a href="http://www.asciitable.com/">ASCII Table</a>
is_xmlencoded(Char) -> 
    (Char < 32) orelse (Char =:= 192) orelse (Char =:= 193) orelse (Char > 244) 
        orelse (Char =:= $&) orelse (Char =:= $<) orelse (Char =:= $>) 
        orelse (Char =:= $') orelse (Char =:= $").

%% Encode a single character using the XML encoding scheme to create a
%% character entity reference.
xmlencode_char($&) ->
    <<"amp">>;
xmlencode_char($<) ->
    <<"lt">>;
xmlencode_char($>) ->
    <<"gt">>;
xmlencode_char($') ->
    <<"apos">>;
xmlencode_char($") ->
    <<"quot">>;
xmlencode_char(Char) when Char < 32 ->
    Hi = integer_to_hex_char((Char band 16#f0) bsr 4),
    Lo = integer_to_hex_char((Char band 16#0f)),
    <<"#x", Hi, Lo>>;
xmlencode_char(Char) ->
    Char.


%%--------------------------------------------------------------------
%% @spec xmldecode(bstr()) -> bstr()
%% @doc  Decode a bstr using the XML-encoding scheme to resolve any character
%%       entity reference present in the string.
%% @end
%%--------------------------------------------------------------------
-spec xmldecode(bstr()) -> bstr().
xmldecode(Str) ->
    xmldecode(Str, 0).
%% This part of the function iterates over the bstr without copying any data
%% and once it finds a character that is XML-encoded it switches to
%% the version of the function that accumulates the converted bstr.
xmldecode(Str, Offset) ->
    case xmldecode_char(Str, Offset) of
        %% We found a character that does not need to be decoded
        {_Char, 1} ->
            xmldecode(Str, Offset + 1);
        %% We found a character that needs to be encoded; we create the accumulator
        %% and call the function that copies each character to create a new string.
        {Char, Length} ->
            {Head, _Tail} = split_binary(Str, Offset),
            xmldecode(Str, Offset + Length, [Char, Head]);
        eof ->
            Str
    end.
xmldecode(Str, Offset, Acc) ->
    case xmldecode_char(Str, Offset) of
        {Char, Length} ->
            xmldecode(Str, Offset + Length, [Char | Acc]);
        eof ->
            list_to_binary(lists:reverse(Acc))
    end.

%%--------------------------------------------------------------------
%% @spec xmldecode_char(bstr(), Offset::integer()) -> {char(), 1 | 4 | 5 | 6} | eof
%% @doc  Given a string and an offset, this function checks whether there is a
%%       character in that position that has to be decoded using the XML 
%%       encoding scheme for character entity references and returns a tuple 
%%       with the decoded character and the length of the encoded substring in 
%%       the original string.
%% @end
%%--------------------------------------------------------------------
-spec xmldecode_char(bstr(), integer()) -> {char(), 1 | 4 | 5 | 6} | eof.
xmldecode_char(Str, Offset) ->
    case Str of
        <<_Head:Offset/binary, "&amp;", _Tail/binary>> ->
            {$&, 5};
        <<_Head:Offset/binary, "&lt;", _Tail/binary>> ->
            {$<, 4};
        <<_Head:Offset/binary, "&gt;", _Tail/binary>> ->
            {$>, 4};
        <<_Head:Offset/binary, "&apos;", _Tail/binary>> ->
            {$', 6};
        <<_Head:Offset/binary, "&quot;", _Tail/binary>> ->
            {$", 6};
        <<_Head:Offset/binary, "&#x", Hi, Lo, $;,  _Tail/binary>> ->
            {((hex_char_to_integer(Hi) bsl 4) bor hex_char_to_integer(Lo)), 6};
        <<_Head:Offset/binary, Char, _Tail/binary>> ->
            {Char, 1};
        _ ->
           eof
    end.


%%--------------------------------------------------------------------
%% @spec hexencode(bstr()) -> bstr()
%% @doc  Encode a bstr converting each character to its hexadecimal 
%%       representation.
%% @end
%%--------------------------------------------------------------------
-spec hexencode(bstr()) -> bstr().
hexencode(Str) when is_binary(Str) ->
    hexencode(Str, []).
hexencode(<<Hi:4, Lo:4, Tail/binary>>, Acc) ->
    hexencode(Tail, [integer_to_hex_char(Lo, lower), integer_to_hex_char(Hi, lower) | Acc]);
hexencode(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc)).


%%--------------------------------------------------------------------
%% @spec hexdecode(bstr()) -> bstr()
%% @doc  Decode a bstr with an hexadecimal representation of a string.
%% @end
%%--------------------------------------------------------------------
-spec hexdecode(bstr()) -> bstr().
hexdecode(Str) when is_binary(Str) ->
    hexdecode(Str, []).
hexdecode(<<Hi, Lo, Tail/binary>>, Acc) ->
    Char = ((hex_char_to_integer(Hi) bsl 4) bor hex_char_to_integer(Lo)),
    hexdecode(Tail, [Char | Acc]);
% If the number of characters wasn't even we raise an exception.
hexdecode(<<_Char>>, _Acc) ->
    erlang:error(badarg);
hexdecode(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc)).

