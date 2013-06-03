%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2011. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

-module(ej_ucs).

-compile([verbose,report_warnings,warn_unused_vars]).

%%% Conversion to/from RFC-1345 style mnemonic strings consisting
%%% of subsets of ISO-10646 with "escape" sequences.
%-export([from_mnemonic/1, from_mnemonic/2]).

%%% UTF-16, and UTF-8 encoding and decoding
-export([from_utf16be/1]).
-export([to_utf8/1, from_utf8/1]).

from_utf16be(Bin) when is_binary(Bin) -> from_utf16be(Bin,[],[]);
from_utf16be(List) -> from_utf16be(list_to_binary(List),[],[]).

from_utf16be(<<Ch:16/big-unsigned-integer, Rest/binary>>, Acc, Tail)
  when Ch < 16#D800; Ch > 16#DFFF ->
    if Ch < 16#FFFE -> from_utf16be(Rest,[Ch|Acc],Tail) end;
from_utf16be(<<Hi:16/big-unsigned-integer, Lo:16/big-unsigned-integer,
	       Rest/binary>>, Acc, Tail)
  when Hi >= 16#D800, Hi < 16#DC00, Lo >= 16#DC00, Lo =< 16#DFFF ->
    %% Surrogate pair
    Ch = ((Hi band 16#3FF) bsl 10) + (Lo band 16#3FF) + 16#10000,
    from_utf16be(Rest, [Ch|Acc], Tail);
from_utf16be(<<>>,Acc,Tail) ->
    lists:reverse(Acc,Tail);
from_utf16be(Bin,Acc,Tail) ->
    io:format("ucs Error: Bin=~p~n     Acc=~p~n     Tail=~p~n",[Bin,Acc,Tail]),
    {error,not_utf16be}.

%%% UTF-8 encoding and decoding
to_utf8(List) when is_list(List) -> lists:flatmap(fun to_utf8/1, List);
to_utf8(Ch) -> char_to_utf8(Ch).

from_utf8(Bin) when is_binary(Bin) -> from_utf8(binary_to_list(Bin));
from_utf8(List) ->
    case expand_utf8(List) of
	{Result,0} -> Result;
	{_Res,_NumBadChar} ->
	    exit({ucs,{bad_utf8_character_code}})
    end.

%% expand_utf8([Byte]) -> {[UnicodeChar],NumberOfBadBytes}
%%  Expand UTF8 byte sequences to ISO 10646/Unicode
%%  charactes. Any illegal bytes are removed and the number of
%%  bad bytes are returned.
%%
%%  Reference:
%%     RFC 3629: "UTF-8, a transformation format of ISO 10646".

expand_utf8(Str) ->
    expand_utf8_1(Str, [], 0).

expand_utf8_1([C|Cs], Acc, Bad) when C < 16#80 ->
    %% Plain Ascii character.
    expand_utf8_1(Cs, [C|Acc], Bad);
expand_utf8_1([C1,C2|Cs], Acc, Bad) when C1 band 16#E0 =:= 16#C0,
					 C2 band 16#C0 =:= 16#80 ->
    case ((C1 band 16#1F) bsl 6) bor (C2 band 16#3F) of
	C when 16#80 =< C ->
	    expand_utf8_1(Cs, [C|Acc], Bad);
	_ ->
	    %% Bad range.
	    expand_utf8_1(Cs, Acc, Bad+1)
    end;
expand_utf8_1([C1,C2,C3|Cs], Acc, Bad) when C1 band 16#F0 =:= 16#E0,
					    C2 band 16#C0 =:= 16#80,
					    C3 band 16#C0 =:= 16#80 ->
    case ((((C1 band 16#0F) bsl 6) bor (C2 band 16#3F)) bsl 6) bor
	(C3 band 16#3F) of
	C when 16#800 =< C ->
	    expand_utf8_1(Cs, [C|Acc], Bad);
	_ ->
	    %% Bad range.
	    expand_utf8_1(Cs, Acc, Bad+1)
    end;
expand_utf8_1([C1,C2,C3,C4|Cs], Acc, Bad) when C1 band 16#F8 =:= 16#F0,
					       C2 band 16#C0 =:= 16#80,
					       C3 band 16#C0 =:= 16#80,
					       C4 band 16#C0 =:= 16#80 ->
    case ((((((C1 band 16#0F) bsl 6) bor (C2 band 16#3F)) bsl 6) bor
	(C3 band 16#3F)) bsl 6) bor (C4 band 16#3F) of
	C when 16#10000 =< C ->
	    expand_utf8_1(Cs, [C|Acc], Bad);
	_ ->
	    %% Bad range.
	    expand_utf8_1(Cs, Acc, Bad+1)
    end;
expand_utf8_1([_|Cs], Acc, Bad) ->
    %% Ignore bad character.
    expand_utf8_1(Cs, Acc, Bad+1);
expand_utf8_1([], Acc, Bad) -> {lists:reverse(Acc),Bad}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% UTF-8 support
%%% Possible errors encoding UTF-8:
%%%	- Non-character values (something other than 0 .. 2^31-1).
%%%	- Surrogate pair code in string.
%%%	- 16#FFFE or 16#FFFF character in string.
%%% Possible errors decoding UTF-8:
%%%	- 10xxxxxx or 1111111x as initial byte.
%%%	- Insufficient number of 10xxxxxx octets following an initial octet of
%%%	multi-octet sequence.
%%% 	- Non-canonical encoding used.
%%%	- Surrogate-pair code encoded as UTF-8.
%%%	- 16#FFFE or 16#FFFF character in string.
char_to_utf8(Ch) when is_integer(Ch), Ch >= 0 ->
    if Ch < 128 ->
	    %% 0yyyyyyy
	    [Ch];
       Ch < 16#800 ->
	    %% 110xxxxy 10yyyyyy
	    [16#C0 + (Ch bsr 6),
	     128+(Ch band 16#3F)];
       Ch < 16#10000 ->
	    %% 1110xxxx 10xyyyyy 10yyyyyy
	    if Ch < 16#D800; Ch > 16#DFFF, Ch < 16#FFFE ->
		    [16#E0 + (Ch bsr 12),
		     128+((Ch bsr 6) band 16#3F),
		     128+(Ch band 16#3F)]
	    end;
       Ch < 16#200000 ->
	    %% 11110xxx 10xxyyyy 10yyyyyy 10yyyyyy
	    [16#F0+(Ch bsr 18),
	     128+((Ch bsr 12) band 16#3F),
	     128+((Ch bsr 6) band 16#3F),
	     128+(Ch band 16#3F)];
       Ch < 16#4000000 ->
	    %% 111110xx 10xxxyyy 10yyyyyy 10yyyyyy 10yyyyyy
	    [16#F8+(Ch bsr 24),
	     128+((Ch bsr 18) band 16#3F),
	     128+((Ch bsr 12) band 16#3F),
	     128+((Ch bsr 6) band 16#3F),
	     128+(Ch band 16#3F)];
       Ch < 16#80000000 ->
	    %% 1111110x 10xxxxyy 10yyyyyy 10yyyyyy 10yyyyyy 10yyyyyy
	    [16#FC+(Ch bsr 30),
	     128+((Ch bsr 24) band 16#3F),
	     128+((Ch bsr 18) band 16#3F),
	     128+((Ch bsr 12) band 16#3F),
	     128+((Ch bsr 6) band 16#3F),
	     128+(Ch band 16#3F)]
    end.
