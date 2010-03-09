%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fbenavides@novamens.com>
%%% @copyright (C) 2010 Novamens S.A.
%%% @doc ZLibHEX RFB Encoding implementation
%%% @reference <a href="http://www.tigervnc.com/cgi-bin/rfbproto#zlibhex-encoding">More Information</a>
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(erfb_encoding_zlibhex).
-author('Fernando Benavides <fbenavides@novamens.com>').

-behaviour(erfb_encoding).

-export([init/0, read/5, write/4, terminate/2]).

-include("erfblog.hrl").
-include("erfb.hrl").

-record(state, {zstream     :: zlib:zstream(),
                zrawstream  :: term(),
                raw_state   :: term(),
                state       :: undefined | reading | writing}).

%% ====================================================================
%% Server functions
%% ====================================================================
%% @hidden
-spec init() -> {ok, #state{}}.
init() ->
    Z = zlib:open(),
    ZRaw = zlib:open(),
    {ok, RawState} = erfb_encoding_raw:init(),
    {ok, #state{zstream     = Z,
                zrawstream  = ZRaw,
                raw_state   = RawState,
                state       = undefined}}.

%% @hidden
-spec read(#pixel_format{}, #box{}, binary(), port(), #state{}) -> {ok, [#rectangle{}], Read::binary(), Rest::binary(), #state{}}.
read(PF, Box = #box{x = X, y = Y, width = W, height = H}, Bytes, Socket,
     State = #state{zstream     = Z,
                    zrawstream  = ZRaw,
                    raw_state   = RawState,
                    state       = ZState}) ->
    WLast = case W rem 16 of
                0 -> 16;
                Wrem16 -> Wrem16
            end,
    HLast = case H rem 16 of
                0 -> 16;
                Hrem16 -> Hrem16
            end,
    Tiles = [ #box{x = TileX,
                   y = TileY,
                   width =
                       case TileX + 16 of
                           Next when Next >= (X+W) ->
                               WLast;
                           _ ->
                               16
                       end,
                   height =
                       case TileY + 16 of
                           Next when Next >= (Y+H) ->
                               HLast;
                           _ ->
                               16
                       end
                  } ||
                  TileY <- lists:seq(Y, Y+H-HLast, 16),
                  TileX <- lists:seq(X, X+W-WLast, 16) ],
    ?DEBUG("ZLIBHex reader starting for ~p~n", [Box]),
    case ZState of
        writing ->
            ok = zlib:deflateEnd(Z),
            ok = zlib:deflateEnd(ZRaw),
            ok = zlib:inflateInit(Z),
            ok = zlib:inflateInit(ZRaw);
        reading ->
            void;
        undefined ->
            ok = zlib:inflateInit(Z),
            ok = zlib:inflateInit(ZRaw)
    end,
    read(Tiles, Box, PF, Bytes, Socket, [], <<>>, State#state{state = reading}).

%% @hidden
-spec write(#pixel_format{}, #box{}, [#rectangle{}], #state{}) -> {ok, binary(), #state{}} | {error, invalid_data, #state{}}.
write(PF, _Box, Tiles,
      State = #state{zstream    = Z,
                     zrawstream = ZRaw,
                     state      = ZState}) when is_list(Tiles) ->
    case ZState of
        reading ->
            ok = zlib:inflateEnd(Z),
            ok = zlib:inflateEnd(ZRaw),
            ok = zlib:deflateInit(Z),
            ok = zlib:deflateInit(ZRaw);
        writing ->
            void;
        undefined ->
            ok = zlib:deflateInit(Z),
            ok = zlib:deflateInit(ZRaw)
    end,
    internal_write(PF, Tiles, <<>>, State#state{state = writing});
write(_PF, _, Data, State) ->
    ?ERROR("Invalid data for zlibhex encoding:~p~n", [Data]),
    {error, invalid_data, State}.

%% @hidden
-spec terminate(term(), #state{}) -> ok.
terminate(Reason, #state{zstream    = Z,
                         zrawstream = ZRaw,
                         raw_state  = RawState}) ->
    ok = zlib:close(Z),
    ok = zlib:close(ZRaw),
    erfb_encoding_raw:terminate(Reason, RawState).


%% ====================================================================
%% Internal functions
%% ====================================================================
-spec read([#box{}], #box{}, #pixel_format{}, binary(), port(), [#rectangle{}], binary(), #state{}) -> {ok, #rectangle{}, Read::binary(), Rest::binary(), #state{}}.
read([], _OutsideBox, _PF, Rest, _Socket, Tiles, Read, State) ->
    {ok, lists:reverse(Tiles), Read, Rest, State};
read(Boxes, OutsideBox, PF, <<>>, Socket, Tiles, BytesRead, State) ->
    read(Boxes, OutsideBox, PF, erfb_utils:complete(<<>>, 1, Socket, true),
         Socket, Tiles, BytesRead, State);
read([Box | Boxes], OutsideBox, PF = #pixel_format{bits_per_pixel = BPP},
     <<Byte:1/binary, NextBytes/binary>>, Socket, Tiles, BytesRead,
     State = #state{zstream     = Z,
                    zrawstream  = ZRaw,
                    raw_state   = RawState}) ->
    <<_Padding:1/unit:1,
      ZLib:1/unit:1,
      ZLibRaw:1/unit:1,
      SubrectsColoured:1/unit:1,
      AnySubrects:1/unit:1,
      ForegroundSpecified:1/unit:1,
      BackgroundSpecified:1/unit:1,
      Raw:1/unit:1>> = Byte,
    
    case {ZLibRaw, ZLib, Raw} of
        {1,_,_} ->
            ?DEBUG("Box ~p read with ~s~n", [Box, zraw]),
            {CompHeader, Decompressed, Rest} =
                get_decompressed(NextBytes, Socket, ZRaw),
            {ok, RawData, _, _, NewRawState} =
                erfb_encoding_raw:read(PF, Box, Decompressed, Socket, RawState),
            read(
              Boxes, OutsideBox, PF, Rest, Socket,
              [#rectangle{box       = Box,
                          encoding  = ?ENCODING_ZLIB,
                          data      = {zraw, RawData}} | Tiles],
              <<BytesRead/binary, Byte/binary, CompHeader/binary, Decompressed/binary>>,
              State#state{raw_state = NewRawState});
        {0,1,1} ->
            ?DEBUG("Box ~p read with ~s~n", [Box, z]),
            {CompHeader, Decompressed, Rest} =
                get_decompressed(NextBytes, Socket, Z),
            {ok, RawData, _, _, NewRawState} =
                erfb_encoding_raw:read(PF, Box, Decompressed, Socket, RawState),
            read(
              Boxes, OutsideBox, PF, Rest, Socket,
              [#rectangle{box       = Box,
                          encoding  = ?ENCODING_ZLIB,
                          data      = {z, RawData}} | Tiles],
              <<BytesRead/binary, Byte/binary, CompHeader/binary, Decompressed/binary>>,
              State#state{raw_state = NewRawState});
        {0,0,1} ->
            ?DEBUG("Box ~p read with ~s~n", [Box, raw]),
            {ok, RawData, Read, Rest, NewRawState} =
                erfb_encoding_raw:read(PF, Box, NextBytes, Socket, RawState),
            read(
              Boxes, OutsideBox, PF, Rest, Socket,
              [#rectangle{box       = Box,
                          encoding  = ?ENCODING_RAW,
                          data      = RawData} | Tiles],
              <<BytesRead/binary, Byte/binary, Read/binary>>,
              State#state{raw_state = NewRawState});
        {0,1,0} ->
            ?DEBUG("Box ~p read with ~s~n", [Box, zlib]),
            {CompHeader, Decompressed, Rest} =
                get_decompressed(NextBytes, Socket, Z),
            PixelSize = erlang:trunc(BPP / 8),
            HeaderLength =
                get_header_length(PixelSize, BackgroundSpecified,
                                  ForegroundSpecified, AnySubrects),
            <<Header:HeaderLength/binary, Body/binary>> = Decompressed,
            {Background, Foreground, _} =
                get_header_data(PixelSize, BackgroundSpecified,
                                ForegroundSpecified, AnySubrects, Header),
            Rectangles =
                hextile_subreader(Body, PixelSize, SubrectsColoured),
            read(
              Boxes, OutsideBox, PF, Rest, Socket,
              [#rectangle{box     = Box,
                          data    =
                              #zlibhex_data{background = Background,
                                            foreground = Foreground,
                                            compressed = true,
                                            rectangles = Rectangles}} | Tiles],
              <<BytesRead/binary, Byte/binary, CompHeader/binary, Decompressed/binary>>,
              State);
        {0,0,0} ->
            ?DEBUG("Box ~p read with ~s~n", [Box, hextile]),
            PixelSize = erlang:trunc(BPP / 8),
            HeaderLength =
                get_header_length(PixelSize, BackgroundSpecified,
                                  ForegroundSpecified, AnySubrects),
            AllBytes =
                case bstr:len(NextBytes) of
                    HL when HL < HeaderLength ->
                        erfb_utils:complete(NextBytes, HeaderLength, Socket, true);
                    _ ->
                        NextBytes
                end,
            <<Header:HeaderLength/binary, Rest/binary>> = AllBytes,
            {Background, Foreground, Count} =
                get_header_data(PixelSize, BackgroundSpecified,
                                ForegroundSpecified, AnySubrects, Header),
            BodyLength = Count * (2 + (SubrectsColoured * PixelSize)), %%NOTE: if set, each subrect comes with its own colour
            AllRest =
                case bstr:len(Rest) of
                    BL when BL < BodyLength andalso Boxes =:= [] ->
                        erfb_utils:complete(Rest, BodyLength, Socket, true);
                    BL when BL < BodyLength ->
                        erfb_utils:complete(Rest, BodyLength + 1, Socket, true); %%NOTE: +1 to get the first byte of the next tile
                    _ ->
                        Rest
                end,
            Body = bstr:substr(AllRest, 1, BodyLength),
            Rectangles =
                hextile_subreader(Body, PixelSize, SubrectsColoured),
            read(
              Boxes, OutsideBox, PF,
              bstr:substr(AllRest, BodyLength + 1), Socket,
              [#rectangle{box     = Box,
                          data    =
                              #zlibhex_data{background  = Background,
                                            foreground  = Foreground,
                                            compressed  = false,
                                            rectangles  = Rectangles}} | Tiles],
              <<BytesRead/binary, Byte/binary, Header/binary, Body/binary>>,
              State)
    end.

-spec hextile_subreader(binary(), integer(), 0 | 1) -> [#rectangle{}].
hextile_subreader(Body, PixelSize, 1) ->
    [#rectangle{box     = #box{x = X,
                               y = Y,
                               width = W+1,
                               height = H+1},
                data    = Pixel} ||
               <<Pixel:PixelSize/unit:8,
                 X:1/unit:4,
                 Y:1/unit:4,
                 W:1/unit:4,
                 H:1/unit:4>> <= Body];
hextile_subreader(Body, _PixelSize, 0) ->
    [#rectangle{box     = #box{x = X,
                               y = Y,
                               width = W+1,
                               height = H+1}} ||
               <<X:1/unit:4,
                 Y:1/unit:4,
                 W:1/unit:4,
                 H:1/unit:4>> <= Body].

-spec internal_write(#pixel_format{}, [#rectangle{}], binary(), #state{}) -> {ok, binary(), #state{}} | {error, invalid_data, #state{}}. 
internal_write(_PF, [], Result, State) ->
    {ok, Result, State};
internal_write(PF,
               [#rectangle{box      = Box,
                           encoding = ?ENCODING_RAW,
                           data     = Data} | Tiles],
               Acc,
               State = #state{raw_state = RawState}) ->
    ?DEBUG("Box ~p written with ~s~n", [Box, raw]),
    case erfb_encoding_raw:write(PF, Box, Data, RawState) of
        {ok, NewData, NewRawState} ->
            internal_write(PF, Tiles,
                           <<Acc/binary, 1:1/unit:8, NewData/binary>>,
                           State#state{raw_state = NewRawState});
        {error, invalid_data, NewRawState} ->
            {error, invalid_data, State#state{raw_state = NewRawState}}
    end;
internal_write(PF,
               [#rectangle{box      = Box,
                           encoding = ?ENCODING_ZLIB,
                           data     = {ZStream, Data}} | Tiles],
               Acc,
               State = #state{zstream   = Z,
                              zrawstream= ZRaw,
                              raw_state = RawState}) ->
    ?DEBUG("Box ~p written with ~s~n", [Box, ZStream]),
    case erfb_encoding_raw:write(PF, Box, Data, RawState) of
        {ok, Uncompressed, NewRawState} ->
            NewData = 
                bstr:bstr(zlib:deflate(
                            case ZStream of
                                zraw -> ZRaw;
                                z -> Z
                            end, Uncompressed, sync)),
            Length =
                bstr:len(NewData),
            Header =
                case ZStream of
                    zraw    -> 32;
                    z       -> 65 %%NOTE: 65 = 64 (ZLIB) + 1 (RAW)
                end,
            internal_write(PF, Tiles,
                           <<Acc/binary, Header:1/unit:8,
                             Length:2/unit:8, NewData/binary>>,
                           State#state{raw_state = NewRawState});
        {error, invalid_data, NewRawState} ->
            {error, invalid_data, State#state{raw_state = NewRawState}}
    end;
internal_write(PF = #pixel_format{bits_per_pixel = BPP},
               [#rectangle{box = Box,
                           data = #zlibhex_data{background = Background,
                                                foreground = Foreground,
                                                compressed = Compressed,
                                                rectangles = Rectangles}} | Tiles],
               Acc, State = #state{zstream = Z}) ->
    ?DEBUG("Box ~p written with ~s~n", [Box, case Compressed of true -> zlib; _ -> hextile end]),
    PixelSize = erlang:trunc(BPP / 8),
    ForegroundSpecified =
        case Foreground of
            undefined -> 0;
            _ -> 1
        end,
    BackgroundSpecified =
        case Background of
            undefined -> 0;
            _ -> 1
        end,
    {AnySubrects, SubrectsColoured} =
        case Rectangles of
            [] ->
                {0, 0};
            [#rectangle{data = undefined} | _] ->
                {1, 0};
            _ ->
                {1, 1}
        end,
    ZLib =
        case Compressed of
            true -> 1;
            _ -> 0
        end,
    Header =
        <<0:1/unit:1,
          ZLib:1/unit:1,
          0:1/unit:1, %% ZLibRaw
          SubrectsColoured:1/unit:1,
          AnySubrects:1/unit:1,
          ForegroundSpecified:1/unit:1,
          BackgroundSpecified:1/unit:1,
          0:1/unit:1>>,
    Uncompressed =
        bstr:join([case BackgroundSpecified of
                       1 -> <<Background:PixelSize/unit:8>>;
                       0 -> <<>>
                   end,
                   case ForegroundSpecified of
                       1 -> <<Foreground:PixelSize/unit:8>>;
                       0 -> <<>>
                   end,
                   case AnySubrects of
                       1 -> <<(erlang:length(Rectangles)):PixelSize/unit:8>>;
                       0 -> <<>>
                   end |
                       [case Pixel of
                            undefined ->
                                <<X:1/unit:4,
                                  Y:1/unit:4,
                                  (W-1):1/unit:4,
                                  (H-1):1/unit:4>>;
                            Pixel ->
                                <<Pixel:PixelSize/unit:8,
                                  X:1/unit:4,
                                  Y:1/unit:4,
                                  (W-1):1/unit:4,
                                  (H-1):1/unit:4>>
                        end ||
                        #rectangle{box = #box{x = X,
                                              y = Y,
                                              width = W,
                                              height = H},
                                   data = Pixel} <- Rectangles]]),
    Body =
        case Compressed of
            true ->
                CompData = bstr:bstr(zlib:deflate(Z, Uncompressed, sync)),
                Length = bstr:len(CompData),
                <<Length:2/unit:8, CompData/binary>>;
            _ ->
                Uncompressed
        end,
    internal_write(PF, Tiles, <<Acc/binary, Header/binary, Body/binary>>, State).

-spec get_decompressed(binary(), port(), zlib:zstream()) -> {CompHeader :: binary(), Decompressed :: binary(), Rest :: binary()}.
get_decompressed(Bytes, Socket, Z) ->
    {Length, OtherBytes} =
        case Bytes of
            <<L:2/unit:8, O/binary>> ->
                {L, O};
            _ ->
                <<L:2/unit:8, O/binary>> =
                    erfb_utils:complete(Bytes, 2, Socket, true),
                {L, O}
        end,
    {RectBytes, Rest} =
        case bstr:len(OtherBytes) of
            LL when LL < Length ->
                ?TRACE("~p~n", [{LL, Length}]),
                {erfb_utils:complete(OtherBytes, Length, Socket, true),
                 <<>>};
            _ ->
                ?TRACE("~p~n", [Length]),
                {bstr:substr(OtherBytes, 1, Length),
                 bstr:substr(OtherBytes, Length+1)}
        end,
    Decompressed = zlib:inflate(Z, RectBytes),
    {<<Length:4/unit:8>>, bstr:bstr(Decompressed), Rest}.

-spec get_header_data(integer(), 0 | 1, 0 | 1, 0 | 1, binary()) -> {integer() | undefined, integer() | undefined, integer()}.
get_header_data(PixelSize, BackgroundSpecified, ForegroundSpecified, AnySubrects, Header) ->
    ?TRACE("~p~n", [{PixelSize, BackgroundSpecified, ForegroundSpecified, AnySubrects, Header}]),
    case {BackgroundSpecified, ForegroundSpecified, AnySubrects} of
        {1,1,1} ->
            <<B:PixelSize/unit:8, F:PixelSize/unit:8, C:1/unit:8>> = Header,
            {B, F, C};
        {1,1,0} ->
            <<B:PixelSize/unit:8, F:PixelSize/unit:8>> = Header,
            {B, F, 0};
        {1,0,1} ->
            <<B:PixelSize/unit:8, C:1/unit:8>> = Header,
            {B, undefined, C};
        {1,0,0} ->
            <<B:PixelSize/unit:8>> = Header,
            {B, undefined, 0};
        {0,1,1} ->
            <<F:PixelSize/unit:8, C:1/unit:8>> = Header,
            {undefined, F, C};
        {0,1,0} ->
            <<F:PixelSize/unit:8>> = Header,
            {undefined, F, 0};
        {0,0,1} ->
            <<C:1/unit:8>> = Header,
            {undefined, undefined, C};
        {0,0,0} ->
            {undefined, undefined, 0}
    end.

-spec get_header_length(integer(), 0 | 1, 0 | 1, 0 | 1) -> integer().
get_header_length(PixelSize, BackgroundSpecified,
                  ForegroundSpecified, AnySubrects) ->
    case BackgroundSpecified of
        1 -> PixelSize;
        0 -> 0
    end + case ForegroundSpecified of
              1 -> PixelSize;
              0 -> 0
    end +
        AnySubrects. %%NOTE: 1 => 1 more byte
