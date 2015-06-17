%%
%% pmap_xdr was generated by erpcgen (do not edit)
%% date: Jun 17 14:37:03 2015
%%
-module(pmap_xdr).
-compile([nowarn_unused_vars, nowarn_unused_function]).
-export([enc_mapping/1, dec_mapping/2]).
-export([enc_pmaplist/1, dec_pmaplist/2]).
-export([enc_call_args/1, dec_call_args/2]).
-export([enc_call_result/1, dec_call_result/2]).

enc_mapping(_1) ->
    case _1 of
        {_5,_4,_3,_2} ->
            [<<_5:32>>,<<_4:32>>,<<_3:32>>,<<_2:32>>]
    end.

dec_mapping(_1, _2) ->
    begin
        begin
            <<_:_2/binary,_3:32/unsigned,_/binary>> = _1,
            _4 = _2 + 4
        end,
        begin
            <<_:_4/binary,_5:32/unsigned,_/binary>> = _1,
            _6 = _4 + 4
        end,
        begin
            <<_:_6/binary,_7:32/unsigned,_/binary>> = _1,
            _8 = _6 + 4
        end,
        begin
            <<_:_8/binary,_9:32/unsigned,_/binary>> = _1,
            _10 = _8 + 4
        end,
        {{_3,_5,_7,_9},_10}
    end.

enc_pmaplist(_1) ->
    case _1 of
        void ->
            <<0:32>>;
        _ ->
            [<<1:32>>,
             case _1 of
                 {_3,_2} ->
                     [enc_mapping(_3),enc_pmaplist(_2)]
             end]
    end.

dec_pmaplist(_1, _2) ->
    begin
        <<_:_2/binary,_3:32/unsigned,_/binary>> = _1,
        _4 = _2 + 4,
        if
            _3 == 0 ->
                {void,_4};
            _3 == 1 ->
                begin
                    {_5,_6} = dec_mapping(_1, _4),
                    {_7,_8} = dec_pmaplist(_1, _6),
                    {{_5,_7},_8}
                end
        end
    end.

enc_call_args(_1) ->
    case _1 of
        {_6,_5,_4,_2} ->
            [<<_6:32>>,
             <<_5:32>>,
             <<_4:32>>,
             begin
                 _3 = io_list_len(_2),
                 [<<_3:32/unsigned>>,_2,enc_align(_3)]
             end]
    end.

dec_call_args(_1, _2) ->
    begin
        begin
            <<_:_2/binary,_3:32/unsigned,_/binary>> = _1,
            _4 = _2 + 4
        end,
        begin
            <<_:_4/binary,_5:32/unsigned,_/binary>> = _1,
            _6 = _4 + 4
        end,
        begin
            <<_:_6/binary,_7:32/unsigned,_/binary>> = _1,
            _8 = _6 + 4
        end,
        {_9,_10} =
            begin
                <<_:_8/binary,_11:32/unsigned,_/binary>> = _1,
                _12 = _8 + 4,
                <<_:_12/binary,_13:_11/binary,_/binary>> = _1,
                {_13,_12 + align(_11)}
            end,
        {{_3,_5,_7,_9},_10}
    end.

enc_call_result(_1) ->
    case _1 of
        {_4,_2} ->
            [<<_4:32>>,
             begin
                 _3 = io_list_len(_2),
                 [<<_3:32/unsigned>>,_2,enc_align(_3)]
             end]
    end.

dec_call_result(_1, _2) ->
    begin
        begin
            <<_:_2/binary,_3:32/unsigned,_/binary>> = _1,
            _4 = _2 + 4
        end,
        {_5,_6} =
            begin
                <<_:_4/binary,_7:32/unsigned,_/binary>> = _1,
                _8 = _4 + 4,
                <<_:_8/binary,_9:_7/binary,_/binary>> = _1,
                {_9,_8 + align(_7)}
            end,
        {{_3,_5},_6}
    end.

io_list_len(L) -> io_list_len(L, 0).
io_list_len([H|T], N) ->
  if
    H >= 0, H =< 255 -> io_list_len(T, N+1);
    is_list(H) -> io_list_len(T, io_list_len(H,N));
    is_binary(H) -> io_list_len(T, size(H) + N);
    true -> exit({xdr, opaque})
  end;
io_list_len(H, N) when is_binary(H) ->
  size(H) + N;
io_list_len([], N) ->
N.

enc_align(Len) ->
  case Len rem 4 of
    0 -> <<>>;
    1 -> <<0,0,0>>;
    2 -> <<0,0>>;
    3 -> <<0>>
  end.

align(Len) ->
  case Len rem 4 of
    0 -> Len;
    1 -> Len+3;
    2 -> Len+2;
    3 -> Len+1
  end.
