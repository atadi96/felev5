-module(huffman).
-export([build_code_table/1, encode/1, decode/1]).

weights(Text) ->
    weights(Text, maps:new()).

weights([], Weights) ->
    maps:to_list(Weights);
weights([Ch | Rest], Weights) ->
    NewWeights =
        case maps:find(Ch, Weights) of
            {ok,Weight} -> maps:put(Ch, Weight+1, Weights);
            error -> maps:put(Ch, 1, Weights)
        end,
    weights(Rest, NewWeights).

weights_to_leaves(Weights) ->
    WeightKeys =
        lists:map(
            fun({Ch,Weight}) -> { Weight, {leaf,Ch} } end,
            Weights
        ),
    lists:keysort(1, WeightKeys).

make_tree([Node]) ->
    Node;
make_tree([Node1, Node2 | Nodes]) ->
    { W1, _ } = Node1,
    { W2, _ } = Node2,
    NewNode = {W1 + W2, {internal,{Node1,Node2}} },
    NewNodes = lists:keymerge(1, [NewNode], Nodes),
    make_tree(NewNodes).

codes_from_tree(Tree) ->
    codes_from_tree("", Tree).

codes_from_tree(CodeStub, Tree) ->
    case Tree of
        {_,{leaf,Char}} -> [{Char, string:join(lists:reverse(CodeStub), "")}];
        {_,{internal,{Left,Right}}} ->
            Lefts = codes_from_tree([ "0" | CodeStub], Left),
            Rights = codes_from_tree([ "1" | CodeStub], Right),
            lists:keymerge(2, Lefts, Rights)
    end.

char_code_table(Text) ->
    codes_from_tree(
    make_tree(
    weights_to_leaves(
    weights(Text)))).

display_code_table(Table) ->
    lists:map(fun({Char,Code}) -> {[Char],Code} end, Table).

read_code_table(Displayed) ->
    lists:map(fun({[Char],Code}) -> {Char,Code} end, Displayed).

build_code_table(Text) ->
    display_code_table(char_code_table(Text)).

encode(Text) ->
    CodeList = char_code_table(Text),
    Codes = maps:from_list(CodeList),
    { display_code_table(CodeList)
    , string:join(
          lists:map(
            fun(Char) -> maps:get(Char, Codes) end,
            Text
          ),
          ""
      )
    }.

decode({DisplayedTable,Encoded}) ->
    decode(
        maps:from_list(
            lists:map(
                fun({Char,Code}) -> {Code,Char} end,
                read_code_table(DisplayedTable)
            )
        ),
        "",
        "",
        Encoded
    ).

decode(_Codes, DecodedPart, "", []) ->
    lists:reverse(DecodedPart);
decode(Codes, DecodedPart, CurrentCode, [Bit | Encoded]) ->
    NewCode = CurrentCode++[Bit],
    case maps:find(NewCode, Codes) of
        {ok, Char} -> decode(Codes, [Char | DecodedPart], "", Encoded);
        error -> decode(Codes, DecodedPart, NewCode, Encoded)
    end.


