%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: dbtree.erl
%%% @author jakdan@kth.se
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(dbtree).
-export([write/3, new/0, read/2, delete/2,match/2,destroy/1]).

%% @doc Creates a new database tree
new() -> empty.

%% @doc Writes a new entry to the database, overrides if Key already exists.
write(Key,Value,Tree) when Tree == empty ->
	 {Key,Value,empty,empty}; 
write(Key,Value,{Key,_V,Left,Right}) -> 
	{Key,Value,Left,Right};
write(Key,Value,{K, _V, Left,Right}) when Key < K ->
	case Left == empty of
		true -> {K, _V,{Key,Value,empty,empty},Right};
		false -> {K,_V,write(Key,Value,Left),Right}
	end;
write(Key,Value,{K, _V, Left,Right}) when Key > K ->
	case Right == empty of
		true -> {K, _V, Left,{Key,Value,empty,empty}};
		false -> {K, _V, Left,write(Key,Value,Right)}
	end.
	
	
%% @doc Returns a value of given Key if it exists, else returns error.
read(_Key,Tree) when Tree == empty ->
	{error,instance};
read(Key,{Key,Value,_Left, _Right}) ->
	Value;
read(Key,{K,_Value,Left,_Right}) when Key < K ->
	read(Key,Left);
read(Key,{K,_Value,_Left,Right}) when Key > K ->
	read(Key,Right).
	
	
	
%% @doc Deletes an entry of a given Key from the database.
delete(_Key, Tree) when Tree == empty ->
	empty;
	
delete(Key,Tree) ->
	delete(Key,Tree,empty).
	
delete(Key,{Key,Value,Left,Right},Parent) when Parent == empty -> 
	{Key,Value,Left,Right};
	
delete(Key,{Key,_Value,Left,Right},_Parent) -> 
	if
		((Left == empty) and (Right == empty)) -> empty;
		(Left == empty) -> Right;
		(Right == empty) -> Left;
		((Left /= empty) and (Right /= empty)) -> append(Right,Left)
	end;

delete(Key,{K,Value,Left,Right},_Parent) when Key < K ->
	{K,Value,delete(Key,Left,{K,Value,Left,Right}),Right};

delete(Key,{K,Value,Left,Right},_Parent) when Key > K ->
	{K,Value,Left,delete(Key,Right,{K,Value,Left,Right})}.
	

%% @doc Help function to Delete if deleted node has two children.
append({Key,Val1, Left1,Right1},{Key2, Val2, Left2,Right2}) ->
	case Left1 == empty of
		true -> {Key,Val1,{Key2,Val2,Left2,Right2},Right1};
		false -> {Key,Val1,append(Left1,{Key2,Val2,Left2,Right2}),Right1}
	end.

	
%% @doc Returns a list of all matching Keys of a given Element.
match(_Element,empty) -> [];

match(Element,{Key,Element,Left,Right}) ->
	L = [Key | match(Element,Left)],
	[L | match(Element,Right)];

match(Element,{_Key,_E,Left,Right}) ->
	L = match(Element,Left),
	[L | match(Element,Right)].
	
	
destroy(_Db) -> 
	ok.
	
	
	
	
	