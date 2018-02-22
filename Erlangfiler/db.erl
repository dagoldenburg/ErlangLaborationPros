-module(db).
-export([new/0,destroy/1,write/3,read/2,delete/2,match/2]).

new()-> [].

write(Key,Element,[]) -> 
	[{Key,Element}];
write(Key,Element,[{Key,_} | Db]) -> 
	[{Key,Element} | Db];
write(Key,Element,[Current | Db]) -> 
	[Current | write(Key,Element,Db)]. 
	
read(Key, [{Key, Element} | _Db]) ->
	{ok, Element};
read(Key,[_Current | Db]) ->
	read(Key,Db);
read(_Key, []) -> {error, instance}.


delete(_Key,[]) ->
	[];
delete(Key, [{Key, _Element} | Db]) ->
	Db;
delete(Key,[Current | Db]) -> 
	[Current | delete(Key,Db)].
	
match(Element, [{Key, Element}|Db]) ->
	[Key|match(Element, Db)];
match(Element, [_Tuple|Db]) ->
	match(Element, Db);
match(_Key, []) ->
	[].
	
destroy(_Db) -> 
	ok.