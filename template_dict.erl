-module(template_dict).

-export([new/0,add_value/3,add_section/3, add_include/4, lookup/2, merge_section_dict/2, test/0]).

new()->
	[].
	
%% Internal: {value, Key, Value}	
add_value(Key,Value,Dict)->
	lists:append(Dict,[{value,Key,Value}]).

%% Internal: {section, Key, [ [ Dict1], [Dict2], ... ]}
add_section(Key,SectDict,Dict)->
	case lookup(Key,Dict) of
		[] ->
			lists:append(Dict,[{section,Key,[SectDict]}]);
		DictList ->
			NewDict=lists:keydelete(Key,2,Dict),
			NewSectDict=lists:append(DictList,[SectDict]),
			lists:append(NewDict,[{section,Key,NewSectDict}])
	end.

%% Internal: {include, Key, {Filename,[[Dict1],[Dict2],...]}
add_include(Key,Filename,InclDict,Dict)->
	case lookup(Key,Dict) of
		[] ->
			lists:append(Dict,[{include,Key,{Filename,[InclDict]}}]);
		{_,DictList}->
			NewDict=lists:keydelete(Key,2,Dict),
			NewSectDict=lists:append(DictList,[InclDict]),
			lists:append(NewDict,[{include,Key,{Filename,NewSectDict}}])
	end.
	
lookup(Key,Dict)->
	case lists:keyfind(Key,2,Dict) of
		{_,Key,Value}->Value;
		false        ->[]
	end.

merge_section_dict(Section, Dict)->
	TopDict = lists:filter( fun(Elem)->case Elem of {value,_,_}->true;_->false end end, Dict),
	SectDicts = lookup(Section,Dict),
	merge_1section_dict(SectDicts,TopDict,[]).

merge_1section_dict([],_,Acc)->
	lists:reverse(Acc);
merge_1section_dict([SectDict|Rest], TopDict, Acc)->
	NewDict = lists:merge(SectDict,TopDict),
	merge_1section_dict(Rest,TopDict,[NewDict|Acc]).

test()->
	D = new(),
	[{value,"NAME","Petr"}]=add_value("NAME","Petr",D),
	
	D1=add_value("NAME","Petr",D),
	[{value,"NAME","Petr"},{value,"VAL",1}]=add_value("VAL",1,D1),
	
	[{section,"SUB",[[{value,"NAME","Petr"}]]}]=add_section("SUB",D1,D),
	
	DX=add_section("SUB",D1,D),
	[{section,"SUB",[[{value,"NAME","Petr"}],[{value,"NAME","Petr"}]]}]=add_section("SUB",D1,DX),
	
	[]=lookup("NOTEXISTING",D1),
	"Petr"=lookup("NAME",D1),
	
	D2=add_value("SURENAME","Doe", D),
	D3=add_section("SUB",D2,D1),
	[[{value,"NAME","Petr"},{value,"SURENAME","Doe"}]]=merge_section_dict("SUB",D3),
	
	D4=add_value("SURENAME","Foo", D),
	[[{value,"NAME","Petr"},{value,"SURENAME","Doe"}],
	 [{value,"NAME","Petr"},{value,"SURENAME","Foo"}]]=merge_section_dict("SUB",add_section("SUB",D4,D3)),
	ok.
	