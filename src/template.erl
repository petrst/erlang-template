-module(template).

-export([expand/2, test/0]).
-import(template_dict,[new/0,add_value/3,add_section/3,add_include/4, lookup/2,merge_section_dict/2]).	

expand(Template, Dict)->
	parse(Template,Dict,[]).

parse([], _Dict, Out)->
	Out;
parse([${,${,$/|Rest],_Dict,Out)->
	%% Section end marker
	{_Name,Rest2}=parse_name(Rest,[]),
	{Out,Rest2};
	
parse([${,${,$#|Rest],Dict,Out)->
	%% Section start
	{Name,Rest2}=parse_name(Rest,[]),
	%%SectDict=lookup(Name,Dict),
	SectDict=merge_section_dict(Name,Dict),
	{Value,Rest3}=parse_section(Rest2,SectDict,[]),
	parse(Rest3,Dict,Out++Value);

parse([${,${,$!|Rest],Dict,Out)->
	%% Comment start
	Rest2=parse_comment(Rest),
	parse(Rest2,Dict,Out);
	
parse([${,${,$>|Rest],Dict,Out)->
	%% Include marker
	{Name,Rest2}=parse_name(Rest,[]),	
	InclDict=lookup(Name,Dict),
	Value=expand_include(InclDict),
	parse(Rest2,Dict,Out++Value);
	
parse([${,${,C|Rest],Dict,Out) when C /= $#, C /= $!, C /= $< ->
	%% Variable marker
	{Name,Rest2}=parse_name(Rest,[C]),	
	case lookup(Name,Dict) of
		false -> parse(Rest2,Dict,Out);
		Value -> parse(Rest2,Dict,Out++Value)
	end;
	
parse([C|Rest],Dict,Out)->
	parse(Rest,Dict,Out++[C]).
	
parse_name([$},$}|Rest], Name)->
	{lists:reverse(Name),Rest};
parse_name([C|Rest], Name)->
	parse_name(Rest,[C|Name]).

parse_comment([$},$}|Rest])->
	Rest;
parse_comment([_C|Rest])->
	parse_comment(Rest).

parse_section(S,[],Out)->
	%% When no subdictionary found for this section, just swallow it
	{_,Rest}=parse(S,[],[]),
	{Out,Rest};
parse_section(S,[D|Rest],Out)->
	{Value,_Rest2}=parse(S,D,Out),
	parse_section(S,Rest,Value).

expand_include({Filename,Dict})->
	{ok,Bin}=file:read_file(Filename),
	Value=expand_1include(binary_to_list(Bin),Dict,[]),
	Value.
	
expand_1include(_,[],Out)->
	Out;
expand_1include(S,[Dict|Rest],Out)->
	Value=parse(S,Dict,Out),
	expand_1include(S,Rest,Value).

%% =================== UNIT TESTS =========================

test_app()->
	Template="{{!Header}}=====================\r\n"
			 "{{#SEC}}{{FIRST}} {{LAST}}\r\n{{/SEC}}"
			 "{{! Footer }}Generated from template\r\n",
	Names=[{"John","Doe"},{"Patrick","Faulkner"},{"John","Steinbeck"}],
	
	SectDicts=lists:map(fun({First,Last})->
				add_value("LAST",Last, add_value("FIRST",First,new()))
			  end, Names),
	Dict=lists:foldl(fun(Dict,Acc)->
				add_section("SEC",Dict,Acc) end, new(), SectDicts),
	"=====================\r\n"
	"John Doe\r\n"
	"Patrick Faulkner\r\n"
	"John Steinbeck\r\n"
	"Generated from template\r\n"=expand(Template,Dict),
	ok.

test()->
	%% Variable expansion
	D1=add_value("NAME","Petr",new()),
	D2=add_value("NAME","Paul",new()),
	{2,"Hi, Petr!"} = {2,expand("Hi, {{NAME}}!", D1)},
	
	%% Section expansion
	{3,"Hi, !"}     = {3,expand("Hi, {{#SEC}}Petr{{/SEC}}!", new())},
	{4,"Hi, Petr!"} = {4,expand("Hi, {{#SEC}}Petr{{/SEC}}!", add_section("SEC",new(),new()))},
	{5,"Hi, Petr!"} = {5,expand("Hi, {{#SEC}}{{NAME}}{{/SEC}}!", add_section("SEC",D1,new()) )},
	
	%% Section Iteration
	D3=add_section("SEC",D1, new()),	
	{6,"Hi, Petr!Hi, Paul!"} = {6,expand("{{#SEC}}Hi, {{NAME}}!{{/SEC}}", add_section("SEC",D2,D3))},
	
		
	%% Comments
	"Hi, Petr!" = expand("Hi, {{! comment }}Petr!",new()),
	"Hi, Petr!" = expand("Hi, {{#SEC}}{{!comment}}{{NAME}}{{/SEC}}!", D3),
	
	%% Dictionary inheritance
	%% Dictionary for SEC (D3) doesn't have NAME
	D4=add_value("SURENAME","Foo",D3),	
	"Hi, Petr!" = expand("Hi, {{#SEC}}{{NAME}}{{/SEC}}!", add_section("SEC",D4,D1)),
	
	ok=test_app(),
	
	%% Test Include
	InclTemplate= <<"{{FIRST}} {{LAST}}\r\n">>,
	ok=file:write_file("test_include.tmpl",InclTemplate),
	
	DI1=add_value("LAST","Doe", add_value("FIRST","John",new())),
	DI2=add_value("LAST","Bar", add_value("FIRST","Foo",new())),
	DI3=add_include("INCL","test_include.tmpl", DI1, new()),
	DI4=add_include("INCL","test_include.tmpl", DI2, DI3),
	
	"People:\r\nJohn Doe\r\nFoo Bar\r\n"=expand("People:\r\n{{>INCL}}", DI4),
	ok=file:delete("test_include.tmpl"),
	
	ok.
	
