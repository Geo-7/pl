#!/usr/local/bin/rebol3
Rebol [
	Title: "PL/0 Compiler"
]

print "PL/0 Started"
pp: func [text]
  [if my_debug [print text]]
my_debug: false
digit: charset "0123456789"
letter: charset [#"a" - #"z" #"A" - #"Z"]
ws: charset reduce [tab newline #" "]
ws*: [any ws]
ident: [some letter]
number: [some digit]
end-st: [";" ws*]
ending: [ws* "end" ws*]
begin: [ws* "begin" ws*]
var: ["var" ws* ident (pp "#De var") any [ws* "," ws* ident] ws* end-st  (pp "#De var-end") ]
const-assign: [ident ws* "=" ws* number]
const: [ws* "const" (pp "#De const") ws* const-assign any [ws* "," ws* const-assign] ws* end-st (pp "#De end-const")]
assign: [ident ws* ":=" ws* exp ws* end-st]
call: [ ws* "call" (pp "#De call") ws* ident (pp "#De call-ident") ws* end-st]
question: [ "?" ws* ident ws* end-st]
plnot: [ "!" ws* ident ws* end-st]
exp1: [ ident | number]
exp: [ exp1 ws* any [ws* ["-" | "+" | "*" | "/"] ws* exp1 (pp "#De nested exp1")]]
statement:
 [ws* [assign (pp "#De assign")| call | question | plnot | loop | plif | procedure |
 [ws* begin (pp "#De begin") ws* 
 any statement (pp "#D nested st") ending (pp "#De end") ws*]] ws*]
statement1: [ ws* [assign | call | question | plnot | plif ] ws*]
loop: [ws* "while" (pp "#De while") ws* condition ws* "do" ws* statement]
plif: [ws* "if" (pp "#De if") ws* condition ws* 
"then" (pp "#De then") ws* 
[[statement end-st] | statement1] ws*]
condition: [exp (pp "#De cond exp") ws* 
["#" | "<=" | ">=" | "<" | ">" | "="] ws* exp (pp "#De cond exp2") ws*]
procedure: [ws* "procedure" ws* ident ws* end-st ws* any [var | const] statement ws* end-st ws*]
main_prog: [any [const | var | procedure | statement | ws* ] "." (pp "#D end of app") ws*]

test_var: does
  [
    result: true
    if not parse "var  ab, av;" var [result: false]
    if not parse "var  ab , av ;" var [result: false]
    if not parse "var  ab ,av;" var [result: false]
    either result [print "parsing var: OK"]
    [print "parsing var: FAILED"]
  ]

test_const: does
  [
    either parse "const  ab=10, av =  20;" const [print "parsing const: OK"]
    [print "parsing const: FAILED"]
  ]

test_call: does
  [
    either parse "call  abfbfb;" call [print "parsing call: OK"]
    [print "parsing call: FAILED"]
  ]

test_question: does
  [
    either parse "?  abfbfb;" question [print "parsing question: OK"]
    [print "parsing question: FAILED"]
  ]

test_not: does
  [
    either parse "!  abfbfb;" plnot [print "parsing not: OK"]
    [print "parsing not: FAILED"]
  ]

test_assign: does 
  [
    either parse "a := i * j;" assign [print "parsing assign OK"]
    [print "parsing assign: FAILED"]
  ]

test_statement: does
  [
    st1: {
      begin
      i := 0; s := 0;
      end
    }
    either parse st1 statement [print "parsing statement OK"]
    [print "parsing statement FAILED"]
  ]

test_if: does
  [
    if1: "if i < I * 10 then z:=2*z;"
    either parse if1 plif [print "parsing if OK"]
    [print "parsing if FAILED"]
    
  ]

test_loop: does
  [
    loop1: {
	      while arg < max do
	      begin
		      call isprime;
		      if ret = 1 then x := 1;
		      arg := arg + 1;
	      end
      }
      either parse loop1 loop [print "parsing loop OK"]
      [print "parsing loop FAILED"]
  ]

test_procedure: does
  [
    proc1: {
    	procedure primes;
      begin
	      arg := 2;
	      while arg < max do
	      begin
		      call isprime;
		      if ret = 1 then x := 1;
		      arg := arg + 1;
	      end
      end;
      }
      either parse proc1 procedure [print "parsing procedure OK"]
      [print "parsing procedure FAILED"]
  ]
test_complete: does
  [
    prog:{
    	const max = 100;
var arg, ret;

procedure isprime;
var i;
begin
	ret := 1;
	i := 2;
	while i < arg do
	begin
		if arg / i * i = arg then
		begin
			ret := 0;
			i := arg;
		end;
		i := i + 1;
	end
end;

procedure primes;
begin
	arg := 2;
	while arg < max do
	begin
		call isprime;
		if ret = 1 then x :=2 ;
		arg := arg + 1;
	end
end;

call primes;
.
    }
     either parse prog main_prog [print "parsing complete_app OK"]
     [print "parsing complete_app FAILED"]
  ]
test_var
test_const
test_call
test_question
test_not
test_assign
test_statement
test_if
test_loop
test_procedure
test_complete

































































