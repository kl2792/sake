Test Details:

test_42:

sk: prints 42
out: 42

test_block:

sk: testing block comments
out: I sucessfully naviagated a block comment

test_bool:

sk: testing boolean variables and assignment true/false 
c: input i is true
out:
L is false
i is true

test_comment:

sk: testing line comments  
out: Line comment sandwhich

test_concurrent:

sk: Testing concurrent fsms 
out:
3
3
3
45
45
45

test_emptyfsm:

sk: empty fsm
out: empty file 

test_for:

sk: testing for loop
k initial set to be 4, loops though for loop 0:4:1 then prints k
out: 8

test_fsmhello:

sk: simple 2 state fsm to print hello world 
c:  
out: 
Hello Emma's World
Hello

test_header:

sk: testing header generation
out: empty file

test_hello:

sk: prints number 1-8
out:
1
2
3
4
5
6
7
8

test_printing:

sk: testing prints with %d and %s combinations AND ALL escape characters
out:
1
42 is a cool number!
34 is also a cool number!
hello world!
e is the coolest letter!
This is multiple strings
        This is tabbed in
 is a backspace
Carriage return
\ is a backslash
' is a single quote

test_ifelse:

sk: simple if else test, changes output depending on input
c: enter multiple inputs to see change, print output from tester
out: 
6
42
21
89
42

test_nestedIf:

sk: test nested for loop with true/false statements 
out: True Loop

test_nestedIf2:

sk: test using ==
out: 
n: 9
m: 9


test_nestedFor:

sk: nested for loop
c: print the value returned in the output struct 
out: 14


test_string:

sk: print Hello World using \n and \t
out:
Hello World
        Hello World
                Hello World

test_switch1:

sk: single switch case 
out: 4

test_switch: 

sk: switch on incoming input varaible 
out: 
42
24

test_multiSwitch:

sk: having multiple lines of code for switch case
out:
42
42
24
24

test_trafficLight

sk: single traffic light that depending on traffic flow switches colors
c: test on single input stream
out: look in c file and out

test_hogTL

sk: concurrent traffic lights that switch flow depending on flow traffic
c: tests on input1 and input2 streams 
out: written in the c file and out

test_brokenTL

sk: concurrent traffic lights that only look at own flow (i.e. both could go green at the same time - he he he)
c: 
out:

test_unreachableTL

sk: single traffic light that goes through the motions but gets stuck in yello
out: 

test_loopingTL

sk: traffic lights that loop together stay together 
c: 
out:

test_variables

sk: testing parsing of all the variables 
out: empty file

test_while

sk: simple while loop to alter the output varaibles
out: 15

