LLI="lli"
LLC="llc"
SAKE="./sake.native"
CC="cc"

$SAKE hello.sk > hello.ll
$LLC hello.ll > hello.s 
$CC -o hello.exe hello.s printbig.o
./hello.exe > hello.out
cat hello.out
