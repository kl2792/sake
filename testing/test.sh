#!/bin/sh

# Path to the LLVM interpreter
LLI="lli"
#LLI="/usr/local/opt/llvm/bin/lli"

# Path to the LLVM compiler
LLC="llc"

# Path to the C compiler
CC="cc"

# TODO path to sake compiler??? 

# Set time limit for all operations
ulimit -t 40

globallog=testall.log
rm -f $globallog
error=0
globalerror=0

# TESTING CONFIGURATIONS

# TODO fill in each segment if needed 
# TODO FUNCTIONS 

# usage
Usage() {
    echo "Usage: test.sh [.sake files]"
    exit 1
}

# SignalError()
signalError() {    
    if [ $error -eq 0 ] ; then            
        echo "FAILED"                    
        error=1                     
    fi               
    echo "  $1"      
}

# Comparing the generated with expected 
Compare() {    
    generatedfiles="$generatedfiles $3"           
    echo diff -b $1 $2 ">" $3 1>&2        
    diff -b "$1" "$2" > "$3" 2>&1 || {
        SignalError "$1 differs"
        echo "FAILED $1 differs from $2" 1>&2   
    }                                
}

# Run functions

# Check functions


# TODO uncomment so it checks for LLVM 
#LLIFail() {  
#    echo "Could not find the LLVM interpreter \"$LLI\"." 
#    echo "Check your LLVM installation and/or modify the LLI variable in testall.sh"
#    exit 1
#}

#which "$LLI" >> $globallog || LLIFail


# TODO 

# CODE TO GET THE TEST FILES

if [ $# -ge 1 ] 
then
    files=$@
else
    #Check this path 
    files="testing/test-*.sake testing/fail-*.sake"
fi

# TODO CODE TO CALL FUNCTIONS ON FILES 






exit $globalerror


