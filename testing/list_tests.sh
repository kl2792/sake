#!/bin/bash
# GNU bash, version 4.4.12(1)-release (x86_64-apple-darwin15.6.0)
#
# A script for printing out code listings for the following files:
# test_*.{c,sk,out} and fail_*.{sk,err}
#
# Author: Kai-Zhan Lee

# Check if files exist
function check {
	for file in $@; do
		[ ! -e "$file" ] && >&2 echo "Houston, we've got a problem" && exit
	done
}

# Print the code listing of a single file; exit if any are not present
function list {
	check $@
	for file in $@; do
		echo "$file:"
		cat $file | sed 's/^/    /'
		echo
	done
}

# Print the script itself (it's in the testing directory, after all!)
list $0

# Print listings for adventure
list adventure.{sk,c}

# Print listings for traffic light FSMs
for file in test_*TL.sk; do list "${file%.*}".{sk,c,out}; done

# Print listings for positive test files
GLOBIGNORE="*TL.sk"
for file in test_*.sk; do list "${file%.*}".{sk,c,out}; done
unset GLOBIGNORE

# Print listings for negative fail files
for file in fail_*.sk; do list "${file%.*}".{sk,err}; done
