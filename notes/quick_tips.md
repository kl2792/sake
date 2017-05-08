Helpful shortcuts:

Run ./testall -k to keep intermediary files

SSH into VM: ssh -p 4115 plt@127.0.0.1

To get LLVM code: clang -S -emit-llvm file-name.c

Octal dump to see into file: od -t a file-name

Assembly/Object dump: objdump -d file-name

Subtle breakage in testall.sh: change /bin/bash to /bin/sh
