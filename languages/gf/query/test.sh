#/usr/bin/bash

gf --run < tests/query.gfs | diff -u - tests/query.GOLD

echo "If this is the first line you see, it means success!"

echo "Here are the results:"
gf --run < tests/query.gfs
