#/usr/bin/bash

gf --run < tests/query.gfs | diff -u - tests/query.GOLD

echo "If this is the only line you see, it means success!"
