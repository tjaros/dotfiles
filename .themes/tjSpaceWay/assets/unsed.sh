#!/bin/sh
sed -i \
         -e 's/rgb(0%,0%,0%)/#000000/g' \
         -e 's/rgb(100%,100%,100%)/#cdcdcd/g' \
    -e 's/rgb(50%,0%,0%)/#000000/g' \
     -e 's/rgb(0%,50%,0%)/#7d8fa3/g' \
 -e 's/rgb(0%,50.196078%,0%)/#7d8fa3/g' \
     -e 's/rgb(50%,0%,50%)/#1c1c1c/g' \
 -e 's/rgb(50.196078%,0%,50.196078%)/#1c1c1c/g' \
     -e 's/rgb(0%,0%,50%)/#cdcdcd/g' \
	"$@"
