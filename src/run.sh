#!/bin/bash

cd source_counties; 
  todo=`ls -1 *.shp | awk '{ print substr($1,1,index($1,".")-1) }'`; 
    cd ..;

for c in $todo; do
  if [ ! -r $c".7z" ]; then
    echo "cp /home/ktaylora/nass/source_counties/"$c".*" $PWD/ | /bin/bash 
    echo "time R --no-save --vanilla --slave --args . "$c" < 01_*.R" | /bin/bash
    echo "time R --no-save --vanilla --slave --args "$c" < 02_*.R" | /bin/bash
    echo "time R --no-save --vanilla --slave --args "$c" < 03_*.R" | /bin/bash
    echo "7za a "$c".7z "$c"*.tif "$c"*.rdata "$c"*farmed_binary_pts*"  | /bin/bash
    echo "rm -rf "$c"*.tif" | /bin/bash
    echo "rm -rf "$c".sh*" | /bin/bash
    echo "rm -rf "$c".prj" | /bin/bash
    echo "rm -rf "$c".dbf" | /bin/bash
    echo "rm -rf "$c"*.rdata" | /bin/bash
    echo "rm -rf "$c"*farmed_binary_pts*" | /bin/bash
  fi
done
