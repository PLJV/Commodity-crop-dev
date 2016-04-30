#!/bin/bash

if [[ $# -ne 1 ]]; then # did the user pass a session layer by name?  If not, assume a series of runs for all shapefiles in source_counties/
  cd source_counties;
    todo=`ls -1 *.shp | awk '{ print substr($1,1,index($1,".")-1) }'`;
      cd ..;
else
  todo=$1;
fi

for c in $todo; do
  if [ `ls -lh1 $c.7z | awk '{ print $5 }' | grep  "M" | wc -l` -eq 0 ]; then # if the current session zipfile doesn't exist or is less than a megabyte
    echo "cp "$PWD"/source_counties/"$c".*" $PWD/ | /bin/bash
    if [ -r $c".shp" ]; then # make sure that $c isn't NULL -- it will lead to local deleteions of our source raster data
      if [ -r $c.7z ]; then 
        7za x $c.7z;
      fi
      echo "time R --no-save --vanilla --slave --args . "$c" < 01_*.R" | /bin/bash
      echo "time R --no-save --vanilla --slave --args "$c" < 02_*.R" | /bin/bash
      echo "time R --no-save --vanilla --slave --args "$c" < 03_*.R" | /bin/bash
      echo "7za a "$c".7z "$c"*.tif "$c"_model.rdata "$c"_farmed_binary_pts*"  | /bin/bash
      echo "rm -rf "$c"*.tif" | /bin/bash
      echo "rm -rf "$c".sh*" | /bin/bash
      echo "rm -rf "$c".prj" | /bin/bash
      echo "rm -rf "$c".dbf" | /bin/bash
      echo "rm -rf "$c"_model.rdata" | /bin/bash
      echo "rm -rf "$c"_farmed_binary_pts*" | /bin/bash
    fi
  elif [ `ls -lh1 $c.7z | awk '{ print substr($5,1,index($5,"M")-1) }' | xargs printf "%.f" ` -lt 100 ]; then # if the current session zipfile exists and is less than 100 megabytes
    7za x $c.7z
    rm -rf $c.7z
    bash ./run.sh $c # unpack session zipfile and recursively call run.sh for the focal session
  fi
done
