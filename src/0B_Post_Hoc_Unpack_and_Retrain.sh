#!/bin/bash

if [[ $# -ne 1 ]]; then # did the user pass a session layer by name?  If not, assume a series of runs for all shapefiles in source_counties/
  todo=`ls -1 *.7z | awk '{ print substr($1,1,index($1,".")-1) }'`;
else
  todo=$1;
fi

for t in $todo; do
  7za x $t".7z"

  if [ -r $t"_prob_occ.tif" ]; then
    rm -rf $t"_prob_occ.tif"
    rm -rf $t"_model.rdata"
    R --no-save --vanilla --slave --args $t < 03_*.R # re-train our forests
    7za a $t".7z" $t"_*.*"
  fi
  rm -rf `echo $t"_*"`
done
