#!/bin/bash

PORT=$1
if [ -z "$1" ]
  then
    PORT=5001
fi



echo "source('app.R');runApp(host='0.0.0.0',port=$PORT);" > run.R
R --no-save --file=run.R

