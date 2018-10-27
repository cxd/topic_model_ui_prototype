#!/bin/bash

PORT=$1
if [ -z "$1" ]
  then
    PORT=5001
fi


R -e "source('app.R');runApp(host='0.0.0.0',port=$PORT);"
