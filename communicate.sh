#!/bin/bash

if [ ! -f TOKEN.txt ]
then echo "TOKEN.txt required"
     exit 1
fi

if [ $# -ne 1 ]
then echo "Synopsis: ./communicate.sh request.txt"
     exit 1
fi

URL=https://boundvariable.space/communicate
TOKEN=$(cat TOKEN.txt)

curl -H "Authorization: Bearer $TOKEN" -X POST -d @$1 $URL
