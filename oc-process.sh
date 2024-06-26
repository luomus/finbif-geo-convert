#!/bin/bash

i="all"

while getopts ":f:e:i::" flag; do
case $flag in
f) f=${OPTARG} ;;
e) e=${OPTARG} ;;
i) i=${OPTARG} ;;
esac
done

set -a

source ./$e

set +a

BRANCH=$(git symbolic-ref --short -q HEAD)
FINBIF_PRIVATE_API="unset"

if [ "$BRANCH" != "main" ]; then

HOST=$HOST_DEV
FINBIF_PRIVATE_API="dev"

fi

if [ $i = "deploy" ]; then

ITEM=".items[0]"

elif [ $i = "service" ]; then

ITEM=".items[1]"

elif [ $i = "route" ]; then

ITEM=".items[2]"

else

  ITEM=""

fi

oc process -f $f \
-p BRANCH=$BRANCH \
-p HOST=$HOST \
-p FINBIF_PRIVATE_API=$FINBIF_PRIVATE_API \
-p SMTP_SERVER=$SMTP_SERVER \
-p SMTP_PORT=$SMTP_PORT \
-p ERROR_EMAIL_TO=$ERROR_EMAIL_TO \
-p ERROR_EMAIL_FROM=$ERROR_EMAIL_FROM \
| jq $ITEM
