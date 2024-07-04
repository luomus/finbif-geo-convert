#!/bin/bash

i="all"
f="template.yml"
e=".env"

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
  MEMORY=$MEMORY_DEV
  MAX_CHUNK_SIZE=$MAX_CHUNK_SIZE_DEV
  FINBIF_PRIVATE_API="dev"

fi

if [ $i = "deploy" ]; then

  ITEM=".items[0]"

elif [ $i = "service" ]; then

  ITEM=".items[1]"

elif [ $i = "route" ]; then

  ITEM=".items[2]"

elif [ $i = "all" ]; then

  ITEM=""

else

  echo "Object not found"
  exit 1

fi

echo "# $(oc project finbif-geo-convert)"

oc process -f $f \
  -p BRANCH=$BRANCH \
  -p HOST=$HOST \
  -p MEMORY=$MEMORY \
  -p MAX_CHUNK_SIZE=$MAX_CHUNK_SIZE \
  -p FINBIF_PRIVATE_API=$FINBIF_PRIVATE_API \
  -p SMTP_SERVER=$SMTP_SERVER \
  -p SMTP_PORT=$SMTP_PORT \
  -p ERROR_EMAIL_TO=$ERROR_EMAIL_TO \
  -p ERROR_EMAIL_FROM=$ERROR_EMAIL_FROM \
  | jq $ITEM
