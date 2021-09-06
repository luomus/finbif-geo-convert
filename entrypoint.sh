#!/bin/bash
sed -i 's/RapiDoc/'"$PAGE_TITLE"'/g' \
  /usr/local/lib/R/site-library/rapidoc/dist/index.html
echo "user:x:$(id -u):0::/home/user:/sbin/nologin" >> /etc/passwd
"$@"
