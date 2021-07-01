#!/bin/bash
echo "user:x:$(id -u):0:user user:/home/user:/sbin/nologin" >> /etc/passwd
Rscript --verbose finbif_geo_convert.R
