FROM ghcr.io/luomus/base-r-image@sha256:bfc65b835b503a235459716a9eb8afefb2bec94effdb19e69573885b898b00e8

COPY favicon.ico /home/user/favicon.ico
COPY api.md /home/user/api.md
COPY renv.lock /home/user/renv.lock
COPY convert.r /usr/local/bin/convert
COPY pkg /home/user/pkg
COPY api.R /home/user/api.R

RUN R -e "renv::restore()" \
 && sed -i 's/RapiDoc/finbif-geo-convert/g' \
    `R --slave -e "cat(.libPaths()[[1]])"`/rapidoc/dist/index.html \
 && mkdir -p /home/user/logs /home/user/coverage \
 && chgrp -R 0 /home/user \
 && chmod -R g=u /home/user /etc/passwd

ENV FINBIF_USER_AGENT=https://github.com/luomus/finbif-geo-convert
ENV FINBIF_USE_PRIVATE_API=true
