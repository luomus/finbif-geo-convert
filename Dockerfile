FROM ghcr.io/luomus/base-r-image@sha256:585ec3b42f1a6e6805657e550b03e0c354168111d868155473ab0839b9b3f04b

COPY favicon.ico /home/user/favicon.ico
COPY api.md /home/user/api.md
COPY renv.lock /home/user/renv.lock
COPY convert.r /usr/local/bin/convert
COPY pkg /home/user/pkg
COPY api.R /home/user/api.R

RUN R -e "renv::restore()" \
 && sed -i 's/RapiDoc/'finbif-geo-convert'/g' \
    `R --slave -e "cat(.libPaths()[[1]])"`/rapidoc/dist/index.html \
 && mkdir -p /home/user/logs /home/user/coverage \
 && chgrp -R 0 /home/user \
 && chmod -R g=u /home/user /etc/passwd

ENV FINBIF_USER_AGENT=https://github.com/luomus/finbif-geo-convert
ENV FINBIF_USE_PRIVATE_API=true
