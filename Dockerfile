FROM ghcr.io/luomus/base-r-image@sha256:b61f78d380e35c41b4161a55b56b4ba2c6ba9baeb5837df9504d141e1a8cdce7

ENV FINBIF_USER_AGENT=https://github.com/luomus/finbif-geo-convert
ENV FINBIF_USE_PRIVATE_API=true

COPY renv.lock /home/user/renv.lock
COPY favicon.ico /home/user/favicon.ico
COPY api.R /home/user/api.R
COPY api.md /home/user/api.md
COPY logo.png /home/user/figures/logo.png
COPY DESCRIPTION /home/user/DESCRIPTION
COPY inst /home/user/inst
COPY man /home/user/man
COPY NAMESPACE /home/user/NAMESPACE
COPY R /home/user/R
COPY tests /home/user/tests

RUN R -e 'renv::restore()' \
 && R -e 'remotes::install_local(dependencies = FALSE, upgrade = FALSE)' \
 && permissions.sh
