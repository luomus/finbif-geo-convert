FROM ghcr.io/luomus/base-r-image@sha256:aa2caca64a234e63f7c1ba06cb06b04d18603d2b0a66be62cc8587ebe0ac876d

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
