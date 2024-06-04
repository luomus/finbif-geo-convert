# docker manifest inspect ghcr.io/luomus/base-r-image:main -v | jq '.Descriptor.digest'
FROM ghcr.io/luomus/base-r-image@sha256:19a4ab18e6d63f9cf4051bfb7c1f770fbf3878ba4b32fa7d52ef885d9f53809b

COPY renv.lock /home/user/renv.lock

RUN R -s -e "renv::restore()"

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
COPY .Rbuildignore /home/user/.Rbuildignore

RUN R CMD INSTALL .
RUN permissions.sh
