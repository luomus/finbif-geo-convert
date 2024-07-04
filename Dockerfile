# docker manifest inspect ghcr.io/luomus/base-r-image:main -v | jq '.Descriptor.digest'
FROM ghcr.io/luomus/base-r-image@sha256:fe208061e35234991fa0221b0b684ec49a978de7c9a1e5cd02c18ad777ff9c8a

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
