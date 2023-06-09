## Modified from https://github.com/rocker-org/rocker-versioned2/blob/caff65d9b31327e0662633860c54ae2cc28bc60f/dockerfiles/Dockerfile_r-ver_4.1.0
FROM ubuntu:20.04@sha256:0e0402cd13f68137edb0266e1d2c682f217814420f2d43d300ed8f65479b14fb

ENV R_VERSION=4.3.0
ENV TERM=xterm
ENV LC_ALL=en_US.UTF-8
ENV LANG=en_US.UTF-8
ENV R_HOME=/usr/local/lib/R
ENV CRAN=https://packagemanager.rstudio.com/all/__linux__/focal/latest
ENV TZ=Etc/UTC

COPY install_R.sh install_R.sh

RUN /install_R.sh

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      curl \
      gpg-agent \
      pkg-config \
      file \
      libsodium-dev \
      libssl-dev \
      software-properties-common \
 && apt-get autoremove -y \
 && apt-get autoclean -y \
 && rm -rf /var/lib/apt/lists/*

RUN add-apt-repository ppa:ubuntugis/ubuntugis-unstable \
 && apt-get update \
 && apt-get install -y --no-install-recommends \
      gdal-bin \
      libudunits2-dev \
      libgeos-dev \
      libproj-dev \
      libgdal-dev \
 && apt-get autoremove -y \
 && apt-get autoclean -y \
 && rm -rf /var/lib/apt/lists/*

RUN install2.r -e \
      callr \
      classInt \
      cpp11 \
      covr \
      data.table \
      DBI \
      dplyr \
      DT \
      e1071 \
      fastmap \
      future \
      htmltools \
      httr \
      logger \
      later \
      lutz \
      plumber \
      promises \
      proxy \
      rapidoc \
      remotes \
      readODS \
      readxl \
      s2 \
      stringi \
      units \
      tictoc \
      tidyr \
      tinytest \
      withr \
      wk

RUN installGithub.r r-spatial/sf luomus/finbif@9e9591c9

HEALTHCHECK --interval=1m --timeout=10s \
  CMD curl -sfI -o /dev/null 0.0.0.0:8000/healthz || exit 1

ENV HOME /home/user
ENV OPENBLAS_NUM_THREADS 1
ENV OMP_THREAD_LIMIT 1
ENV FINBIF_USE_PRIVATE_API true

COPY entrypoint.sh /usr/local/bin/entrypoint.sh
COPY convert.r /usr/local/bin/convert
COPY init.r /usr/local/bin/init
COPY api.R /home/user/api.R
COPY api.md /home/user/api.md
COPY favicon.ico /home/user/favicon.ico
COPY robots.txt /home/user/robots.txt
COPY pkg /home/user/fgc

WORKDIR /home/user

RUN  R -e "remotes::install_local('fgc', NULL, FALSE, 'never')" \
  && mkdir -p /home/user/logs \
  && mkdir -p /home/user/coverage \
  && chgrp -R 0 /home/user \
       /usr/local/lib/R/site-library/fgc/tinytest \
       /usr/local/lib/R/site-library/rapidoc/dist \
  && chmod -R g=u /home/user \
       /usr/local/lib/R/site-library/fgc/tinytest \
       /usr/local/lib/R/site-library/rapidoc/dist \
       /etc/passwd

USER 1000

EXPOSE 8000

ENTRYPOINT ["entrypoint.sh"]

CMD ["init"]
