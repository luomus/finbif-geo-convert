## Modified from https://github.com/rocker-org/rocker-versioned2/blob/caff65d9b31327e0662633860c54ae2cc28bc60f/dockerfiles/Dockerfile_r-ver_4.1.0

FROM osgeo/gdal:ubuntu-small-latest

ENV R_VERSION=4.1.0
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
      libssl-dev \
      libudunits2-dev \
      software-properties-common \
 && apt-get autoremove -y \
 && apt-get autoclean -y \
 && rm -rf /var/lib/apt/lists/*

RUN install2.r -s -e \
  classInt \
  cpp11 \
  data.table \
  DBI \
  dplyr \
  e1071 \
  fastmap \
  future \
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
  tidyr \
  tictoc \
  wk

RUN sed -i 's/RapiDoc/FinBIF Geo-convert/g' \
      /usr/local/lib/R/site-library/rapidoc/dist/index.html

RUN add-apt-repository ppa:ubuntugis/ubuntugis-unstable \
 && apt-get install -y --no-install-recommends \
      libgeos-dev \
      libproj-dev \
 && apt-get autoremove -y \
 && apt-get autoclean -y \
 && rm -rf /var/lib/apt/lists/*

RUN install2.r -s -e -r cran.r-project.org sf

RUN installGithub.r luomus/finbif@06c6cf4

COPY pkg fgc

RUN R -e "remotes::install_local('fgc')"

ENV HOME /home/user
ENV OPENBLAS_NUM_THREADS 1

COPY entrypoint.sh /usr/local/bin/entrypoint.sh
COPY convert.r /usr/local/bin/convert
COPY init.r /usr/local/bin/init
COPY api.R /home/user/api.R
COPY api.md /home/user/api.md

RUN  mkdir -p /home/user/logs \
  && chgrp -R 0 /home/user \
  && chmod -R g=u /home/user /etc/passwd

WORKDIR /home/user

USER 1000

EXPOSE 8000

ENTRYPOINT ["entrypoint.sh"]

CMD ["init"]
