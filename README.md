# finbif-geo-convert

[![Docker](https://github.com/luomus/finbif-geo-convert/actions/workflows/docker-publish.yml/badge.svg)](https://github.com/luomus/finbif-geo-convert/actions/workflows/docker-publish.yml)

Convert FinBIF occurrence data downloads to various geographic file formats

## HTTP API

See online [documentation](https://fgc.rahtiapp.fi/__docs__/) for details.

### Deploy

#### Docker compose

```bash
docker-compose up --build -d
```

#### Openshift

```bash
./oc-process.sh -f template.yml -e .env | oc create -f -
```
