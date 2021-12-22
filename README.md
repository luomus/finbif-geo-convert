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
oc new-app -f openshift-template.yml \
  -p HOST=<host-url>
```

## CLI

### Requirements

* git
* docker

### Installation

```bash
git clone https://github.com/luomus/finbif-geo-convert
docker pull ghcr.io/luomus/finbif-geo-convert
cp /finbif-geo-convert/fgc /usr/local/bin/
```

### Usage

To convert a local file:

```bash
fgc -o test.shp -g footprint HBF.53254.zip
```

Note that the container must run with the same UID as the host system and the 
directory containing the file to convert needs to be bound with the container's
home directory.

### Documentation

```bash
fgc --usage
```
