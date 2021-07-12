# finbif-geo-convert

Convert FinBIF occurrence data downloads to various geographic file formats

## HTTP API

See online [documentation](https://fgc.rahtiapp.fi/__docs__/) for details.

## CLI

### Requirements

* Docker
 
### Installation

```bash
docker pull ghcr.io/luomus/finbif-geo-convert
```

### Usage

To convert a local file:

```bash
docker run --rm -u $(id -u) -v $PWD:/home/user ghcr.io/luomus/finbif-geo-convert convert -o test.shp -g footprint HBF.49381.zip
```

Note that the container must run with the same UID as the host system and the 
directory containing the file to convert needs to be bound with the container's
home directory.

### Documentation

```bash
docker run ghcr.io/luomus/finbif-geo-convert convert --help
```
