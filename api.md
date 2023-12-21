## Overview

Use this HTTP API to convert a FinBIF occurrence data file into a geographic
data format (a "shapefile").

### **FinBIF Occurrence Data Files**

FinBIF occurrence data downloads come in two formats: "citable" and "lite".

Citable files (e.g., [HBF.53254](https://tun.fi/HBF.53254)) are zip archives of
a time-stamped FinBIF data requests made at
[laji.fi](https://laji.fi/observation/list). The archives contain the occurrence
data and associated metadata. They have persistent identifiers in the form of a
URI that links to a copy of the archive. Requesting the creation of a citable
file requires a registered user account at [laji.fi](https://laji.fi/#login),
but existing citable files are open access.

Lite download files are openly accessible and can be downloaded directly from
[laji.fi](https://laji.fi/observation/list) for any requests that contain less
than 10,000 records. Lite downloads can have a more limited range of record
attributes than what is provided with a citable file archive.

### **Converting to Geographic Data Formats**

Both citable and lite downloads can be converted to geographic data formats
using the API via a POST request. Converted citable files can also be directly
accessed with a GET request that includes the integer representation of the
citable file's identifier as a path parameter.

### **Supported Operations**

* Request a file conversion
* Upload a data file for conversion 
* Poll the status of a conversion
* Download the converted file in a geographic format

### **Conversion Status & Automatic Redirection**

For files that are quick to convert (< 30s) the server will redirect the
client to the output file automatically. Otherwise, the status of the conversion
can be polled at `/status/{id}` where `{id}` is an identifier assigned by the
server to the conversion. When the conversion is complete polling the status
will redirect the client to `/output/{id}` where the file will be available
for 24hrs after the initial request was made.

### **Examples**

Make a request to convert a citable file:

```bash
$ curl https://fgc.rahtiapp.fi/HBF.53254/shp/point/wgs84
"HBF.53254-20920cf1"
```

Get the status of a request:

```bash
$ curl http://fgc.rahtiapp.fi/status/HBF.53254-20920cf1
"pending"
```

Get the converted output file:

```bash
$ curl -O http://fgc.rahtiapp.fi/output/HBF.53254-20920cf1
```

Get the converted file via redirection:

```bash
$ curl -JLO https://fgc.rahtiapp.fi/HBF.53254/shp/point/wgs84
```

Upload a lite download and get a GeoPackage file back:

```bash
$ curl -JLOF file=@laji-data.tsv https://fgc.rahtiapp.fi/test/gpkg/point/wgs84
```
