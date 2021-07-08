## Overview

Use this HTTP API to convert a FinBIF occurrence data file into a geographic
data format (a "shapefile").

### **FinBIF Occurrence Data Files**

FinBIF occurrence data downloads come in two formats: "citable" and "lite".

Citable files (e.g., [HBF.53254](https://tun.fi/HBF.53254)) are zip archives of
a time-stamped FinBIF data requests made at
[laji.fi](https://laji.fi/observation/list). The archives contain the occurrence
data and associated metadata and have persistent identifiers in the form of a
URI that links to a copy of the archive. Requesting the creation of a citable
file requires a registered user account at [laji.fi](https://laji.fi/#login) but
existing citable files are open access.

Lite download files are openly accessible and can be downloaded directly from
[laji.fi](https://laji.fi/observation/list) for any requests that contain less
than 10,000 records. Lite downloads have a more limited range of record
attributes than are available for a citable file archive.

### **Converting to Geographic Data Formats**

Both citable and lite downloads can be converted to geographic data formats
using the API via a POST request. Converted citable files can also be downloaded
directly using a GET request by including the integer representation of their
identifier as a path parameter.

### **Supported Operations**

* Request a file conversion
* Upload a data file for conversion 
* Poll the status of a conversion
* Download the converted file in a geographic format
* See a list of supported output formats

### **Conversion Status & Automatic Redirection**

For files that are quick to convert (< 30-60s) the server will redirect the
client to the output file automatically. Otherwise, the status of the conversion
can be polled at `/status/{id}` where `{id}` is an identifier assigned by the
server to the conversion. When the conversion is complete the client will be
redirected to `/output/{id}` where the file will be available for 1 to 24hrs
after the initial request was made.

### **Examples**

Make a request to convert a citable file

```bash
$ curl https://fgc.rahtiapp.fi/53254/shp/point/wgs84
"3705b6ab"
```

Get the status of a request

```bash
$ curl http://fgc.rahtiapp.fi/status/3705b6ab
"pending"
```

Get the converted output file

```bash
$ curl -O http://fgc.rahtiapp.fi/output/3705b6ab
```

Get the converted file via redirection

```bash
$ curl -JLO https://fgc.rahtiapp.fi/53254/shp/point/wgs84
```

Upload a lite download and get a GeoJSON file back

```bash
$ curl -JLOF file=@laji-data.tsv https://fgc.rahtiapp.fi/1234/geojson/point/kkj
```
