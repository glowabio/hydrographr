# Docker image

_Merret Buurman, IGB Berlin, 2026-10-09_

This directory contains what is needed to build a Linux-based
Docker image that has R and hydrographr installed. The image is
based on Ubuntu 26. Of course, docker needs to be installed to
build and run docker containers.


## How to build

To build an image that has hydrographr installed, but does not contain
e.g. the test data, run the following command in the `docker` directory
of the hydrographr package. (This is the directory where the Dockerfiles
are located).

```
cd docker
builddate=$(date '+%Y%m%d')
docker build -f Dockerfile-base -t hydrographr:${builddate}-base .
```

This may take a while. After successful build, you should see the image
in docker's image list:

```
docker image ls | grep hydrographr
```

To build an image with a fixed commit of hydrographr, use a slightly
adapted Dockerfile:

```
cd docker
builddate=$(date '+%Y%m%d')
docker build -f Dockerfile-fixed -t hydrographr:${builddate}-fixed .

# it would be quite useful to add the commit to the image name:
githash=pleaseadd # whichever githash is in your dockerfile!
docker build -f Dockerfile-fixed -t hydrographr:${builddate}-${githash} .
```

### Include static files (for less download)

For some functions, hydrographr needs some static files, which it will
download if it does not find them in the temp directory. To avoid this,
you can add them to the image...

For example, `get_tile_id` needs the file `lookup_tile_regunit.txt`, and
`get_regional_unit_id` needs `regional_unit_ovr.tif`. There is also a zipped
directory of test data which you can use for running the examples of all
hydrographr functions.

First download them:

* [regional_unit_ovr.tif](https://public.igb-berlin.de/index.php/s/agciopgzXjWswF4/download?path=%2Fglobal&files=regional_unit_ovr.tif) (118 MB)
* [lookup_tile_regunit.txt](https://drive.google.com/uc?export=download&id=1deKhOEjGgvUXPwivYyH99hgHlJV7OgUv&confirm=t) (5 KB)
* [hydrography90m_test_data.zip](https://public.igb-berlin.de/index.php/s/QtRef2tMKrGePyf/download) (38 MB)


> [!TIP] In case the download URLs of `lookup_tile_regunit.txt`
> and `regional_unit_ovr.tif` have changed, you can download them using hydrographr:
> `get_tile_id(data.frame(lon = 20.853771, lat = 40.251642), lon="lon", lat="lat", tempdir=target_dir)`

> [!TIP] In case the download URL of `hydrography90m_test_data.zip`
> has changed, you can download it using hydrographr:
> `download_test_data(download_dir=target_dir)`


You can either mount them into your containers at run time (using `docker run -v`, see below),
or you can build an image that includes them:

```
cd docker
builddate=$(date '+%Y%m%d')
docker build -f Dockerfile-fixed-lessdownload -t hydrographr:${builddate}-lessdownload .

# it would be quite useful to add the commit to the image name:
githash=pleaseadd # whichever githash is in your dockerfile!
docker build -f Dockerfile-fixed-lessdownload -t hydrographr:${builddate}-${githash}-lessdownload .
```


## How to run

To run the simple test script that runs a few basic functions of hydrographr,
just to test whether the image was built correctly, run this command:

```
docker run -it hydrographr:${builddate}-fixed Rscript basic_test_script.R
```

To run a container and directly open an R session:

```
docker run -it hydrographr:${builddate}-fixed
```

To run a docker container and open a command line session - you can use R
from that command line, by typing "R", as on any other Linux machine.

```
docker run -it hydrographr:${builddate}-fixed /bin/bash
```

To run a container including the static files, to reduce downloads:

```
docker run -it hydrographr:${builddate}-lessdownload Rscript basic_test_script.R
```


To run a docker container and mount a host dir:

```
mkdir hytestresults
docker run -it -v ./hytestresults:/home/ubuntu/hydro/mounted hydrographr:${builddate}-fixed /bin/bash
```

Now, if you store anything into `/home/ubuntu/hydro/mounted` inside the container,
it will be visible outside the container in `./hytestresults`.

For example, you can mount the downloaded static files to the place where the test
script expects them (`/tmp/hydrographr`), to reduce downloads:

```
mkdir downloaded_static # download the static files into this dir
docker run -it -v ./downloaded_static:/tmp/hydrographr hydrographr:${builddate}-fixed /bin/bash
```



## TODO

* Maybe make an image that contains e.g. test data and some required data
* Make a slimmer image
* Extend the basic test scripts, eventually write real unit tests
* Create an image that has all library versions fixed
