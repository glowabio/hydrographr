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


To run a docker container and mount a host dir:

```
mkdir hytestresults
docker run -it -v ./hytestresults:/home/ubuntu/hydro/mounted hydrographr:${builddate}-fixed /bin/bash
```

Now, if you store anything into `/home/ubuntu/hydro/mounted` inside the container,
it will be visible outside the container in `./hytestresults`.





## TODO

* Maybe make an image that contains e.g. test data and some required data
* Make a slimmer image
* Extend the basic test scripts, eventually write real unit tests
* Create an image that has all library versions fixed
