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


## How to run

To run the simple test script that runs a few basic functions of hydrographr,
just to test whether the image was built correctly, run this command:

```
docker run -it hydrographr:${builddate}-base Rscript basic_test_script.R
```

To run a container and directly open an R session:

```
docker run -it hydrographr:${builddate}-base
```

To run a docker container and open a command line session - you can use R
from that command line, by typing "R", as on any other Linux machine.

```
docker run -it hydrographr:${builddate}-base /bin/bash
```

## TODO

* Maybe make an image that contains e.g. test data and some required data
* Make a slimmer image
* Extend the basic test scripts, eventually write real unit tests
* Create an image that has all library versions fixed
