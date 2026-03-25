# hydrographr in JupyterHub

Merret Buurman, IGB, 2026-03-25

## Introduction

These are introductions to provide an R environment including hydrographr in
Jupyter Notebooks, served on an instance of JupyterHub on a Linux server. This
is useful for example for Workshop settings, where users can access a
predefined environment and do not have to install the package themselves.

Note: This was first done for the workshop at the Living data Conference in
October 2025 in Bogotá, Colombia. This is the updated version for the IGB
workshop in spring 2026.


## Steps

* Install Docker
* Install and configure nginx (to be used as reverse proxy)
* Install and configure JupyterHub
* JupyterHub's Production Security List
* Run via systemctl
* Dockerspawner: Jupyter Notebooks as Docker containers
* JupyterNotebook Docker image with hydrographr
* Adding files/data to the containers
* Generating Jupyter Notebook files from R-markdown vignette files
* Server cleanup


## Install Docker

Install docker, following the official docker documentation:

* https://docs.docker.com/engine/install/ubuntu/
* https://docs.docker.com/engine/install/linux-postinstall/


## Install and configure nginx

Install nginx, also following the official nginx documentation. As a guideline,
the following steps are needed (but please adapt them according to your )

Install the package:

```
sudo apt install nginx

# it should be running right away:
sudo systemctl status nginx
```

The configuration file is here: `/etc/nginx/sites-enabled/default`

Configure the server to listen for HTTPS on port 443, using TLS certificate
and key. Of course, the firewall has to allow connections on port 443 for this.

Configure a location where the JupyterHub will be running - in our case we want
it to be accessible under `https://.../workshop/`.

For this, add this piece of configuration:

```
location /workshop/ {

    # proxying to wherever JupyterHub is running:
    proxy_pass http://127.0.0.1:8000/workshop/;

    # Proxy settings
    proxy_set_header Host $host;
    proxy_set_header X-Real-IP $remote_addr;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header X-Forwarded-Proto $scheme;

    # WebSocket support
    proxy_http_version 1.1;
    proxy_set_header Upgrade $http_upgrade;
    proxy_set_header Connection "upgrade";

    # Disable buffering to support streaming
    proxy_buffering off;

    # To prevent: File Save Error for bla.ipynb Invalid response: 413 Request Entity Too Large
    client_max_body_size 50M;
}

```

Check whether configuration is valid:

```
nginx -t
```

Restart nginx:

```
date; sudo systemctl restart nginx
sudo systemctl status nginx
```


## Install and configure JupyterHub

### Install JupyterHub

Install JupyterHub, following the official documentation:
https://jupyterhub.readthedocs.io/en/stable/tutorial/quickstart.html

The main steps you have to do are the following:

Decide where to install it. This directory will contain configuration, virtual
environment, etc. We are using `/opt/jupyterhub`:

```
sudo mkdir /opt/jupyterhub
sudo chown MYUSER jupyterhub/
cd /opt/jupyterhub
```

Install, create and activate a virtual environment for python:

```
sudo apt install python3-virtualenv
virtualenv venv
source venv/bin/activate
# check whether the correct paths are used:
which python
which pip
```

Install the JupyterHub software and some dependencies depending on your system
(please refer to the documentation):

```
python3 -m pip install jupyterhub
# possibly:
date; sudo apt install npm
date; npm install -g configurable-http-proxy
date; sudo apt-get install nodejs npm
# needed if running the notebook servers in the same environment
python3 -m pip install jupyterlab notebook
```

Check installation:

```
date; jupyterhub -h
```

Try running it for the first time. (Check for error messages printed on screen.
If all looks good, close again using `ctrl-c`).

```
date; jupyterhub
```

### Configure JupyterHub

The configuration file is called `jupyterhub_config.py`. First, generate a
default config file which you can then edit:

```
jupyterhub --generate-config

# edit:
vi jupyterhub_config.py
```

**Config file helper:**

The configuration file has many commented options. To view only the active
lines, this command is useful:

```
cat jupyterhub_config.py | grep -v "^#" | grep -v "^$"'
```

If you add it as an alias to your `~/.bashrc`, you can easily use it by running
`jupyterhub_config`:

```
# in ~/.bashrc
alias jupyterhub_config='date; cat jupyterhub_config.py | grep -v "^#" | grep -v "^$"'
```

**Networking:**

Tell JupyterHub where it will be running. We want it to be accessible at
`https://our-url.de/workshop/`. The `/workshop/` part is handled by nginx
(see nginx config section) but JupyterHub needs to know this so that it can
add it to any redirection URLs.

```
c.JupyterHub.public_url = 'https://your-url.de/workshop/'
c.JupyterHub.base_url = '/workshop'
```

Jupyter is made of several components that run separately: The hub and proxy
(managing users, notebooks, ...), and each individual notebook. Here, we tell
JupyterHub where the hub will be running - on port `8081`.

```
c.JupyterHub.hub_routespec = '/workshop/hub/'
c.JupyterHub.hub_bind_url = 'http://0.0.0.0:8081'
c.JupyterHub.hub_connect_ip = '172.17.0.1'
```

**Authentication:**

In a workshop, we want all users to be able to log in using the same password,
so we write this:

```
c.JupyterHub.authenticator_class = "shared-password"
c.SharedPasswordAuthenticator.user_password = "your-password"
c.Authenticator.allow_all = True
```

There are many more options for Authenticators in JupyterHub.


## JupyterHub Production Security Checklist

**Important: Security**

Please also go through JupyterHub's production security checklist to make sure
that your installation is not harmful!


## Run via systemctl

Create this file:

```
date; sudo vi /etc/systemd/system/jupyterhub.service
```

Add this content:

```
[Unit]
After=network.target docker.service
Requires=docker.service

[Service]
Type=simple
User=MYUSER [TODO]
WorkingDirectory=/opt/jupyterhub
ExecStart=/opt/jupyterhub/venv/bin/jupyterhub
Restart=always
RestartSec=10
Environment=PATH=/usr/local/bin:/usr/bin:/bin
Environment=PATH=/opt/jupyterhub/venv/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

[Install]
WantedBy=multi-user.target
```

Any changes to this file require running `date; sudo systemctl daemon-reload`.

Now it should be possible to run jupyterhub via:

```
date; sudo systemctl start jupyterhub
sudo systemctl status jupyterhub
```

If something does not work, this log should be helpful: `/var/log/syslog`.


## Dockerspawner: Jupyter Notebooks as Docker containers

We want JupyterHub to start each Notebook as a docker container. This can be
done using the `DockerSpawner` module (
[Documentation](https://jupyterhub-dockerspawner.readthedocs.io/en/latest/),
repo on [GitHub](https://github.com/jupyterhub/dockerspawner)).

Install the `dockerspawner` python package into the python virtual environment:

```
cd /opt/jupyterhub
source venv/bin/activate
which pip
pip install dockerspawner
```

Configure (in `jupyterhub_config.py`):

```
# Tell jupyterhub to use DockerSpawner:
from dockerspawner import DockerSpawner
c.JupyterHub.spawner_class = DockerSpawner

# Tell Dockerspawner which image to use:
c.DockerSpawner.image = "jupyter/r-notebook"
```

Tell DockerSpawner where the individual notebooks can connect to the hub:
c.DockerSpawner.hub_ip_connect = '127.17.0.1'

```
c.DockerSpawner.hub_ip_connect = '127.17.0.1'
# DEPRECATED, now user:
c.JupyterHub.hub_connect_ip = '172.17.0.1'
```

Notebook home directory ([Documentation](https://jupyterhub-dockerspawner.readthedocs.io/en/latest/api/index.html#dockerspawner.DockerSpawner.notebook_dir))

```
c.DockerSpawner.notebook_dir = '/home/participant'
```

From inside the Docker container, the host server can not be accessed directly 
unless configured. By setting the DNS entry `host.docker.internal`, we set it to
`host-gateway`, which is a special Docker keyword that represents the host’s
ip inside the container.

```
c.DockerSpawner.extra_host_config = {
    'extra_hosts': {
        'host.docker.internal': 'host-gateway'
    }
}
```


## JupyterNotebook Docker image with hydrographr

We use a modified docker image, based on the `jupyter/r-notebook` image.

### Files required for building the image

First download the Dockerfile from GitHub (or clone from GitHub).


**Required files for hydrographr:**

Building the image required some data which is copied into the image using
the `COPY` command in the Dockerfile:

```
cat Dockerfile | grep COPY
COPY ./tmp_readonly/lookup_tile_regunit.txt /tmp/
COPY ./tmp_readonly/regional_unit_ovr.tif /tmp/
COPY ./data_readonly/notebook.ipynb /home/participant/data_write/notebook_changeme.ipynb
```

Background:

The hydrographr function `get_regional_unit_id()` (https://github.com/glowabio/hydrographr/blob/main/R/get_regional_unit_id.R)
needs the file `regional_unit_ovr.tif`. The function `get_tile_id()`
(https://github.com/glowabio/hydrographr/blob/main/R/get_tile_id.R) needs the
file `lookup_tile_regunit.txt`.

Usually, hydrographr downloads these files into the temp directory of the
user's current R session when the function gets called. In a workshop
setting, we include it in the image to reduce repeated downloads in each
individual notebook. (For this, the user has to specify `tmp` as the temp
directory when calling the function, otherwise the R session's current specific
temp directory is used, which is newly created for each R session).

So make sure these files are available in `./tmp_readonly`:

* `regional_unit_ovr.tif`: Available at: https://public.igb-berlin.de/index.php/s/agciopgzXjWswF4/download?path=%2Fglobal&files=regional_unit_ovr.tif
* `lookup_tile_regunit.txt`: Included in the hydrographr git repo, and available at: https://drive.google.com/uc?export=download&id=1deKhOEjGgvUXPwivYyH99hgHlJV7OgUv&confirm=t

**Required file: The actual Jupyter Notebook**

To avoid users having to copy-paste the code to be run in the workshop, we
provide it as a ready-to-use jupyter notebook file (`*.ipynb`). So please make
sure this file is available in `./data_readonly`:

* `notebook.ipynb`: Included in the hydrographr git repo.

Note: You can easily create a `*.ipynb` file from a `*.Rmd` file, e.g. from
a vignette.


### Build and test the image

The directory where you build the container (in this case, `/opt/jupyterhub/`)
should now look like this:

```
cd /opt/jupyterhub/
ls -1
|  Dockerfile
|  tmp_readonly
|  data_readonly

ls -1 tmp_readonly/
|  lookup_tile_regunit.txt
|  regional_unit_ovr.tif

ls -1 data_readonly/
|  notebook.ipynb
```

You may have to change the permissions/ownership of data (e.g. by
`chown 1000:100 ...`) for the users to be able to read them. (TODO check).

Now that you have all the required files, build the Docker image:

```
today=$(date '+%Y%m%d')
date; docker build -t workshop:${today} . ; date
```

Test it locally, without JupyterHub:

```
date; docker run -it --name test1 workshop:${today} /bin/bash
```

Now you can start R by typing `R`, import hydrographr
(`library(hydrographr)`) and test the code you want to run.

### Installing missing dependencies

If any dependencies for running the hydrographr / running the vignette are
missing, you can try installing them in R (via `docker run`) or via the
Jupyter Notebook. It may work or not (e.g. depending on whether you need
sudo rights).

But even if it works, these dependencies will be only in available in the
running container, not in the image. So, any other user, in a new container,
will have to install them again. So it is preferable that you add the
installation commands to the Dockerfile and rebuild the image.

**Life hack:**

You may first want to test the installation commands (or figure out which 
dependencies you actually have to install) without rebuilding the image, so
that you don't have to rebuild the image and restart the process after every
new dependency. If you need sudo rights to install, you can create a
(temporary) image that runs as root, by removing the command `USER $NB_UID`
from the Dockerfile. Run a container of that image, test your code, install
the required dependencies, write down the installation commands and later add
them all at once to the normal (non-root) Dockerfile.


## Adding files/data to the containers

You may want to add files to the container, for example input data, or some
vignette as R-markdown file.

Data can be made accessible to the notebook users in various ways:

* Data bind-mounted from a (common) directory on the host
* Data in a docker-volume for each user
* Data baked into the image (added at build-time)
* Data inside the container

### Bind-mounted from host (ideal for read-only data)

This is ideal for read-only data, which is the same for all users. If you mount
it in a read-write mode, every user could interfere with the user's work.

This is how you mount a pre-existing directory
(e.g. `/opt/jupyterhub/data_readonly`) with the read-only data:

```
# Tell Dockerspawner which volumnes to mount:
c.DockerSpawner.volumes = {
    ...
    '/opt/jupyterhub/data_readonly': {
        'bind': '/home/participant/data_readonly',
        'mode': 'ro'
    }
}
```

You may have to change the permissions/ownership of data (e.g. by
`chown 1000:100 ...`) for the users to be able to read them.

Test this by:

```
date; docker run -it --name test_ro -v "/opt/jupyterhub/data_readonly:/home/participant/data_readonly:ro" workshop:${today} /bin/bash
```

**Life Hack**

While preparing a workshop, you can mount the host directory as `read-write`
(instead of `read-only`), so that you can use the JupyterNotebook to download
data into this directory. Then later, mount it as `read-only` to disable users
from modifying it.

### Docker-volume for each user

Create an individual Docker volume for every user that logs in. This volume
persists removal of the docker.

```
# Tell Dockerspawner which volumnes to mount:
c.DockerSpawner.volumes = {
    'jupyter-user-{username}': {
        'bind': '/home/participant/data_write',
        'mode': 'rw'
    },
    ...
}
```

### Data baked into the image (added at build-time)

If you have data you want available for all users in a writeable mode, you can
add it to the docker image, using `COPY`. It will be available in the
container's filesystem. It makes the image bigger, and the data will be gone
when the container is gone.

You could use it to then automatically copy the data to the user's
docker-volume upon login. For the IGB spring workshop, this happens in the
`start.sh` script that gets run when a docker container is started:


### Data inside the container

You can copy data into a running container's file system using `docker cp`, but
it is not really that useful, except for during development / debugging, where
you want to quickly add files without having to go through any of the other
ways.



## Generating Jupyter Notebook files from R-markdown vignette files

To convert a vignette `*.Rmd` file to a Jupyter Notebook `*.ipynb`, run this:

```
date; jupytext --to notebook data_readonly/notebook.Rmd
```

Likely you first have to install `jupytext`:

```
# activate virtual environment:
source venv/bin/activate

# install package:
pip install jupytext
```

If you now copy this `*.ipynb` file to the read-only directory, all participants
will be able to see it, but they cannot make (?) / save changes, so they would
have to first copy it to their own working dir.

Make sure to also change ownership:

```
sudo chown 1000:100 data_readonly/notebook.ipynb
```



## Data for the workshop in spring 2026 at IGB

### Bind-mounted from host (ideal for read-only data)

We are not mounting any read-only data this time. All relevant data is added to
the user's docker volumes, see below.

### Docker-volume for each user

A docker-volume for each user is created when the user logs in and the docker
container is started.

### Data baked into the image (added at build-time)

Some input data (directories `spatial` and `env90m`, files `pred_tab.csv`,
`pred_tab_complete.csv`, `subc_ids.txt`, `workflow_diagram2.png`) is baked
into the image, using `COPY`:

```
COPY ./input_data/spatial /home/bootstrap-data/spatial

COPY \
  ./input_data/chelsa_bioclim_v2_1 \
  ./input_data/esa_cci_landcover_v2_1_1 \
  ./input_data/hydrography90m_v1_0 \
  /home/bootstrap-data/env90m/

# Copy files
COPY \
  ./input_data/notebook_merged__sent20260317afternoon.ipynb \
  ./input_data/workflow_diagram2.png \
  ./input_data/Readme_env90m.md \
  ./input_data/pred_tab.csv \
  ./input_data/pred_tab_complete.csv \
  ./input_data/subc_ids.txt \
  /home/bootstrap-data/
```

Inside the container, it is available in `/home/bootstrap-data`.

As changes to this data would be gone when the containers are gone, we use the
script `start.sh` to copy it over to the user's individual writeable docker
volumes during docker container startup.

## Server cleanup

Several things will fill up the server's disk, so don't forget some
clean-up.

Repeated testing of new images will add docker containers and docker volumes.
Check for unneccessary containers with `docker ps -a` and for unneccessary
volumes with `docker volume list`. Remove with `docker volume rm <volume>`
and with `docker stop <container>; docker rm <container>`.

Running `docker build ...` repeatedly will fill up disk space with cached
build information and cached intermediate images. There is some `prune`
command to clean up - but only do that once you have your final version,
as the next `docker build ...` run afterwards will take much longer.

