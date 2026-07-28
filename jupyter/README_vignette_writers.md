# Jupyter Notebooks for workshops

Merret Buurman, IGB, 2026-03-25


## Who are these instructions for?

These instructions are for the developers of R vignettes that should be
converted to functioning Jupyter Notebooks, e.g. to be run in workshops.
These instructions tell you what you need to provide to the person who sets up
the server where JupyterHub runs, so that that person can create a
Jupyter-Notebook-Docker-Image, a Jupyter-Notebook file (`.ipynb`), and make
sure all the input data is available.


## What is needed to make a workshop-specific Jupyter setup

The Jupyter Notebooks will run on a server (instructions for setting up the
server are in another README file), in _Docker containers_ that have **all the
necessary programs/software/libraries** pre-installed. So one part of the
workshop preparation is writing a _Dockerfile_ and building a _Docker image_
from it, for which the required software has to be defined.

Then we can also provide the **R code** or vignette that workshop participants
will run as a Notebook file. Alternatively, they could type the code themselves,
or copy-paste from somewhere else.

Finally, we should provide the **input data**, as downloading it to the server
during the workshop is probably a waste of time, network bandwidth, and disk
space (as every user would have to download the same data, to the same
server...). Intermediate results can also be provided, to allow lost users to
jump back in at a later stage of the workshop.


## What you need to provide

Very short:

* All dependencies (packages/software that needs to be pre-installed), and
  possibly an installation-check command
* All input data, and possibly intermediate results
* Your R code / vignette, as R markdown file (`.Rmd`) or (much better!) as a
  Notebook file (`.ipynb`), and possibly images you want to include

### For preparing the Dockerfile

Please provide everything that needs to be pre-installed to be able to
smoothly run your R code / vignette:

* ... the **version of hydrographr** that needs to be installed (ideally:
      which branch and which commit).
* ... the names of any **further R packages** that have to be installed
      (e.g. _leaflet_, _corrplot_, _usdm_, _ranger_, etc.).
* ... all **other dependencies** which have to be installed
      (e.g. the so-called "Guidos Toolbox"), and the exact commands on how to
      install them. Please try to remember if you had to install something else
      before (e.g. installing "pktools" before installing "Guidos Toolbox").
* ... a **check command** (if you can): A command that checks whether the
      installation worked fine (e.g. `gdalinfo --version ` for "gdal"). Such
      checks can be added to the Dockerfile so that an installation failure
      would be noticed during the preparation of the Docker, and not just when
      running/testing the code in the notebook.


### For preparing the Notebook file

Please provide...

* ... the **R code** that the participants should be running,
      as **R markdown** file (`.Rmd`), or as a **Notebook file** (`.ipynb`).
* ... any **images** you may want to display in the vignette.

**To convert** an R markdown file to an `.ipynb` note you can use the python
tool `jupytext`.

If you are already working on a Notebook and just want to send changes, please
directly send the `.ipynb` file, which you can **download from JupyterHub**
(by rightclicking on the notebook file). Please make sure the Notebook file is
either _"wiped"_ (i.e. no results are pre-displayed, so that the participants
see a nice empty Notebook), or that the order of execution of the cells is
_"clean"_ (so that the cell numbers which indicate in which order they were run
are in a clean sequence). Just run the code cells one after the other from the
first to the last to clean the order of execution, before downloading.

To "wipe" a Notebook file before downloading, go to menu _"Kernel"_ and select
_"Restart Kernel and Clear Output of All Cells"_.


### For preparing the data directories

Input data may be quite big, so we add it to the server already. This way we
avoid that all download the same data to the same server, possibly using slow
eduroam or conference WiFi.

So please provide...

* ... input data that would be downloaded using hydrographr
      (provide both the files and the lines of R code to download them).
* ... any other input data
* ... possibly intermediate results, so that participants who couldn't follow
      for some reason can jump back in.

**How does the data get to the server?**

The person who sets up the server can download that input data directly on the
server using R/hydrographr on the server, or download it from wherever you make
it available (Nimbus is complicated, because it needs login to download - other
options without login would be preferred). Or (maybe easier) they can give you
write-permissions on the `read_only` directory during workshop preparation,
so that you can add all the inputs there yourself.


## Important: Directory paths (where to store)

The input data can go either into a _read-only_ directory which is available to
all users to read/copy data from (`/home/participant/data_readonly/`).
This option saves disk space, but users cannot modify the data in place, or
store new files into this directory.

Or we can place them in the user's own writeable directory
(`/home/participant/data_write/`), but that uses a lot of disk space and
makes the Notebook startup quite slow. So it is best to put all the inputs
into `data_readonly`, and use `data_write` only for the users' own results.

**This means that you may have to modify your vignette / R code** so that data
is _read_ from `data_readonly`, but results are _written_ to `data_write`.

