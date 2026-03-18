FROM jupyter/r-notebook

# Use root user to install required packages.
USER root

# Install GRASS GIS dependencies and tools
# (software-properties-common are needed for add-apt-repository)
RUN apt-get update && \
    apt-get install -y software-properties-common && \
    add-apt-repository ppa:ubuntugis/ubuntugis-unstable -y && \
    apt-get update && \
    apt-get install -y --no-install-recommends \
        build-essential \
        g++ gcc make git wget \
        grass grass-core grass-dev grass-gui grass-doc \
        gdal-bin python3-gdal \
        && rm -rf /var/lib/apt/lists/*

# Create dummy GRASS location and install addons
RUN mkdir -p /tmp/grassdata && \
    grass -c EPSG:4326 /tmp/grassdata/dummy_location --exec bash -c "\
        g.extension extension=r.stream.distance && \
        g.extension extension=r.stream.order && \
        g.extension extension=r.stream.snap && \
        g.extension extension=r.stream.basins \
    "

# Clean up dummy GRASS location to reduce image size
RUN rm -rf /tmp/grassdata

# Install r dependencies using conda, as the R in the image is based on conda
RUN conda install -c conda-forge \
    udunits2 gdal proj geos \
    r-sf r-terra r-units r-raster r-remotes \
    && conda clean --all --yes

# Install required packages and GNU parallel
RUN cd /tmp && \
    wget http://ftp.gnu.org/gnu/parallel/parallel-latest.tar.bz2 && \
    tar xjf parallel-latest.tar.bz2 && \
    PARALLEL_DIR=$(find . -maxdepth 1 -type d -name "parallel-*") && \
    cd "$PARALLEL_DIR" && \
    ./configure && make && make install && \
    cd / && rm -rf /tmp/* && \
    apt-get remove -y wget build-essential && \
    apt-get autoremove -y && \
    apt-get clean

# Optional: Silence the citation prompt by accepting the citation
RUN parallel --citation > /dev/null || true

# Install gnu bc:
RUN apt-get update && apt-get install -y bc

# Create directory for user:
RUN mkdir -p /home/participant/work \
 && mkdir -p /home/participant/data_readonly \
 && chown -R $NB_UID:$NB_GID /home/participant

# Drop back to non-root for Jupyter
USER $NB_UID

# Install pure-R packages via:
RUN R -e "install.packages(c('leaflet', 'corrplot', 'tmap', 'usdm', 'ranger', 'pdp'), repos='http://cran.rstudio.com/')"

# Finally install hydrographr:
# These env variables are needed to point R to the paths:
RUN export LD_LIBRARY_PATH=/opt/conda/lib:$LD_LIBRARY_PATH && \
    export UDUNITS2_LIBS="-L/opt/conda/lib -ludunits2" && \
    export UDUNITS2_INCLUDE="/opt/conda/include" && \
    R -e "remotes::install_github('glowabio/hydrographr@dev_pass_tempdir')"

# Back to root user to install some packages via mamba
USER root
# Install compatible R packages with mamba
# Note: Probably we could have installed them further above, but that would have meant
# longer rebuild-time and we were in a hurry.
RUN mamba install -y -c conda-forge \
    r-sf>=1.0.19 \
    r-stars \
    r-tmap \
    r-terra>=1.8.21

# Back to non-root to copy input data:
USER $NB_UID

# These two files are needed by hydrographr. It downloads them automatically,
# but to speed up things, we provide them in tmp:
COPY ./tmp_readonly/lookup_tile_regunit.txt /tmp/
COPY ./tmp_readonly/regional_unit_ovr.tif /tmp/

# Another R dependency we could have installed further up.
RUN R -e "install.packages(c('geojsonio'), repos='http://cran.rstudio.com/')"

# We decide not to use /home/participant/work as work directory for the users,
# but to remove the mkdir statement above, we'd again have rebuild most of the image.
# So this is a dirty quick fix to remove the work dir without having to rebuild everything:
RUN mkdir /home/participant/data_write ; rmdir /home/participant/work

# We decide to use /home/participant/data_write as working directory to the users,
# as it is consistent with /home/participant/data_readonly, which we mount.
# We add the notebook file into data_write, so that users can use it.
COPY ./data_readonly/notebook.ipynb /home/participant/data_write/notebook_changeme.ipynb

