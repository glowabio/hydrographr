FROM jupyter/r-notebook

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

# Install bc and parallel
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

# Create directories for user:
RUN mkdir -p /home/participant/data_write \
 && mkdir -p /home/participant/data_readonly \
 && chown -R $NB_UID:$NB_GID /home/participant

# Drop back to non-root for Jupyter
USER $NB_UID

# Install pure-R packages via:
# TODO: Use some dependencies/requirements file?
RUN R -e "install.packages(c('leaflet', 'corrplot', 'tmap', 'usdm', 'ranger', 'pdp', 'geojsonio'), repos='http://cran.rstudio.com/')"

# Finally install hydrographr:
# These env variables are needed to point R to the paths:
RUN export LD_LIBRARY_PATH=/opt/conda/lib:$LD_LIBRARY_PATH && \
    export UDUNITS2_LIBS="-L/opt/conda/lib -ludunits2" && \
    export UDUNITS2_INCLUDE="/opt/conda/include" && \
    R -e "remotes::install_github('glowabio/hydrographr@f6523a6c6ebe97ed13a559aa71610caa38a7af00')"
# Test during build whether the installation was successful:
RUN R -e "library(hydrographr)"

# Install mamba packages as root
# TODO: Install this above?
USER root
# Install compatible R packages with mamba
RUN mamba install -y -c conda-forge \
    r-sf>=1.0.19 \
    r-stars \
    r-tmap \
    r-terra>=1.8.21

# Drop back to non-root user:
USER $NB_UID

# Add temp files required by hydrographr
COPY ./tmp_readonly/lookup_tile_regunit.txt /tmp/
COPY ./tmp_readonly/regional_unit_ovr.tif /tmp/

# Make directory where to add the baked-in input data, and
# add the script that copies the data from there to
# /home/participant/data_write, where we will mount the user's
# writeable docker-volume.
USER root
RUN mkdir -p /home/bootstrap-data
COPY start.sh /usr/local/bin/start.sh
RUN chmod +x /usr/local/bin/start.sh

# Drop back to non-root user:
USER $NB_UID

# Copy directories:
COPY ./input_data/spatial /home/bootstrap-data/spatial
COPY \
  ./input_data/chelsa_bioclim_v2_1 \
  ./input_data/esa_cci_landcover_v2_1_1 \
  ./input_data/hydrography90m_v1_0 \
  /home/bootstrap-data/env90m/

# Copy files
COPY \
  ./input_data/fish_gbif_from_sp_list_clean_to_snap.csv \
  ./input_data/pred_tab.csv \
  ./input_data/pred_tab_complete.csv \
  ./input_data/subc_ids.txt \
  /home/bootstrap-data/
COPY ./input_data/Readme_env90m.md /home/bootstrap-data/env90m/Readme_env90m.md
COPY ./input_data/workflow_diagram2.png /home/bootstrap-data/img/workflow_diagram2.png

# Copy the Juypter Notebook (the vignette)
COPY ./notebook_workshop_igb_20260317.ipynb /home/bootstrap-data/

# Added the data-bootstrapping:
ENTRYPOINT ["/usr/local/bin/start.sh"]
CMD ["start-notebook.py"]
