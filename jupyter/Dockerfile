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

# Drop back to non-root for Jupyter
USER $NB_UID

# Install pure-R packages via:
# TODO: Use some dependencies/requirements file?
RUN R -e "install.packages(c('leaflet', 'corrplot', 'tmap', 'usdm', 'ranger', 'pdp', 'geojsonio'), repos='http://cran.rstudio.com/')"
# Install these packages needed for markdown cells:
RUN R -e "install.packages(c('xfun', 'knitr', 'rmarkdown', 'formatR', 'evaluate', 'stringr', 'yaml'), repos = 'https://cran.rstudio.com/')"

# Finally install hydrographr:
# These env variables are needed to point R to the paths:
RUN export LD_LIBRARY_PATH=/opt/conda/lib:$LD_LIBRARY_PATH && \
    export UDUNITS2_LIBS="-L/opt/conda/lib -ludunits2" && \
    export UDUNITS2_INCLUDE="/opt/conda/include" && \
    R -e "remotes::install_github('glowabio/hydrographr@328e1fe72e613b767bf9ba51e3cd6825a5389159')"
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

# Copy temp files needed by hydrographr
COPY ./tmp_readonly/lookup_tile_regunit.txt /tmp/
COPY ./tmp_readonly/regional_unit_ovr.tif /tmp/

# Directory creation as root:
USER root

# Create directories for user:
RUN mkdir -p /home/participant/data_write \
 && mkdir -p /home/participant/data_readonly \
 && chown -R $NB_UID:$NB_GID /home/participant

# Make directory where to add the baked-in input data, and
# add the script that copies the data from there to
# /home/participant/data_write, where we will mount the user's
# writeable docker-volume.
RUN mkdir -p /home/bootstrap-data
COPY start.sh /usr/local/bin/start.sh
RUN chmod +x /usr/local/bin/start.sh

# Drop back to non-root user:
USER $NB_UID

# Copy directories:
COPY ./input_data/spatial /home/bootstrap-data/spatial
COPY ./input_data/env90m  /home/bootstrap-data/env90m

# Copy files
COPY \
  ./input_data/fish_gbif_from_sp_list_clean_to_snap.csv \
  ./input_data/pred_tab.csv \
  ./input_data/pred_tab_complete.csv \
  ./input_data/subc_ids.txt \
  /home/bootstrap-data/
COPY ./input_data/Readme_env90m.md /home/bootstrap-data/env90m/Readme_env90m.md
COPY ./input_data/workflow_20260323.png /home/bootstrap-data/img/workflow_20260323.png
COPY \
   ./input_data/lake_intersection_outlet.PNG \
   ./input_data/lake_with_species.PNG \
   ./input_data/lake_catchment.PNG \
   ./input_data/lakes_with_species.PNG \
   ./input_data/lakes_species_bbox.PNG \
   /home/bootstrap-data/img/

# Copy the Juypter Notebook (the vignette)
# TODO: The part of the SDM had some missing libraries etc.
COPY ./notebook_snapping_sdm.ipynb /home/bootstrap-data/notebook_snapping_sdm.ipynb
COPY ./workflow_diagram_TT.svg /home/bootstrap-data/
COPY ./notebook_lakes_final_final.ipynb /home/bootstrap-data/notebook_lakes.ipynb

# Set PROJ_DATA globally
# Note/TODO: We seem to have two conflicting versions of proj installed - only by conda
# (probably by installing gdal through conda), and one in the system (probably by installing
# gdal-bin python3-gdal through apt-get).
# Two locations are found by running library(sf); sf::sf_proj_info("path"), but only
# '/opt/conda/share/proj/' actually contains 'proj.db', so we set that manually.
# In generall, it would be better to not install gdal twice!!! (TODO!)
# I hope this is early enough...
ENV PROJ_DATA=/opt/conda/share/proj

# Added the data-bootstrapping:
ENTRYPOINT ["/usr/local/bin/start.sh"]
CMD ["start-notebook.py"]
