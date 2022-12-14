% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_ids.R
\name{extract_ids}
\alias{extract_ids}
\title{Extract sub-catchment and/or basin IDs for a point location}
\usage{
extract_ids(
  data,
  lon,
  lat,
  id = NULL,
  basin_layer = NULL,
  subc_layer = NULL,
  quiet = TRUE
)
}
\arguments{
\item{data}{a data.frame or data.table with lat/lon coordinates in WGS84}

\item{lon}{character. The name of the column with the longitude coordinates}

\item{lat}{character. The name of the column with the latitude coordinates}

\item{id}{character. The name of a column containing unique IDs for each row
of "data" (e.g., occurrence or site IDs)}

\item{basin_layer}{character. Full path to the basin ID .tif layer}

\item{subc_layer}{character. Full path to the sub-catchment ID .tif layer}

\item{quiet}{Whether the standard output will be printed or not (deafult
is TRUE)}
}
\description{
Extracts the ID value of the basin and/or sub-catchment raster layer at a
given point location.
}
\details{
For the extraction of a value at a given point location from the basin
and/or sub-catchment raster layer of the Hydrography90m dataset, the GDAL
function 'gdallocationinfo' is used. The point locations have to be defined
by coordinates of the WGS84 reference system. The function can also be used
to extract any value from a given raster layer with a WGS84 projection, such
as e.g. environmental information that is stored in the input raster file.
}
\note{
Duplicated rows will be removed.
}
\examples{
# Download test data into temporary R folder
download_test_data(tempdir())

# Load occurrence data
species_occurrence <- read.table(
paste0(tempdir(), "/hydrography90m_test_data/spdata_1264942.txt"),
header = TRUE)

# Define full path to the basin and sub-catchments raster layer
basin_rast <- paste0(tempdir(),
                     "/hydrography90m_test_data/basin_1264942.tif")
subc_rast <- paste0(tempdir(),
                    "/hydrography90m_test_data/basin_1264942.tif")

# Extract basin and sub-catchment IDs from the Hydrography90m layers
hydrography90m_ids <- extract_ids(data = species_occurrence,
                                  lon = "longitude",
                                  lat = "latitude",
                                  id = "occurrence_id",
                                  subc_layer = subc_rast,
                                  basin_layer = basin_rast)

# Show the output table
hydrography90m_ids
}
\references{
\url{https://gdal.org/programs/gdallocationinfo.html}
}
\author{
Afroditi Grigoropoulou, Maria ??blacker
}
