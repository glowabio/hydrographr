% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_predict_table.R
\name{get_predict_table}
\alias{get_predict_table}
\title{Get predict table}
\usage{
get_predict_table(
  variable,
  statistics = "ALL",
  tile_id,
  input_var_path,
  subcatch_id,
  out_file_path,
  n_cores = NULL,
  read = TRUE,
  quiet = TRUE
)
}
\arguments{
\item{variable}{character vector of variable names. Possible values are:
c("bio1", "bio10", "bio11", "bio12", "bio13","bio14", "bio15", "bio16",
"bio17", "bio18","bio19", "bio2", "bio3", "bio4", "bio5","bio6", "bio7",
"bio8", "bio9", "c100", "c10", "c20", "c30", "c40", "c50","c60", "c70",
"c80", "c90", "chancurv","chandistdwseg", "chandistupcel", "chandistupseg",
"chanelvdwcel", "chanelvdwseg", "chanelvupcel","chanelvupseg",
"changraddwseg", "changradupcel", "changradupseg", "elev", "flow",
"flowpos", "gradient", "length", "out", "outdiffdwbasin", "outdiffdwscatch",
"outdistdwbasin", "outdistdwscatch", "outlet", "slopdiff", "slopgrad",
"soil", "source", "strdiffdwnear", "strdiffupfarth", "strdiffupnear",
"strdistdwnear", "strdistprox", "strdistupfarth", "strdistupnear",
"stright").}

\item{statistics}{character vector of statistics names. Possible values are
"sd", "mean", "range" or "ALL". Default "ALL".}

\item{tile_id}{character. The IDs of the tiles of interest.}

\item{input_var_path}{path to table with environmental variables for entire
tiles.}

\item{subcatch_id}{path to a text file with subcatchments ids.}

\item{out_file_path}{character. The path to the output file.}

\item{n_cores}{numeric. Number of cores used for parallelization.}

\item{read}{logical. If TRUE, the table with environmental variables gets
read into R.
If FALSE, the table is only stored on disk. Default is TRUE.}

\item{quiet}{logical. If FALSE, the standard output will be printed.
Default is TRUE.}
}
\value{
The function returns a table with
\itemize{
\item sub-catchment ID (subcID)
\item a column for each descriptive statistic of each variable (eg. bio1_mean:
mean of the variable bio1)
}
}
\description{
This function creates a table with environmental variables from
an specific subset of subcatchments.
}
\examples{
# Download test data into the temporary R folder
# or define a different directory
my_directory <- tempdir()
download_test_data(my_directory) # TODO make test data available for download!

# Define variable and tile:
var <- c("bio1")
tile_id <- c("h18v02")

# Point to input data
in_path <- paste0(my_directory, '/hydrography90m_test_data')
subc_ids <- paste0(my_directory, '/hydrography90m_test_data/subc_IDs.txt')
output <- paste0(my_directory, '/hydrography90m_test_data/predictTB.csv')

# Run the function with 2 cores and calculate all statistics:
get_predict_table(variable = var,
                  statistics = c("ALL"),
                  tile_id = tile_id,
                  input_var_path = in_path,
                  subcatch_id = subc_ids,
                  out_file_path = output,
                  read = FALSE, quiet = FALSE,
                  n_cores = 2)

# Now you can see the result in /tmp/.../hydrography90m_test_data/predictTB.csv

}
\author{
Jaime García Márquez, Yusdiel Torres-Cambas
}
