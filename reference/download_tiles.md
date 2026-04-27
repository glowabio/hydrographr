# Download files of the Hydrography90m dataset

The function downloads data of the Hydrography90m dataset, which is
split into 20°x20° tiles. If a tile ID is specified, then the selected
layers (variable) will be downloaded. In addition, the Hydrography90m is
organized in non-interrupted drainage basins called regional units. If a
regional unit ID (reg_unit_id) is specified, then only the raster mask
of the drainage basin is downloaded (useful for later processing).
Multiple regular tiles, e.g. belonging to regional units, can be
downloaded in a single request. The tile or regional unit IDs can be
obtained using the functions "get_tile_id" and "get_regional_unit_id",
respectively. The files will be stored locally in a folder architecture,
similar as in the data repository, available at
<https://public.igb-berlin.de/index.php/s/agciopgzXjWswF4?path=%2F>.

## Usage

``` r
download_tiles(
  variable,
  file_format = "tif",
  tile_id = NULL,
  reg_unit_id = NULL,
  global = FALSE,
  download_dir = "."
)
```

## Arguments

- variable:

  character vector of variable names. See Details for all the variable
  names.

- file_format:

  character. Format of the requested file ("tif" or "gpkg"). See
  Details.

- tile_id:

  character vector. The IDs of the requested tiles.

- reg_unit_id:

  character vector. The IDs of the requested regional units.

- global:

  logical. If TRUE, the global extent file is downloaded. Default is
  FALSE.

- download_dir:

  character. The directory where the files will be downloaded. Default
  is the working directory.

## Details

In the following table you can find all the variables included in the
Hydrography90m dataset. The column "Variable" includes the variable
names that should be used as an input in the parameter "variable" of the
function. Likewise, the column "File format" contains the input that
should be given to the "file_format" parameter. For more details and
visualisations of the spatial layers, please refer to
<https://hydrography.org/hydrography90m/hydrography90m_layers/>.

|  |  |  |  |  |
|----|----|----|----|----|
| **Variable type** | **Variable name** | **Variable** | **Unit** | **File format** |
| Network | Drainage basin | basin |  | tif |
| Network | Drainage basin | basin |  | gpkg |
| Network | Sub-catchment | sub_catchment |  | tif |
| Network | Sub-catchment | sub_catchment |  | gpkg |
| Network | Stream segment | segment |  | tif |
| Network | Outlet | outlet |  | tif |
| Network | Outlet | outlet |  | gpkg |
| Network | Regional unit | regional_unit |  | tif |
| Network | Flow direction | direction |  | tif |
| Flow | Flow accumulation | accumulation | km^2 | tif |
| Stream slope | Cell maximum curvature | slope_curv_max_dw_cel | 1/m | tif |
| Stream slope | Cell minimum curvature | slope_curv_min_dw_cel | 1/m | tif |
| Stream slope | Cell elevation difference | slope_elv_dw_cel | m | tif |
| Stream slope | Cell gradient | slope_grad_dw_cel |  | tif |
| Stream distance | Shortest distance to drainage divide | stream_dist_up_near | m | tif |
| Stream distance | Longest distance to drainage divide | stream_dist_up_farth | m | tif |
| Stream distance | Nearest down stream stream grid cell | stream_dist_dw_near | m | tif |
| Stream distance | Outlet grid cell in the network | outlet_dist_dw_basin | m | tif |
| Stream distance | Down stream stream node grid cell | outlet_dist_dw_scatch | m | tif |
| Stream distance | Euclidean distance | stream_dist_proximity | m | tif |
| Elevation difference | Shortest path | stream_diff_up_near | m | tif |
| Elevation difference | Longest path | stream_diff_up_farth | m | tif |
| Elevation difference | Nearest downstream stream pixel | stream_diff_dw_near | m | tif |
| Elevation difference | Outlet grid cell in the network | outlet_diff_dw_basin | m | tif |
| Elevation difference | Downstream stream node grid cell | outlet_diff_dw_scatch | m | tif |
| Segment properties | Segment downstream mean gradient | channel_grad_dw_seg |  | tif |
| Segment properties | Segment upstream mean gradient | channel_grad_up_seg |  | tif |
| Segment properties | Cell upstream gradient | channel_grad_up_cel |  | tif |
| Segment properties | Cell stream course curvature | channel curv_cel |  | tif |
| Segment properties | Segment downstream elevation difference | channel_elv_dw_seg |  | tif |
| Segment properties | Segment upstream elevation difference | channel_elv_up_seg |  | tif |
| Segment properties | Cell upstream elevation difference | channel_elv_up_cel |  | tif |
| Segment properties | Cell downstream elevation difference | channel_elv_dw_cel |  | tif |
| Segment properties | Segment downstream distance | channel_dist_dw_seg |  | tif |
| Segment properties | Segment upstream distance | channel_dist_up_seg |  | tif |
| Segment properties | Cell upstream distance | channel_dist_up_cel |  | tif |
| Stream order | Strahler’s stream order | order_strahler |  | tif |
| Stream order | Shreve’s stream magnitude | order_shreve |  | tif |
| Stream order | Horton’s stream order | order_horton |  | tif |
| Stream order | Hack’s stream order | order_hack |  | tif |
| Stream order | Topological dimension of streams | order_topo |  | tif |
| Stream order | Strahler’s stream order | order_vect_segment |  | gpkg |
| Stream order | Shreve’s stream magnitude | order_vect_segment |  | gpkg |
| Stream order | Horton’s stream order | order_vect_segment |  | gpkg |
| Stream order | Hack’s stream order | order_vect_segment |  | gpkg |
| Stream order | Topological dimension of streams | order_vect_segment |  | gpkg |
| Stream reach | Length of the stream reach | order_vect_segment | m | gpkg |
| Stream reach | Straight length | order_vect_segment | m | gpkg |
| Stream reach | Sinusoid of the stream reach | order_vect_segment |  | gpkg |
| Stream reach | Accumulated length | order_vect_segment | m | gpkg |
| Stream reach | Flow accumulation | order_vect_segment | km^2 | gpkg |
| Stream reach | Distance to outlet | order_vect_segment | m | gpkg |
| Stream reach | Source elevation | order_vect_segment | m | gpkg |
| Stream reach | Outlet elevation | order_vect_segment | m | gpkg |
| Stream reach | Elevation drop | order_vect_segment |  | gpkg |
| Stream reach | Outlet drop | order_vect_segment |  | gpkg |
| Stream reach | Gradient | order_vect_segment |  | gpkg |
| Flow index | Stream power index | spi |  | tif |
| Flow index | Sediment transportation index | sti |  | tif |
| Flow index | Compound topographic index | cti |  | tif |

## Note

If there is an error during the download of a file (more likely in case
of files bigger than 3-4GB), you can try to manually download this file
by pasting the link that is returned by the error message in your
browser.

## References

Amatulli G., Garcia Marquez J., Sethi T., Kiesel J., Grigoropoulou A.,
Üblacker M., Shen L. & Domisch S. (2022-08-09 ) Hydrography90m: A new
high-resolution global hydrographic dataset. IGB Leibniz-Institute of
Freshwater Ecology and Inland Fisheries. dataset.
<https://doi.org/10.18728/igb-fred-762.1>

## Author

Afroditi Grigoropoulou

## Examples

``` r
# Download data for two variables in three regular tiles
# to the current working directory
download_tiles(variable = c("sti", "stream_dist_up_farth"),
               file_format = "tif",
               tile_id = c("h00v02","h16v02", "h16v04"))

# Download the global .tif layer for the variable "direction"
# into the temporary R folder or define a different directory
# Define directory
my_directory <- tempdir()
# Download layer
download_tiles(variable = "direction",
               file_format = "tif",
               global = TRUE,
               download_dir = my_directory)

# Download the raster mask of two regional units
# to the current working directory.
download_tiles(variable = "regional_unit",
               file_format = "tif",
               reg_unit_id = c("33","34"))

# Download the raster mask of all regional units
# to the current working directory.
download_tiles(variable = "regional_unit",
               file_format = "tif",
               global = TRUE)
```
