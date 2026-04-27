# Internal function that downloads a single file from https://public.igb-berlin.de/index.php/s/agciopgzXjWswF4?path= GDrive folder. It is called and inherits arguments by the function 'download_tiles()'.

Internal function that downloads a single file from
https://public.igb-berlin.de/index.php/s/agciopgzXjWswF4?path= GDrive
folder. It is called and inherits arguments by the function
'download_tiles()'.

## Usage

``` r
download_tiles_base(
  variable,
  file_format = "tif",
  tile_id = NULL,
  global = FALSE,
  download_dir = ".",
  file_size_table = NULL,
  server_url = NULL
)
```

## Arguments

- variable:

  character vector of variable names.

- file_format:

  character. Format of the requested file ("tif" or "gpkg").

- tile_id:

  character. The ID of the requested tile or regional unit.

- global:

  logical. If TRUE, the global extent file is downloaded. Default is
  FALSE.

- download_dir:

  character. The directory where the files will be downloaded. Default
  is the working directory.

- file_size_table:

  data.frame. Lookup table including file names and sizes (inherited by
  'download_tiles()').

- server_url:

  character. url to the the home download folder in either Nimbus or
  GDrive (inherited by 'download_tiles()').
