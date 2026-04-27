# Download test data

Download the test data of the package, which includes all Hydrography90m
and species point observation data for a small geographic extent, to
test the functions.

The test data will be automatically downloaded and unzipped with this
function to a desired path, or can be alternatively downloaded at

<https://drive.google.com/file/d/1kYNWXmtVm6X7MZLISOePGpvxB1pk1scD/view?usp=share_link>.

## Usage

``` r
download_test_data(download_dir = ".")
```

## Arguments

- download_dir:

  character. The directory where the files will be downloaded. Default
  location is the working directory.

## Details

Downloads the test data of the Hydrography90m dataset

## References

Amatulli, G., Garcia Marquez, J., Sethi, T., Kiesel, J., Grigoropoulou,
A., Üblacker, M. M., Shen, L. Q., and Domisch, S.: Hydrography90m: a new
high-resolution global hydrographic dataset, Earth Syst. Sci. Data, 14,
4525–4550, [https://doi.org/10.5194/essd-14-4525-2022,
2022.](https://doi.org/10.5194/essd-14-4525-2022,%202022.)

Amatulli G., Garcia Marquez J., Sethi T., Kiesel J., Grigoropoulou A.,
Üblacker M., Shen L. & Domisch S. (2022-08-09 ). Hydrography90m: A new
high-resolution global hydrographic dataset. IGB Leibniz-Institute of
Freshwater Ecology and Inland Fisheries. dataset.
<https://doi.org/10.18728/igb-fred-762.1>

## Author

Afroditi Grigoropoulou

## Examples

``` r
# Download the test data to the current working directory
download_test_data()

# Download the data to a specific (existing) directory
download_test_data("path/to/your/directory")
```
