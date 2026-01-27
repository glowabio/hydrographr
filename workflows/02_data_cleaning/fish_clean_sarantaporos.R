library(dplyr)
library(readxl)
library(data.table)

wd <- "/home/grigoropoulou/Documents/Postdoc/projects/sarantaporos/data"
setwd(wd)

nimbus_path <- "/run/user/1000/gvfs/dav:host=nimbus.igb-berlin.de,ssl=true,user=grigoropoulou,prefix=%2Fremote.php%2Fwebdav/afroditi/pygeoapi_hydrographr_test_data"

# Clean fish data
spdata <- read_xlsx("Sarantaporos_Barbus.xlsx") %>%
  rename(Rel_Abund = "Barbus prespensis") %>%
  filter(Rel_Abund > 0) %>% 
  mutate(site_id = paste0("FISH_", row_number())) %>%
  rename(longitude = "Latitude",
         latitude  = "Longtitude") %>%
  select(site_id, longitude, latitude, Rel_Abund)

# Write clean version
fwrite(spdata, "spdata_barbus_clean.csv")
file.copy("spdata_barbus_clean.csv", nimbus_path, overwrite = TRUE)
