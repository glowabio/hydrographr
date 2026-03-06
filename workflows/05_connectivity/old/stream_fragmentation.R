library(sf)
library(sfnetworks)
library(igraph)
library(tidygraph)
library(reshape2)
library(dplyr)
library(leaflet)
library(htmlwidgets)

#devtools::install_github("glowabio/hydrographr", ref = "workflow")
library(hydrographr)


setwd("/mnt/shared/connectivity")
setwd("~/proyectos/workflow")


# read graph
#vjosa_streams = st_read("vjosa_partial.gpkg")
#streamSA = st_read("partial_segment_Greece_with_upbuffer_2.gpkg")
streamSA = st_read("partial_stream_network.gpkg")
streamSA

# the description says 30 geometries are empty (probably no edges? outlets?)
# Keep non-empty geometries
clean_sf <- streamSA[!st_is_empty(streamSA), ]

# convert to sfnetwork
net = as_sfnetwork(clean_sf, directed=FALSE)
net

# net shows 617 components: each component represents a Basin

##-----------------------------------------------------------------------------
#display

##-----------------------------------------------------------------------------

# Calculate total length for each connected component
component_lengths <- net %>%
  # 1. Start with nodes to identify components
  activate("nodes") %>%
  mutate(comp_id = group_components()) %>%
  
  # 2. Switch to edges to calculate spatial lengths
  activate("edges") %>%
  mutate(length = edge_length()) %>%
  
  # 3. Bring the component ID from nodes to edges
  # .N() access node data of the current edge's endpoints
  mutate(comp_id = .N()$comp_id[from]) %>%
  
  # 4. Summarize
  as_tibble() %>%
  group_by(comp_id) %>%
  summarise(total_length = sum(length))

component_lengths
summary(component_lengths$total_length)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 6.970e+01 9.730e+02 2.431e+03 1.503e+05 1.366e+04 1.571e+07



# extract only one of the components

# step1 : create an id for each component
nestor <- net %>%
  activate("nodes") %>%
  mutate(comp_id = group_components())

# step 2 extract the component related to basin id N. 1294609
# MESTA / NESTOS RIVER Sub-basin
# 2. Find which comp_id is associated with your target edge
# We use .N() to look up the 'comp_id' of the 'from' node of the target edge
target_id <- nestor %>%
  activate("edges") %>%
  filter(basin_id == 1294609) %>%
  # Use terminal nodes of the edge to find the component ID
  # Since all nodes in a component share one ID, we just need one
  mutate(edge_comp = .N()$comp_id[from]) %>% 
  pull(edge_comp) %>%
  unique()

# step 3: Filter the network to keep that entire component
stream <- nestor %>%
  activate("nodes") %>%
  filter(comp_id %in% target_id)

#####  DISPLAY



#(sub-basin shared by by Bulgaria and Greece)
g1 = st_as_sf(stream, "edges")
st_write(g1, "stream_target.shp")

# measure the length of the network
total_length <- stream %>%
  activate("edges") %>%
  st_as_sf() %>%      # Extract edges as an sf object
  st_length() %>%    # Calculate lengths
  sum()              # Sum them up

total_length
# 4164055 [m]

# access underlaying igraph object
#igobj = as.igraph(net)gg


####   here network indices can be calculated gg
# Example: Calculating betweenness centrality
edge_centrality_between = stream %>%
  activate("edges") %>%
  mutate(bc = centrality_edge_betweenness(weights = edge_length()))
#g1 = st_as_sf(edge_centrality_between, "edges")
#st_write(g1, "edge_centrality_betwee.shp")

###########################
###############  FISH

# read points species occurrences
fish_orig = read.csv("fish_all_species_snapped.csv")

# subcid in nestor
snes = stream  %>%
    activate("edges") %>%
    pull(subc_id) 

#### to get basin ID
#fish_bi = "https://nimbus.igb-berlin.de/index.php/s/zjWHEB7odjifdj8/download/fish_all_species_snapped.csv"
#basin_ids <- api_get_local_ids(csv_url = fish_bi,
#colname_lon = "longitude_snapped",
#colname_lat = "latitude_snapped",
#colname_site_id = "site_id",
#colname_subc_id = "subc_id",
#which_ids = "basin_id"
#)

# remove NAs
# convert to sf object
# Filter to only those within the network's bounding box
fish = fish_orig %>%
    drop_na(longitude_snapped, latitude_snapped) %>%
    filter(subc_id %in% snes) %>%
#    st_as_sf(  coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

# how many ocurrences there are? 300

#st_write(fish, "fish_nestor.shp")

###  use only one species as an example: Squalius orpheus
sqor = fish %>%
    filter(species == "Squalius orpheus" | species == "Squalius_orpheus") %>%
    distinct_at(vars(subc_id), .keep_all = TRUE)
# Simple feature collection with 20 features and 15 field
#st_write(sqor, "sqor_nestor.shp")

####  PLOT: visualize location of species occurrences

# calculated distances between fish occurrence locations assuming full connected network
distf = stream %>%
    activate("edges") %>%
    st_network_cost(from = sqor, to = sqor) %>%
    unclass() %>%
    melt()

#for 20 points there a total possible 190 connections
# formula = (n*(n-1))/2

# create data frame with conection between points and without duplicates or
# distance equal zero
dist_all = distf %>%
    distinct(
    # Create temporary standardized pairs
    min_val = pmin(Var1, Var2), 
    max_val = pmax(Var1, Var2), 
    .keep_all = TRUE
  )  %>%
    filter(value != 0)

### PLOT: boxplot of distances?

#write.table(distf, "distf.txt")


############################
##########  DAMS

# read points dams
dams_orig = read.csv("dams_snapped_points.csv")

# filter dams only for Nestor basin
dams = dams_orig %>%
    filter(subc_id %in% snes) %>%
#    st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

#st_write(dams, "dams_nestor.shp")

# how many dams there are? 46 

# what type of dams are there?
levels(as.factor(dams$status))
# "existing" "planned" 

# first check existing dams
# extract only existing dams
dexist = dams[dams$status == "existing",]
#st_write(dexist, "dams_existing.shp",  layer_options = "OVERWRITE=trues )
dexist

# how many dams there are? 8
###  PLOT???

# break the graph

## first identify subcids where the dams are located. We need to remove those edges
# list of subcid to remove
rmsubc = unique(dexist$subc_id)

# remove edges with the specific subcids (where dams are located)
snet = stream %>%
    activate(edges) %>%
    filter(!(subc_id %in% rmsubc))

snet

components(snet)$no
components(snet)$csize # size refers to the number of nodes

### PLOT = showing each component in a different color

#dec = decompose(snet)
#g1 = as_sfnetwork(dec[[1]], directed=FALSE)
#g1 = st_as_sf(g1, "edges")
#st_write(g1, "g1.shp")

## calculate distances again with broken network by existing dams
diste = snet %>%
    activate("edges") %>%
    st_network_cost(from = sqor, to = sqor) %>%
    unclass() %>%
    melt()

####  how many conections are broken?
dist_existing = diste %>%
    filter(value != "Inf") %>%
    distinct(
    # Create temporary standardized pairs
    min_val = pmin(Var1, Var2), 
    max_val = pmax(Var1, Var2), 
    .keep_all = TRUE
  )  %>%
    filter(value != 0)

# how many remaining connections
nrow(dist_existing)
## 51 connections are broken
## 190 - 51 = 139 remaining

a = data.frame('connected' = distdf$value)
b = data.frame('disconnected' = diste$value)
boxplot(dplyr::bind_rows(a, b))
boxplot(dplyr::bind_rows(a, b), ylab="Distance (km)", cex.lab=1.5, cex.axis=2)
