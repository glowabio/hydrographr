Hydrography90m v.1.0 - README

June 11, 2022

Data corresponding to the manuscript "Hydrography90m: A new high-resolution global hydrographic dataset" by Giuseppe Amatulli, Jaime Garcia Marquez, Tushar Sethi, Jens Kiesel, Afroditi Grigoropoulou, Maria M. Üblacker, Longzhu Q. Shen, and Sami Domisch (submitted to Earth System Science Data, DOI: https://doi.org/10.5194/essd-2022-9, and DOI (dataset) https://doi.org/10.18728/igb-fred-762.1).

This repository contains all data associated with the Hydrography90m v.1.0 hydrographical network. The data consists of GeoTiff (.tif) raster files and GeoPackage (.gpkg) vector files that are in the following folders and subfolders (see Table below). The main folder refers to the GRASS-GIS module that was used to create the data. 

All data is provided at a global extent, where the "global" folder contains all globally merged, seamless raster layers. These layers have been optimized (internal overviews) for a fast rendering in e.g. QGIS. 

The other folders contain the raster and vector layers tiled in 20x20 degree tiles ("20d") to facilitate the download and data handling. Regarding the tiling, please see the "Regular_tiles_basins.pdf" in the same folder as this README file. The specific tile code is part of each specific *.tif and *.gpkg file name.

The Hydrography90m dataset (DOI: https://doi.org/10.18728/igb-fred-762.1) is protected by the Creative Commons Attribution-NonCommercial 4.0 International License (CC BY-NC 4.0), which permits sharing and adaption under the following terms: Attribution — You must give appropriate credit, provide a link to the license, and indicate if changes were made. You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use. Non-commercial — Use of the material for commercial purposes is strictly prohibited, except with express permission from the licensor. To view a copy of this license, visit http://creativecommons.org/licenses/by-nc/4.0



------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Folder              Sub-folder                       Variable description             Unit                                                                 File name                    
------------------- -------------------------------- -------------------------------- -------------------------------------------------------------------- -------------------------------
flow.index          cti_tiles20d                     Stream power index (spi)         unit-less (scale factor 1000)                                        spi_*.tif                      

flow.index          spi_tiles20d                     Stream transportation index      unit-less (scale factor 1000)                                        sti_*.tif                      
                                                     (sti)                                                                                                                                

flow.index          sti_tiles20d                     Compound topographic index       unit-less (scale factor 1000)                                        cti_*.tif                      
                                                     (cti)                                                                                                                                

r.stream.channel    channel_curv_cel_tiles20d        Cell stream course curvature     m-1 (scale factor 1000000)                                           channel_curv_cel_*.tif         
                                                     of the focal cell                                                                                                                    

r.stream.channel    channel_dist_dw_seg_tiles20d     Segment downstream distance      m                                                                    channel_dist_dw_seg_*.tif      
                                                     between focal cell and the                                                                                                           
                                                     node/outlet                                                                                                                          

r.stream.channel    channel_dist_up_cel_tiles20d     Upstream distance between        m                                                                    channel_dist_up_cel_*.tif      
                                                     focal cell and next cell                                                                                                             

r.stream.channel    channel_dist_up_seg_tiles20d     Segment upstream distance        m                                                                    channel_dist_up_seg_*.tif      
                                                     between focal cell and the                                                                                                           
                                                     init/node                                                                                                                            

r.stream.channel    channel_elv_dw_cel_tiles20d      Downstream elevation             m                                                                    channel_elv_dw_cel_*.tif       
                                                     difference between focal cell                                                                                                        
                                                     and the next cell                                                                                                                    

r.stream.channel    channel_elv_dw_seg_tiles20d      Segment downstream elevation     m                                                                    channel_elv_dw_seg_*.tif       
                                                     difference between focal cell                                                                                                        
                                                     and the node/outlet                                                                                                                  

r.stream.channel    channel_elv_up_cel_tiles20d      Upstream elevation difference    m (outlet cell value = 99999)                                        channel_elv_up_cel_*.tif       
                                                     between focal cell and the                                                                                                           
                                                     next cell                                                                                                                            

r.stream.channel    channel_elv_up_seg_tiles20d      Segment upstream elevation       m                                                                    channel_elv_up_seg_*.tif       
                                                     difference between focal cell                                                                                                        
                                                     and the init/node                                                                                                                    

r.stream.channel    channel_grad_dw_seg_tiles20d     Segment downstream mean          unit-less (scale factor                                              channel_grad_dw_seg_*.tif      
                                                     gradient between focal cell      1000000)                                                                                            
                                                     and the node/outlet                                                                                                                  

r.stream.channel    channel_grad_up_cel_tiles20d     Upstream gradient between        unit-less (scale factor                                              channel_grad_up_cel_*.tif      
                                                     focal cell and the next cell     1000000)                                                                                            

r.stream.channel    channel_grad_up_seg_tiles20d     Segment upstream mean gradient   unit-less (scale factor                                              channel_grad_up_seg_*.tif      
                                                     between focal cell and the       1000000)                                                                                            
                                                     init/node                                                                                                                            

r.stream.distance   outlet_diff_dw_basin_tiles20d    Elevation difference between     m                                                                    outlet_diff_dw_basin_*.tif     
                                                     focal grid cell and the outlet                                                                                                       
                                                     grid cell in the network                                                                                                             

r.stream.distance   outlet_diff_dw_scatch_tiles20d   Elevation difference between     m                                                                    outlet_diff_dw_scatch_*.tif    
                                                     focal grid cell and the                                                                                                              
                                                     downstream stream node grid                                                                                                          
                                                     cell                                                                                                                                 

r.stream.distance   outlet_dist_dw_basin_tiles20d    Distance between focal grid      m                                                                    outlet_dist_dw_basin_*.tif     
                                                     cell and the outlet grid cell                                                                                                        
                                                     in the network                                                                                                                       

r.stream.distance   outlet_dist_dw_scatch_tiles20d   Distance between focal grid      m                                                                    outlet_dist_dw_scatch_*.tif    
                                                     cell and the downstream stream                                                                                                       
                                                     node grid cell                                                                                                                       

r.stream.distance   stream_diff_dw_near_tiles20d     Elevation difference between     m                                                                    stream_diff_dw_near_*.tif      
                                                     focal grid cell and its                                                                                                              
                                                     nearest downstream stream                                                                                                            
                                                     pixel                                                                                                                                

r.stream.distance   stream_diff_up_farth_tiles20d    Elevation difference of the      m                                                                    stream_diff_up_farth_*.tif     
                                                     longest path from focal grid                                                                                                         
                                                     cell to the sub-catchment                                                                                                            
                                                     drainage divide                                                                                                                      

r.stream.distance   stream_diff_up_near_tiles20d     Elevation difference of the      m                                                                    stream_diff_up_near_*.tif      
                                                     shortest path from focal grid                                                                                                        
                                                     cell to the sub-catchment                                                                                                            
                                                     drainage divide                                                                                                                      

r.stream.distance   stream_dist_dw_near_tiles20d     Distance between focal grid      m                                                                    stream_dist_dw_near_*.tif      
                                                     cell and its nearest                                                                                                                 
                                                     downstream stream grid cell                                                                                                          

r.stream.distance   stream_dist_proximity_tiles20d   Euclidean distance between       m                                                                    stream_dist_proximity_*.tif    
                                                     focal grid cell and the stream                                                                                                       
                                                     network                                                                                                                              

r.stream.distance   stream_dist_up_farth_tiles20d    Longest upstream distance        m                                                                    stream_dist_up_farth_*.tif     
                                                     between focal grid cell and                                                                                                          
                                                     the nearest sub-catchment                                                                                                            
                                                     drainage divide                                                                                                                      

r.stream.distance   stream_dist_up_near_tiles20d     Shortest upstream distance       m                                                                    stream_dist_up_near_*.tif      
                                                     between focal grid cell and                                                                                                          
                                                     the nearest sub-catchment                                                                                                            
                                                     drainage divide                                                                                                                      

r.stream.order      order_hack_tiles20d              Hackâ€™s stream order (raster)   categorical                                                          order_hack_*.tif              

r.stream.order      order_horton_tiles20d            Hortonâ€™s stream order (raster) categorical                                                          order_horton_*.tif            

r.stream.order      order_shreve_tiles20d            Shreveâ€™s stream magnitude      categorical                                                          order_shreve_*.tif            
                                                     (raster)                                                                                                                             

r.stream.order      order_strahler_tiles20d          Strahlerâ€™s stream order        categorical                                                          order_strahler_*.tif          
                                                     (raster)                                                                                                                             

r.stream.order      order_topo_tiles20d              Topological dimension of         categorical                                                          order_topo_*.tif              
                                                     streams (raster)                                                                                                                     

r.stream.order      order_vect_tiles20d              All stream segments              various attributes, see also                                         stream_vect_segment_*.gpkg             
                                                     attributes (vector)              https://grass.osgeo.org/grass78/manuals/addons/r.stream.order.html                                  

r.stream.order      order_vect_tiles20d              All stream nodes                 various attributes, see also                                         stream_vect_point_*.gpkg             
                                                     attributes (vector)              https://grass.osgeo.org/grass78/manuals/addons/r.stream.order.html      

r.stream.slope      slope_curv_max_dw_cel_tiles20d   Maximum curvature between        m-1 (scale factor 1000000)                                           slope_curv_max_dw_cel_*.tif    
                                                     highest upstream cell, focal                                                                                                         
                                                     cell and downstream cell                                                                                                             

r.stream.slope      slope_curv_min_dw_cel_tiles20d   Minimum curvature between        m-1 (scale factor 1000000)                                           slope_curv_min_dw_cel_*.tif    
                                                     lowest upstream cell, focal                                                                                                          
                                                     cell and downstream cell                                                                                                             

r.stream.slope      slope_elv_dw_cel_tiles20d        Elevation difference between     m                                                                    slope_elv_dw_cel_*.tif         
                                                     focal cell and downstream cell                                                                                                       

r.stream.slope      slope_grad_dw_cel_tiles20d       Focal cell gradient              unit-less (scale factor                                              slope_grad_dw_cel_*.tif        
                                                                                      1000000)                                                                                            

r.watershed         accumulation_tiles20d            Flow accumulation (raster)       km2                                                                  accumulation_*.tif             

r.watershed         basin_tiles20d                   Drainage basin (raster)          IDs from 1 to 1,676,628                                              basin_*.tif                    

r.watershed         basin_tiles20d                   Drainage basin (vector)          IDs from 1 to 1,676,628                                              basin_*.gpkg                   

r.watershed         depression_tiles20d              Depression (raster)              ID=1                                                                 depression_*.tif               

r.watershed         direction_tiles20d               Flow direction (raster)          NE-N-NW-W-SW-S-SE-E correspond                                       direction_*.tif                
                                                                                      to 1-2-3-4-5-6-7-8                                                                                  

r.watershed         outlet_tiles20d                  Outlets (raster)                 ID=1                                                                 outlet_*.tif                   

r.watershed         outlet_tiles20d                  Outlets (vector)                 ID=1                                                                 outlet_*.gpkg                  

r.watershed         regional_unit                    Regional unit (raster)           IDs from 1 to 116 and IDs from                                       regional_unit_*.tif            
                                                                                      150 to 200                                                                                          

r.watershed         segment_tiles20d                 Stream segment (raster)          IDs from 1 to 726,723,221                                            segment_*.tif                  

r.watershed         sub_catchment_tiles20d           Sub-catchment (raster)           IDs from 1 to 726,723,221                                            sub_catchment_*.tif            

r.watershed         sub_catchment_tiles20d           Sub-catchment (vector)           IDs from 1 to 726,723,221                                            sub_catchment_*.gpkg           

global              -                                Stream power index (spi) -       unit-less (scale factor 1000)                                        cti_ovr.tif                    
                                                     global                                                                                                                               

global              -                                Stream transportation index      unit-less (scale factor 1000)                                        spi_ovr.tif                    
                                                     (sti) - global                                                                                                                       

global              -                                Compound topographic index       unit-less (scale factor 1000)                                        sti_ovr.tif                    
                                                     (cti) - global                                                                                                                       

global              -                                Cell stream course curvature     m-1 (scale factor 1000000)                                           channel_curv_cel_ovr.tif       
                                                     of the focal cell - global                                                                                                           

global              -                                Segment downstream distance      m                                                                    channel_dist_dw_seg_ovr.tif    
                                                     between focal cell and the                                                                                                           
                                                     node/outlet - global                                                                                                                 

global              -                                Upstream distance between        m                                                                    channel_dist_up_cel_ovr.tif    
                                                     focal cell and next cell -                                                                                                           
                                                     global                                                                                                                               

global              -                                Segment upstream distance        m                                                                    channel_dist_up_seg_ovr.tif    
                                                     between focal cell and the                                                                                                           
                                                     init/node - global                                                                                                                   

global              -                                Downstream elevation             m                                                                    channel_elv_dw_cel_ovr.tif     
                                                     difference between focal cell                                                                                                        
                                                     and the next cell - global                                                                                                           

global              -                                Segment downstream elevation     m                                                                    channel_elv_dw_seg_ovr.tif     
                                                     difference between focal cell                                                                                                        
                                                     and the node/outlet - global                                                                                                         

global              -                                Upstream elevation difference    m (outlet cell value = 99999)                                        channel_elv_up_cel_ovr.tif     
                                                     between focal cell and the                                                                                                           
                                                     next cell - global                                                                                                                   

global              -                                Segment upstream elevation       m                                                                    channel_elv_up_seg_ovr.tif     
                                                     difference between focal cell                                                                                                        
                                                     and the init/node - global                                                                                                           

global              -                                Segment downstream mean          unit-less (scale factor                                              channel_grad_dw_seg_ovr.tif    
                                                     gradient between focal cell      1000000)                                                                                            
                                                     and the node/outlet - global                                                                                                         

global              -                                Upstream gradient between        unit-less (scale factor                                              channel_grad_up_cel_ovr.tif    
                                                     focal cell and the next cell -   1000000)                                                                                            
                                                     global                                                                                                                               

global              -                                Segment upstream mean gradient   unit-less (scale factor                                              channel_grad_up_seg_ovr.tif    
                                                     between focal cell and the       1000000)                                                                                            
                                                     init/node - global                                                                                                                   

global              -                                Elevation difference between     m                                                                    outlet_diff_dw_basin_ovr.tif   
                                                     focal grid cell and the outlet                                                                                                       
                                                     grid cell in the network -                                                                                                           
                                                     global                                                                                                                               

global              -                                Elevation difference between     m                                                                    outlet_diff_dw_scatch_ovr.tif  
                                                     focal grid cell and the                                                                                                              
                                                     downstream stream node grid                                                                                                          
                                                     cell - global                                                                                                                        

global              -                                Distance between focal grid      m                                                                    outlet_dist_dw_basin_ovr.tif   
                                                     cell and the outlet grid cell                                                                                                        
                                                     in the network - global                                                                                                              

global              -                                Distance between focal grid      m                                                                    outlet_dist_dw_scatch_ovr.tif  
                                                     cell and the downstream stream                                                                                                       
                                                     node grid cell - global                                                                                                              

global              -                                Elevation difference between     m                                                                    outlet_ovr.tif                 
                                                     focal grid cell and its                                                                                                              
                                                     nearest downstream stream                                                                                                            
                                                     pixel - global                                                                                                                       

global              -                                Elevation difference of the      m                                                                    stream_diff_dw_near_ovr.tif    
                                                     longest path from focal grid                                                                                                         
                                                     cell to the sub-catchment                                                                                                            
                                                     drainage divide - global                                                                                                             

global              -                                Elevation difference of the      m                                                                    stream_diff_up_farth_ovr.tif   
                                                     shortest path from focal grid                                                                                                        
                                                     cell to the sub-catchment                                                                                                            
                                                     drainage divide - global                                                                                                             

global              -                                Distance between focal grid      m                                                                    stream_diff_up_near_ovr.tif    
                                                     cell and its nearest                                                                                                                 
                                                     downstream stream grid cell -                                                                                                        
                                                     global                                                                                                                               

global              -                                Euclidean distance between       m                                                                    stream_dist_dw_near_ovr.tif    
                                                     focal grid cell and the stream                                                                                                       
                                                     network - global                                                                                                                     

global              -                                Longest upstream distance        m                                                                    stream_dist_proximity_ovr.tif  
                                                     between focal grid cell and                                                                                                          
                                                     the nearest sub-catchment                                                                                                            
                                                     drainage divide - global                                                                                                             

global              -                                Shortest upstream distance       m                                                                    stream_dist_up_farth_ovr.tif   
                                                     between focal grid cell and                                                                                                          
                                                     the nearest sub-catchment                                                                                                            
                                                     drainage divide - global                                                                                                             

global              -                                Hackâ€™s stream order (raster) - categorical                                                          stream_dist_up_near_ovr.tif    
                                                     global                                                                                                                               

global              -                                Hortonâ€™s stream order (raster) categorical                                                          order_hack_ovr.tif             
                                                     - global                                                                                                                             

global              -                                Shreveâ€™s stream magnitude      categorical                                                          order_horton_ovr.tif           
                                                     (raster) - global                                                                                                                    

global              -                                Strahlerâ€™s stream order        categorical                                                          order_shreve_ovr.tif           
                                                     (raster) - global                                                                                                                    

global              -                                Topological dimension of         categorical                                                          order_strahler_ovr.tif         
                                                     streams (raster) - global                                                                                                            

global              -                                All stream segments and nodes    various attributes, see also                                         order_topo_ovr.tif             
                                                     attributes (vector) - global     https://grass.osgeo.org/grass78/manuals/addons/r.stream.order.html                                  

global              -                                Maximum curvature between        m-1 (scale factor 1000000)                                           slope_curv_max_dw_cel_ovr.tif  
                                                     highest upstream cell, focal                                                                                                         
                                                     cell and downstream cell -                                                                                                           
                                                     global                                                                                                                               

global                                               Minimum curvature between        m-1 (scale factor 1000000)                                           slope_curv_min_dw_cel_ovr.tif  
                                                     lowest upstream cell, focal                                                                                                          
                                                     cell and downstream cell -                                                                                                           
                                                     global                                                                                                                               

global              -                                Elevation difference between     m                                                                    slope_elv_dw_cel_ovr.tif       
                                                     focal cell and downstream cell                                                                                                       
                                                     - global                                                                                                                             

global              -                                Focal cell gradient - global     unit-less (scale factor                                              slope_grad_dw_cel_ovr.tif      
                                                                                      1000000)                                                                                            

global              -                                Flow accumulation (raster) -     km2                                                                  accumulation_ovr.tif           
                                                     global                                                                                                                               

global              -                                Drainage basin (raster) -        IDs from 1 to 1,676,628                                              basin_ovr.tif                  
                                                     global                                                                                                                               

global              -                                Depression (raster) - global     ID=1                                                                 depression_ovr.tif             

global              -                                Flow direction (raster) -        NE-N-NW-W-SW-S-SE-E correspond                                       direction_ovr.tif              
                                                     global                           to 1-2-3-4-5-6-7-8                                                                                  

global              -                                Regional unit (raster) -         IDs from 1 to 116 and IDs from                                       regional_unit_ovr.tif          
                                                     global                           150 to 200                                                                                          

global              -                                Stream segment (raster) -        IDs from 1 to 726,723,221                                            segment_ovr.tif                
                                                     global                                                                                                                               

global              -                                Sub-catchment (raster) -         IDs from 1 to 726,723,221                                            sub_catchment_ovr.tif          
                                                     global                                                                                                                               
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
