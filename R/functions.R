
clip_linestring_by_poly <- function(linestring, clipping_polygon,clipping_polygon_buffer_size = 10, n_grid_cells = 100,cap_style = "FLAT",join_style = "BEVEL"){
  
  clipping_polygon_buffer <- st_buffer(clipping_polygon,dist=clipping_polygon_buffer_size,endCapStyle = cap_style,joinStyle = join_style)#buffer clipping polygon by amount
  polygon_ls <- st_cast(clipping_polygon_buffer,"MULTILINESTRING") %>% st_cast("LINESTRING") %>% st_geometry() #convert to linestring
  int_points <- st_intersection(polygon_ls,linestring) %>% #find intersection of road lines and centroid buffers
    st_cast("MULTIPOINT") %>%
    st_cast("POINT")#cconvert to points
  
  int_points_buff <- st_buffer(int_points,dist = 0.0000001) %>% st_union() %>% st_combine() #buffer points to nonzero distance
  
  grid <- st_make_grid(polygon_ls,what = "polygons",n = c(round(sqrt(n_grid_cells)),round(sqrt(n_grid_cells))))#split bounding box of polygon into grid
  grid <- grid[st_intersects(grid,int_points_buff,sparse = F)[,1]]#remove grid cells that do not intersect with the roads
  #iterate through each grid cell and intersect roads with grid cell, split the line geometry within that cell by the input polygon boundary
  for (i in 1:length(grid)) {
    
    temp <- st_intersection(int_points_buff, grid[i])
    linestring <- st_difference(linestring, temp)
    
    print(paste0(i," of ",length(grid), " grid cells complete (", round(i / length(grid) * 100, 1),
                 "%)"))
    
  }
  
  
  
  
  
  output_linestring <- linestring %>%
    st_cast("MULTILINESTRING") %>%
    st_cast("LINESTRING")   %>% #convert to linestring to isolate non-contiguous lines
    st_filter(y = clipping_polygon,.predicate = st_intersects) %>% 
    mutate(length_m = st_length(.))
  
  return(output_linestring)
  
}
