# Polygon Simplification
# Zack Taylor
# University of Western Ontario
# zack.taylor@uwo.ca
# May 1, 2024

library(tidyverse)
library(sf)
library(geojsonsf)
library(rio)
library(mapview)
library(rmapshaper)

# load complete set of all polygons

load("~/Dropbox (Personal)/Observatory Dropbox/census/boundaries/observatory_polygons_20240401.RData")

# polygon simplification function ----

polygon_simplify <- function(filename, x, y) {
  
  print(paste0("Loading ", filename))
  sf <- layers_sf[[filename]] %>% # load the correct layer from the R object containing the list of polygons
    st_transform(crs = 4326) %>% # set projection to WGS web mercator
    st_make_valid() # fix invalid 
  
  # as mapshaper can only import shapefiles or geojson files, we need to export the polygon layer as a shapefile
  # so that mapshaper can bring it back in. Right now it actually saves it out to the project folder, but
  # we could use a temp file to streamline it. It would be even better if we could pass the R object
  # directly to mapshaper.
  
  print(paste0("Saving ", filename, " as shapefile"))
  sf::st_write(sf, dsn = paste0(filename, ".shp"), delete_dsn = TRUE)
  
  # we need to calculate the maximum and minimum polygon perimeter length using the exported shapefile
  # we use the exported reprojected shapefile because that is what mapshaper is going to work on.
  # we need the max and min to normalize the percentage reduction value.
  
  sf2 <- st_read(paste0(filename, ".shp"))
  maxA = max(sf2$Shp_Lng)
  minA = min(sf2$Shp_Lng)
    
  # now we run the mapshaper command:
  # it requires the simp_length.js file, which contains the calculation of the percentage reduction
  # in vertices, which is passed to the -simplify variable percentage argument.
  # x = minimum percentage simplificatioh (below we use 0.6) - for shortest perimeter polygons
  # y - maximum percentage simplification (below we use 0.98) - for longest perimeter polygons

  system2(command = "mapshaper", args = paste0(filename, ".shp -require simp_length.js alias=_ -simplify variable percentage='_.percentage(this, ", x, ", ", y, ", ", minA, ", ", maxA, ")' weighted weighting=1 stats keep-shapes -o ", filename, "_simplified.shp"))

  print(filename)
  print(paste0("x = ", x))
  print(paste0("y = ", y))
  print(paste0("minA = ", minA))
  print(paste0("maxA = ", maxA))

  print(paste0("Loading ", filename, "_simplified"))
  simp <- sf::st_read(paste0(filename, "_simplified.shp")) 
  
  # we use the npts function in the mapview package to calculate the number of vertices 
  # in the original and simplified polygon layers. This could also be returned.
  
  print(paste0("Vertices before: ", mapview::npts(layers_sf[[filename]])))
  print(paste0("Vertices after: ", mapview::npts(simp)))
  
  # return the simplified polygon
  
  return(simp)
  
}

# polygon dissolve function ----

# this function uses rmapshaper to dissolve the CSD layers to CDs and PRs based on the 
# from = source layer (e.g., csd_2021_simplified)
# to = target layer name (e.g., cd_2021)
# field = geographic identification code (e.g. geosid)
# start = start character location of the "field" to capture
# end = end character location of the "field" to capture
#
# e.g., for geosid = 3512005, where 35 is the PR code, 12 is the CD code, and 005 is the CSD code,
# if you wanted to dissolve up to PRs, you would set start = 1, end = 2
#
# This of course only works if your geographical identification code is hierarchical


polygon_dissolve <- function(from, to, field, start, end) {
  
  sf <- get(from) %>%
    mutate(uid = str_sub(get(field), start, end)) %>%
    ms_dissolve(field = "uid", sys = TRUE) %>%
    rename(geosid = uid) %>%
    left_join(layers_sf[[to]] %>% st_drop_geometry(), by = "geosid")
  
  print(paste0("From ", from, " to ", to))
  print(paste0("Vertices of original: ", mapview::npts(layers_sf[[to]])))
  print(paste0("Vertices after: ", mapview::npts(sf)))
  
  return(sf)
  
}

# running the code to simplify the 2021 CSD layer and dissolve it to CD and PR

csd_2021_simplified <- polygon_simplify(filename = "csd_2021", x = 0.6, y = 0.98)
cd_2021_simplified <- polygon_dissolve(from = "csd_2021_simplified", to = "cd_2021", field = "geosid", start = 1, end = 4)
pr_2021_simplified <- polygon_dissolve(from = "csd_2021_simplified", to = "pr_2021", field = "geosid", start = 1, end = 2)

# write the results as geopackages for quick viewing in QGIS

sf::st_write(csd_2021_simplified, dsn = "csd2021_simplified.gpkg", delete_dsn = TRUE)
sf::st_write(cd_2021_simplified, dsn = "cd2021_simplified.gpkg", delete_dsn = TRUE)
sf::st_write(pr_2021_simplified, dsn = "pr2021_simplified.gpkg", delete_dsn = TRUE)

# do the same for 2001

csd_2001_simplified <- polygon_simplify(filename = "csd_2001", x = 0.6, y = 0.98)
cd_2001_simplified <- polygon_dissolve(from = "csd_2001_simplified", to = "cd_2001", field = "geosid", start = 1, end = 4)
pr_2001_simplified <- polygon_dissolve(from = "csd_2001_simplified", to = "pr_2001", field = "geosid", start = 1, end = 2)

sf::st_write(csd_2001_simplified, dsn = "csd2001_simplified.gpkg", delete_dsn = TRUE)
sf::st_write(cd_2001_simplified, dsn = "cd2001_simplified.gpkg", delete_dsn = TRUE)
sf::st_write(pr_2001_simplified, dsn = "pr2001_simplified.gpkg", delete_dsn = TRUE)

# save to an R object

save(list = c("csd_2021_simplified", "cd_2021_simplified", "pr_2021_simplified",
              "csd_2001_simplified", "cd_2001_simplified", "pr_2001_simplified"
              ), file = "polygons_simplified.RData")

# for reference, here is the javascript simp_length.js
# Ideally we would find a way to pass it directly to the mapshaper command call in R 
# rather than have it as a separate file.

# basically what it does is use a log transform so that polygons with longer perimeters 
# have higher simplication percentages. I could imagine allowing other transformations
# that would be steeper or shallower.

simp_length.js <- "
function percentage(a, x, y, minA, maxA) {

    console.log( a.perimeter, x, y, minA, maxA );

    // Ensure a.perimeter is greater than min(a.perimeter) to avoid NaN results
    
    var p = minA;
    
    if ( a.perimeter <= minA ) { p = minA; } else { p = a.perimeter; }

    // calculation of percentage

    var pct = 1 - ((Math.log10(p - minA + 1) / Math.log10(maxA - minA + 1)) * (y - x) + x);
    
    return pct;
    
    console.log( pct );
      
}

exports.percentage = percentage
"