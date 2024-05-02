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

## access here:
## https://www.dropbox.com/scl/fi/zffauot0qqespwp0gwshx/observatory_polygons_20240401.RData?rlkey=209kzl7k6nsz506zd3v36xlcl&dl=1

# polygon simplification function ----
# requires the creation of two subfolders: "_temp" and "_path"
# requires a local instance of mapshaper

polygon_simplify <- function(filename, x, y) {
  
  print(paste0("Loading ", filename))
  sf <- layers_sf[[filename]] %>% # load the correct layer from the R object containing the list of polygons
    st_transform(crs = 4326) %>% # set projection to WGS 84 Web Mercator
    st_make_valid() # fix invalid 
  
  # as mapshaper can only import shapefiles or geojson files, we need to export the polygon layer as a shapefile
  # so that mapshaper can bring it back in. Right now it actually saves it out to the project folder, but
  # we could use a temp file to streamline it. It would be even better if we could pass the R object
  # directly to mapshaper.
  
  print(paste0("Saving ", filename, " as shapefile"))
  sf::st_write(sf, dsn = paste0("_temp/", filename, ".shp"), delete_dsn = TRUE)
  
  # we need to calculate the maximum and minimum polygon perimeter length using the exported shapefile
  # we use the exported reprojected shapefile because that is what mapshaper is going to work on.
  # we need the max and min to normalize the percentage reduction value.
  
  sf2 <- st_read(paste0("_temp/", filename, ".shp"), quiet = TRUE)
  maxA = max(sf2$Shp_Lng)
  minA = min(sf2$Shp_Lng)
    
  # now we run the mapshaper command:
  # it requires the simp_length.js file, which contains the calculation of the percentage reduction
  # in vertices, which is passed to the -simplify variable percentage argument.
  # x = minimum percentage simplificatioh (below we use 0.6) - for shortest perimeter polygons
  # y - maximum percentage simplification (below we use 0.98) - for longest perimeter polygons

  system2(command = "mapshaper", args = paste0("_temp/", filename, ".shp -quiet -require simp_length.js alias=_ -simplify variable percentage='_.percentage(this, ", x, ", ", y, ", ", minA, ", ", maxA, ")' weighted weighting=1 stats keep-shapes -o _temp/", filename, "_simplified.shp"))

  print(filename)
  print(paste0("x = ", x))
  print(paste0("y = ", y))
  print(paste0("minA = ", minA))
  print(paste0("maxA = ", maxA))

  print(paste0("Loading ", filename, "_simplified"))
  simp <- sf::st_read(paste0("_temp/", filename, "_simplified.shp"), quiet = TRUE) 
  
  # we use the npts function in the mapview package to calculate the number of vertices 
  # in the original and simplified polygon layers. This could also be returned.
  
  print(paste0("Vertices in original: ", mapview::npts(layers_sf[[filename]])))
  print(paste0("Vertices after simplifying: ", mapview::npts(simp)))
  
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
  
  sf <- from %>%
    mutate(uid = str_sub(get(field), start, end)) %>%
    ms_dissolve(field = "uid", sys = TRUE, sys_mem = 16, quiet = TRUE) %>%
    rename(geosid = uid) 
  
  print(paste0("Joining ", to, " attributes"))
  
  sf <- sf %>%
    # joins attributes from the source file 
    left_join(layers_sf[[to]] %>% st_drop_geometry(), by = "geosid")
  
  print(paste0("Vertices in original: ", mapview::npts(layers_sf[[to]])))
  print(paste0("Vertices after dissolving: ", mapview::npts(sf)))
  
  return(sf)
  
}

# simplify ----

## create a list to put the simplified polygons
polygons_simplified <- NULL

## create a list to put the vertex counts to assess simplification
vertices <- NULL

## simplify all CSD polygons

for (t in c(1951, seq(1981, 2021, 5))) {
  
  q = paste0("csd_", t)
  x = 0.6
  y = 0.98
  
  polygons_simplified[[q]] <- polygon_simplify(filename = q, x = x, y = y)
  
  sf::st_write(polygons_simplified[[q]], 
               dsn = paste0("_simplified/", q, "_simplified.gpkg"), 
               delete_dsn = TRUE, 
               quiet = TRUE)
  
  vertices[[q]] <- list(orig = mapview::npts(layers_sf[[q]]),
                        simp = mapview::npts(polygons_simplified[[q]]),
                        x = x,
                        y = y)
  
}

## simplify all CT polygons

for (t in seq(1951, 2021, 5)) {
  
  q = paste0("ct_", t)
  x = 0.5
  y = 0.6
  
  polygons_simplified[[q]] <- polygon_simplify(filename = q, x = x, y = y)
  
  sf::st_write(polygons_simplified[[q]], 
               dsn = paste0("_simplified/", q, "_simplified.gpkg"), 
               delete_dsn = TRUE,
               quiet = TRUE)
  
  vertices[[q]] <- list(orig = mapview::npts(layers_sf[[q]]),
                        simp = mapview::npts(polygons_simplified[[q]]),
                        x = x,
                        y = y)
  
}

# dissolve ----

## dissolve CSD to CD 1951

t = 1951
fr = paste0("csd_", t)
to = paste0("cd_", t)

polygons_simplified[[to]] <- polygon_dissolve(from = polygons_simplified[[fr]], to = to, field = "geosid", start = 1, end = 5)

sf::st_write(polygons_simplified[[to]], dsn = paste0("_simplified/", to, "_simplified.gpkg"), delete_dsn = TRUE)

vertices[[to]] <- list(orig = mapview::npts(layers_sf[[to]]),
                       simp = mapview::npts(polygons_simplified[[to]])
)

## dissolve CSD to CD 1981-2021

for (t in seq(1981, 2021, 5)) {
  
  fr = paste0("csd_", t)
  to = paste0("cd_", t)
  
  polygons_simplified[[to]] <- polygon_dissolve(from = polygons_simplified[[fr]], to = to, field = "geosid", start = 1, end = 4)
  
  sf::st_write(polygons_simplified[[to]], dsn = paste0("_simplified/", to, "_simplified.gpkg"), delete_dsn = TRUE)
  
  vertices[[to]] <- list(orig = mapview::npts(layers_sf[[to]]),
                        simp = mapview::npts(polygons_simplified[[to]])
                        )
  
}

## dissolve CSD to PR

for (t in c(1951, seq(1981, 2021, 5))) {
  
  fr = paste0("csd_", t)
  to = paste0("pr_", t)
  
  polygons_simplified[[to]] <- polygon_dissolve(from = polygons_simplified[[fr]], to = to, field = "geosid", start = 1, end = 2)
  
  sf::st_write(polygons_simplified[[to]], dsn = paste0("_simplified/", to, "_simplified.gpkg"), delete_dsn = TRUE)
  
  vertices[[to]] <- list(orig = mapview::npts(layers_sf[[to]]),
                         simp = mapview::npts(polygons_simplified[[to]])
  )
  
}

## dissolve CT to CMA

for (t in seq(1951, 2021, 5)) {
  
  fr = paste0("ct_", t)
  to = paste0("cma_", t)
  
  polygons_simplified[[to]] <- polygon_dissolve(from = polygons_simplified[[fr]], to = to, field = "geosid", start = 1, end = 3)
  
  sf::st_write(polygons_simplified[[to]], dsn = paste0("_simplified/", to, "_simplified.gpkg"), delete_dsn = TRUE)
  
  vertices[[to]] <- list(orig = mapview::npts(layers_sf[[to]]),
                         simp = mapview::npts(polygons_simplified[[to]])
  )
}


# save to an R object ----

save(polygons_simplified, file = "polygons_simplified.RData")
save(vertices, file = "polygons_vertices.RData")
rio::export(vertex_summary, "vertex_summary.xlsx")

# javascript ----

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
