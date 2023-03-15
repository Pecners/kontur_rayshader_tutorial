library(sf)
library(tigris)
library(tidyverse)
library(stars)
library(rayshader)
library(MetBrewer)
library(colorspace)

# load kontur data

data <- st_read("data/kontur_population_US_20220630.gpkg")

# load states 

st <- states()

# filter for florida

florida <- st |> 
  filter(NAME == "Florida") |> 
  st_transform(crs = st_crs(data))

# check with map

florida |> 
  ggplot() +
  geom_sf()

# do intersection on data to limit kontur to florida

st_florida <- st_intersection(data, florida)

# define aspect ratio based on bounding box

bb <- st_bbox(st_florida)

bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) |> 
  st_sfc(crs = st_crs(data))

bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) |> 
  st_sfc(crs = st_crs(data))

# check by plotting points

florida |> 
  ggplot() +
  geom_sf() +
  geom_sf(data = bottom_left) +
  geom_sf(data = bottom_right, color = "red")

width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) |> 
  st_sfc(crs = st_crs(data))

height <- st_distance(bottom_left, top_left)

# handle conditions of width or height being the longer side

if (width > height) {
  w_ratio <- 1
  h_ratio <- height / width
} else {
  h_ratio <- 1
  w_ratio <- width / height
}

# convert to raster so we can then convert to matrix

size <- 5000

florida_rast <- st_rasterize(st_florida, 
                             nx = floor(size * w_ratio),
                             ny = floor(size * h_ratio))

mat <- matrix(florida_rast$population, 
              nrow = floor(size * w_ratio),
              ncol = floor(size * h_ratio))

# create color palette

c1 <- met.brewer("OKeeffe2")
swatchplot(c1)

texture <- grDevices::colorRampPalette(c1, bias = 2)(256)
swatchplot(texture)

# plot that 3d thing!

rgl::rgl.close()

mat |> 
  height_shade(texture = texture) |> 
  plot_3d(heightmap = mat,
          zscale = 100 / 5,
          solid = FALSE,
          shadowdepth = 0)
  
render_camera(theta = -20, phi = 45, zoom = .8)

outfile <- "images/final_plot.png"

{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  if (!file.exists(outfile)) {
    png::writePNG(matrix(1), target = outfile)
  }
  render_highquality(
    filename = outfile,
    interactive = FALSE,
    lightdirection = 280,
    lightaltitude = c(20, 80),
    lightcolor = c(c1[2], "white"),
    lightintensity = c(600, 100),
    samples = 450,
    width = 6000,
    height = 6000
  )
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}

  
  
