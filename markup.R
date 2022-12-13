library(magick)
library(MetBrewer)
library(colorspace)
library(ggplot2)
library(glue)
library(stringr)

img <- image_read("images/final_plot.png")

colors <- met.brewer("OKeeffe2")
swatchplot(colors)

text_color <- darken(colors[7], .25)
swatchplot(text_color)

annot <- glue("This map shows population density of Florida. ",
              "Population estimates are bucketed into 400 meter (about 1/4 mile) ",
              "hexagons.") |> 
  str_wrap(45)

img |> 
  image_crop(gravity = "center",
             geometry = "6000x3500+0-150") |> 
  image_annotate("Florida Population Density",
                 gravity = "northwest",
                 location = "+200+100",
                 color = text_color,
                 size = 200,
                 weight = 700,
                 font = "El Messiri") |> 
  image_annotate(annot,
                  gravity = "west",
                  location = "+600+500",
                  color = text_color,
                  size = 125,
                  font = "El Messiri") |> 
  image_annotate(glue("Graphic by Spencer Schien (@MrPecners) | ",
                      "Data: Kontur Population (Released 2022-06-30)"),
                 gravity = "south",
                 location = "+0+100",
                 font = "El Messiri",
                 color = alpha(text_color, .5),
                 size = 70) |> 
  image_write("images/titled_final_plot.png")
