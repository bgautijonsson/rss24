library(bggjphd)
library(tidyverse)

d <- stations |> 
  stations_to_sf() |> 
  points_to_grid()


spatial_plot(d, variable = station)
