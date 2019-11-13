# download the library if they are not installed
require(devtools)
devtools::install_github("tidymodels/tidymodels", quiet = TRUE)
devtools::install_github('thomasp85/tidygraph', quiet = TRUE)
devtools::install_cran("ggraph", quiet = TRUE)
devtools::install_cran("tidytext", quiet = TRUE)
devtools::install_github("michaeldorman/mapsapi", quiet = TRUE)
devtools::install_github("michaeldorman/mapsapi", quiet = TRUE)
#devtools::install_github("r-spatial/sf")



# load library
library(tidyverse)
library(viridis)
library(ggridges)
library(patchwork)
library(rvest)
library(tidygraph)
library(ggraph)
library(tidytext)

