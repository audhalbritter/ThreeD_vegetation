#### LOAD PACKAGES ####

library(tidyverse)
library(stringi)
library(readxl)
library(writexl)
#devtools::install_github("Between-the-Fjords/dataDownloader")
library(dataDownloader)
library(usethis)
#devtools::install_github("richardjtelford/rjt.misc")
#library(rjt.misc)
library(broom)
library(vegan)
library(ggvegan)
library(performance)
library(targets)
library(DBI)
library(RSQLite)
library(patchwork)
library(glue)
library(wesanderson)
library(tidySEM)
library(piecewiseSEM)
library(gt)
library(ggpubr)
library(here)
library(data.table)


# Stuff
pn <- . %>% print(n = Inf)


# Colours
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
