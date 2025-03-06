library(shiny)
library(DT)
library(shinyjs)
library(bslib)
library(shinyWidgets)  # 有可能不用这个吗
library(dplyr)
library(tibble)
library(purrr)
library(forcats)
library(stringr)
library(tidyr)
library(readr)
library(readxl)
library(openxlsx)  # 不想用这个
library(ggplot2)
library(ggrepel)
# library(ComplexHeatmap)
library(plotly)
library(enviPat)
data(adducts)
# 扩展adducts
adducts_H2O_H <- list("M-H2O+H", NA, 1, 1, NA, "positive", "H1", "H2O1", 1)
adducts <- rbind(adducts, adducts_H2O_H)
data(isotopes)
library(markdown)
library(broom)  # tidy()
options(shiny.maxRequestSize = 300 * 1024^2)

# source("helpers.R")

# source("create_sidebar_menu_header.R")
# source("create_sidebar_link.R")

# source("data-science/aggregate/module-aggr.R")

# source("data-science/statistics/module-stat.R")
# stat_modules_files <- list.files("data-science/statistics/modules/", pattern = "\\.R$", full.names = TRUE)
# lapply(stat_modules_files, source)
# stat_modules <- tools::file_path_sans_ext(basename(stat_modules_files))

# source("data-science/visualization/module-vs.R")
# helpers_script <- list.files("data-science/visualization/helpers/", pattern = "\\.R$", full.names = TRUE)
# lapply(helpers_script, source)
# vs_modules_files <- list.files("data-science/visualization/modules/", pattern = "\\.R$", full.names = TRUE)
# lapply(vs_modules_files, source)
# vs_modules <- tools::file_path_sans_ext(basename(vs_modules_files))

# source("omics/data-preprocessing/module-pp.R")

# enviPat_modules_files <- list.files("mass-tools/enviPat/modules/", pattern = "\\.R$", full.names = TRUE)
# lapply(enviPat_modules_files, source)
# enviPat_modules <- tools::file_path_sans_ext(basename(enviPat_modules_files))

# source("mass-tools/tf/module-tf.R")

# source("toolkits/md5check/module-md5check.R")