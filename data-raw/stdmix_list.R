## code to prepare `istd` dataset goes here

stdmix_files <- list.files("tf-data", pattern = "\\.csv$", full.names = TRUE)
stdmix_files_names <- tools::file_path_sans_ext(basename(stdmix_files))
stdmix_list <- setNames(lapply(stdmix_files, readr::read_csv), stdmix_files_names)
usethis::use_data(stdmix_files_names, overwrite = TRUE)
usethis::use_data(stdmix_list, overwrite = TRUE)
