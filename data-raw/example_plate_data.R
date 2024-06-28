## code to prepare `example_plate_data` dataset goes here

example_plate_data <- readr::read_csv("data-raw/example_plate_masterlist.csv")
usethis::use_data(example_plate_data, overwrite = TRUE)
