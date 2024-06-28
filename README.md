
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cpickr

<!-- badges: start -->
<!-- badges: end -->

The goal of cpickr is to create input files for the DWR-GeM lab Hamilton
Nimbus robots to use as input for cherrypicking samples from 96-well
plates.

## Installation

You can install the development version of cpickr like so:

``` r
devtools::install_github("DWR-CSI/cpickr")
```

### Dependencies

This package depends on the following packages:

- dplyr
- magrittr
- readr
- stats
- tibble

## Functions

Currently, the package contains the following functions:

- `subsample_plate_data()`: Subsample a plate dataset to include only
  the specified SampleIDs.
- `write_hamilton_csvs()`: Write a text-formatted platemap to multiple
  CSV files for Hamilton robot input.
- `result_from()`: Take a Hamilton input file and return the resulting
  destination plate layout.

## Example

Example usage will be placed here. Work in progress…

``` r
library(cpickr)
library(readr)
## basic example code

subsampled_plate_data <- tibble::tibble(
   SampleID = c("C234323CVP", "C240026CVP", "C240036CVP", "C240199CVP"),
   PlateID = c("CVP20240204CDFW01", "CVP20240204CDFW01", "CVP20240204CDFW01", "CVP20240218CDFW02"),
   WellID = c("A1", "A2", "A3","D2")
)

write_hamilton_csvs(
  subsampled_plate_data, # Plate data showing the Sample names, Source plates, and Source wells
  rows_per_file = 96, # Maximum number of rows per file. Cannot be more than 96.
  max_plate_ids = 20, # Maximum number of unique plate IDs per file. Cannot be more than 20.
  file_prefix = "test_", 
  auto_dest_well = TRUE # Automatically fill in the destination well sequentially from A1-A8, B1-B8, etc. Leave blank to fill in manually afterwards.
  )

# Next example: subsampling source plate data
sample_ids_to_keep <- c("C234323CVP", "C240036CVP")
# Below we subsample the already subsampled plate data from above.
sub_subsampled_plate_data <- subsample_plate_data(sub_subsampled_plate_data, sample_ids_to_keep, file_prefix = "sub_subsampled_")
write_hamilton_csvs(sub_subsampled_plate_data)

# To see what the resulting destination plate would look like, use the result_from function on a Hamilton input .txt file.
test_input_file <- readr::read_csv("sub_subsampled_1.txt")
result_plate <- result_from(test_input_file)
result_plate
```

A large example of a full plate map input file (before
subsetting/subsampling) can be found in the `data/raw` folder. This file
is called `example_plate_masterlist.csv`. This is preloaded into the
package and can be accessed as `example_plate_data`.
