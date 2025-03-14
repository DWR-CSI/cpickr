
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cpickr

<!-- badges: start -->
<!-- badges: end -->

The goal of cpickr is to create and manipulate input files for the
DWR-GeM lab Hamilton Nimbus robots to use as input for cherrypicking
samples from 96-well and 384-well plates.

## Installation

You can install the development version of cpickr from GitHub like so:

``` r
remotes::install_github("DWR-CSI/cpickr")
```

If you don't have the "remotes" package installed, you will first need to install it like so:

```r
install.packages("remotes")
```

### Dependencies

This package depends on the following packages:

- dplyr
- magrittr
- readr
- stats
- tibble
- readxl
- tidyr
- cellranger
- writexl

## Functions

Currently, the package contains the following functions:

- `subsample_plate_data()`: Subsample a plate dataset to include only
  the specified SampleIDs.
- `write_hamilton_csvs()`: Write a text-formatted platemap to multiple
  CSV files for Hamilton robot input.
- `result_from()`: Take a Hamilton input file and return the resulting
  destination plate layout.
- `import_excel_plate()`: Import 96-well plate data in a rectangular
  format from Excel.
- `convert_to_excel_layout()`: Convert plate data to an Excel
  spreadsheet with a 96-well or 384-well layout. No other function
  supports 384-well plates at the moment.

## Example

### Cherrypicking file generation

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
  max_plate_ids = 24, # Maximum number of unique plate IDs per file. Cannot be more than 24.
  file_prefix = "test_", 
  auto_dest_well = TRUE # Automatically fill in the destination well sequentially by column, A1, B1, C1..A2, B2, C2... etc. Leave blank to fill in manually afterwards.
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

### Importing and Exporting plate data from and to Excel.

The `convert_to_excel_layout()` function takes plate data (with columns
SampleID, PlateID, and WellID) and creates an Excel file that visually
represents the plate layout. It supports both 96-well and 384-well plate
formats. This function is particularly useful for visualizing the plate
layout or for creating a template for manual data entry.

``` r
# Import plate data from Excel
plate_data <- import_excel_plate("path/to/input_plate.xlsx", sheet = 1, start_cell = "A1")

# Convert the imported data to an Excel file with plate layout
convert_to_excel_layout(plate_data, "output_plate_layout.xlsx", n_wells = 96)
```

### Example Data

An example of a full plate map input file (before
subsetting/subsampling) can be found in the `data/raw` folder. This file
is called `example_plate_masterlist.csv`. It is preloaded into the
package and can be accessed as `example_plate_data`.
