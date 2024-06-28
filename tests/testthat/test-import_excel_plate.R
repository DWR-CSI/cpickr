# Helper function to create test Excel files
create_test_excel <- function(data, file) {
  df <- as.data.frame(data)
  writexl::write_xlsx(df, file)
  return(file)
}

test_that("import_excel_plate handles standard input correctly", {
  temp_file <- tempfile(fileext = ".xlsx")
  test_data <- as.data.frame(matrix(paste0("Sample", 1:96), nrow = 8, ncol = 12))
  create_test_excel(test_data, temp_file)

  result <- import_excel_plate(temp_file, sheet = 1, start_cell = "A2")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 96)
  expect_equal(ncol(result), 3)
  expect_equal(names(result), c("SampleID", "PlateID", "WellID"))
  expect_true(all(result$WellID == paste0(rep(LETTERS[1:8], each = 12), rep(1:12, 8))))
  expect_true(all(result$SampleID[1] == "Sample1", result$SampleID[10] == "Sample73", result$SampleID[96] == "Sample96"))
  expect_true(all(result$PlateID == tools::file_path_sans_ext(basename(temp_file))))

  unlink(temp_file)
})

test_that("import_excel_plate handles custom plate_id correctly", {
  temp_file <- tempfile(fileext = ".xlsx")
  test_data <- as.data.frame(matrix(paste0("Sample", 1:96), nrow = 8, ncol = 12))
  create_test_excel(test_data, temp_file)

  result <- import_excel_plate(temp_file, sheet = 1, start_cell = "B1", plate_id = "TestPlate")

  expect_true(all(result$PlateID == "TestPlate"))

  unlink(temp_file)
})

test_that("import_excel_plate handles empty wells correctly", {
  temp_file <- tempfile(fileext = ".xlsx")
  test_data <- as.data.frame(matrix(c(paste0("Sample", 1:90), rep(NA, 6)), nrow = 8, ncol = 12))
  create_test_excel(test_data, temp_file)

  result <- import_excel_plate(temp_file, sheet = 1, start_cell = "A2")

  expect_equal(nrow(result), 96)  # Still 96 wells total
  expect_true(any(is.na(result$SampleID)))  # Some SampleIDs should be NA
  expect_equal(sum(is.na(result$SampleID)), 6)  # 6 empty wells

  unlink(temp_file)
})

test_that("import_excel_plate handles non-standard start cells", {
  temp_file <- tempfile(fileext = ".xlsx")
  test_data <- as.data.frame(matrix(paste0("Sample", 1:96), nrow = 8, ncol = 12))
  full_data <- as.data.frame(matrix("", nrow = 10, ncol = 14))
  full_data[3:10, 3:14] <- test_data
  create_test_excel(full_data, temp_file)

  result <- import_excel_plate(temp_file, sheet = 1, start_cell = "C4")

  expect_equal(nrow(result), 96)
  expect_true(all(result$SampleID[1] == "Sample1", result$SampleID[10] == "Sample73", result$SampleID[96] == "Sample96"))

  unlink(temp_file)
})

test_that("import_excel_plate handles multiple sheets correctly", {
  temp_file <- tempfile(fileext = ".xlsx")
  test_data1 <- as.data.frame(matrix(paste0("Sample1_", 1:96), nrow = 8, ncol = 12))
  test_data2 <- as.data.frame(matrix(paste0("Sample2_", 1:96), nrow = 8, ncol = 12))
  writexl::write_xlsx(list(Sheet1 = test_data1, Sheet2 = test_data2), temp_file)

  result1 <- import_excel_plate(temp_file, sheet = 1, start_cell = "A2")
  result2 <- import_excel_plate(temp_file, sheet = 2, start_cell = "A2")

  expect_true(all(result1$SampleID[1] == "Sample1_1", result1$SampleID[10] == "Sample1_73", result1$SampleID[96] == "Sample1_96"))
  expect_true(all(result2$SampleID[1] == "Sample2_1", result2$SampleID[10] == "Sample2_73", result2$SampleID[96] == "Sample2_96"))

  unlink(temp_file)
})

