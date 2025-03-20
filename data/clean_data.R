# Data acquisition and transformation
## Dataset: Online Retail II - A real online retail transaction data set of two years.

# import data
data <- rio::import("data/online_retail_II.xlsx") |>
    tibble::tibble()

# clean data
## visualize columns names
colnames(data)
data2 <- data
colnames(data2)[7] <- gsub(" ","",colnames(data2)[7])
colnames(data2)

## visualize columns with missing data
colSums(is.na(data2))

## cleaning
retail_data <- data2 |>
    dplyr::filter(!is.na(CustomerID)) |>  # Remove missing customers
    dplyr::filter(Quantity > 0) |>  # Remove negative movements
    dplyr::filter(Price > 0) |>  # Remove negative prices
    dplyr::mutate(Revenue = Quantity * Price)  # Calculate revenue

## streamline timestamps
retail_data$InvoiceDate <- lubridate::as_date(retail_data$InvoiceDate)

# write data to file
readr::write_csv(
    retail_data,
    "data/cleaned_data.csv")