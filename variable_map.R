library(data.table)
library(stringr)

raw_data <- readxl::read_excel(path = "input_data/TS_RawData.xlsx", col_names = TRUE, skip = 2, sheet = "Sheet1")

raw_data[is.na(raw_data)] <- 0

# Extract index from the colnames. We will need to transpose data to make data columns into index
#data_index = as.POSIXct.Date(as.numeric(colnames(raw_data[, 4:ncol(raw_data)])), origin = "1899-12-30")

sales <- pivot_longer(raw_data, cols = starts_with("4"),
                      names_to = "Date",
                      values_to = "Sales",
                      values_drop_na = FALSE)


map_area = setNames(unique(sales$Area), unique(sales$Area))

map_country = setNames(unique(sales$Country), str_to_title(unique(sales$Country)))

map_part = setNames(unique(sales$Part), unique(sales$Part))
