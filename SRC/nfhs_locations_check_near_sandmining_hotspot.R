setwd("/home/sayan/SANDMINING/")
# Load haven package
library(haven)
library(vroom)
library(sf)
# Read the Stata .dta file
nfhs_data <- read_dta("./DATA/IAHR7EFL.DTA",n_max = 5)
var_labels <- sapply(nfhs_data, attr, "label")
var_labels_df <- data.frame(Variable = names(var_labels), Description = unname(var_labels))
write.csv(var_labels_df, "./DATA/nfhs_variable_labels.csv", row.names = FALSE)

# View the structure of the dataset
str(nfhs_data)

# Show first few rows
head(nfhs_data)


install.packages("sf", dependencies = TRUE)

