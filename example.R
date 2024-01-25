library(eat)
library(lpSolveAPI)
library(readxl)
source("EAT_ByProdcut_model.R")

set.seed(1234)

##########
# READ DATA
##########

labor <- as.data.frame(read_excel("WIOD_JCR_dataset.xls", sheet = "Labor"))
capital <- as.data.frame(read_excel("WIOD_JCR_dataset.xls", sheet = "Capital"))
gross_value_added <- as.data.frame(read_excel("WIOD_JCR_dataset.xls", sheet = "Gross value added"))
co2_emissions <- as.data.frame(read_excel("WIOD_JCR_dataset.xls", sheet = "CO2 emissions"))
emission_relevant_energy_use <- as.data.frame(read_excel("WIOD_JCR_dataset.xls", sheet = "Emission relevant energy use"))

year_str <- as.character(2000) # year 2000
data <- data.frame(labor = labor[,year_str],  # good input
                   capital = capital[,year_str], # good input
                   emission_relevant_energy_use = emission_relevant_energy_use[,year_str], # bad input
                   gross_value_added = gross_value_added[,year_str], # good output
                   co2_emissions = co2_emissions[,year_str]) # bad output

##########
# CALCULATE DFF SCORES
##########

# Set exogenous coefficients
d1 <- d2 <- 0.5

# Get all scores
score <- EATByProduct(data, q = c(1,2), p = 3, y = 4, z = 5, d1, d2, numStop = 5)
row.names(score) <- capital[,"Code"]

score
