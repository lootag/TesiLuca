library(readr)
library(dplyr)

gdpAndTechInvestments <- readr::read_csv("gdp_and_tech_investments.csv")
populationByCountry <-  readr::read_csv("population_by_country.csv")
population2016 <- dplyr::select(populationByCountry, c("Country", "Year_2016"))
names(population2016)[2] <- "Population2016"


gdpAndTechInvestmentsFull <- merge(gdpAndTechInvestments, population2016, by = "Country", all.y = FALSE)
readr::write_csv(gdpAndTechInvestmentsFull, "gdp_and_tech_investments.csv")