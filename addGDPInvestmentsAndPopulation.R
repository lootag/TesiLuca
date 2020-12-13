library(readr)
library(dplyr)
library(sqldf)

gdpData <- readr::read_csv("gdp_and_tech_investments.csv")
names(gdpData)[7] <- "GDP"
names(gdpData)[5] <- "TechInvestments"
gdpDataClean <- sqldf::sqldf("SELECT Country, 
                             AVG(Population2016) AS Population, 
                             AVG(GDP) AS GDP, 
                             AVG(TechInvestments) AS TechInvestments 
                             FROM gdpData GROUP BY Country")
readr::write_csv(gdpDataClean, "gdp_investments_population_clean.csv")