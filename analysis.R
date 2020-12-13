library(readr)
library(dplyr)
library(sqldf)
library(ggplot2)
library(reshape2)


extractYear <- function(data){
  cleanData <- data
  cleanData$Closing_Date <- as.character(cleanData$Closing_Date)
  cleanData$Year <- substr(cleanData$Closing_Date, nchar(cleanData$Closing_Date) - 3, nchar(cleanData$Closing_Date))
  return(cleanData)
}

##When we only consider the last 11 years, and only venture capital firms, the sample is composed of about 40000 units.


groupByInvestorCountry <- function(data){
  cleanData <- extractYear(data)
  return(byCountry <- sqldf::sqldf("SELECT FirmCountry, 
                                    AVG(Amount) AS 'Average_Amount', 
                                   COUNT(FirmCountry) 'N_Investments' 
                                   FROM cleanData 
                                   WHERE FirmType = 'Venture Capital'
                                   AND Year >= 2009
                                   GROUP BY FirmCountry
                                   ORDER BY N_Investments DESC"))
}

groupByInvesteeCountry <- function(data){
  cleanData <- extractYear(data)
  return(byCountry <- sqldf::sqldf("SELECT Country, 
                                    AVG(Amount) AS 'Average_Amount', 
                                   COUNT(FirmCountry) 'N_Investments' 
                                   FROM cleanData 
                                   WHERE FirmType = 'Venture Capital'
                                   AND Year >= 2009
                                   GROUP BY Country 
                                   ORDER BY N_Investments DESC"))
}

groupBySegment <- function(data){
  cleanData <- extractYear(data)
  return(byCountry <- sqldf::sqldf("SELECT IndustrySegment, 
                                    AVG(Amount) AS 'Average_Amount', 
                                   COUNT(IndustrySegment) 'N_Investments' 
                                   FROM cleanData 
                                   WHERE FirmType = 'Venture Capital'
                                   AND Year >= 2009
                                   GROUP BY IndustrySegment
                                   ORDER BY N_Investments DESC"))
  
}

groupByYear <- function(data){
  cleanData <- extractYear(data)
  return(byYear <- sqldf::sqldf("SELECT Year AS Year,
                    AVG(Amount) AS 'Average_Amount'
                    FROM cleanData
                    WHERE FirmType = 'Venture Capital'
                    GROUP BY Year
                    ORDER BY Year DESC"))
}

removeWhiteSpaceFromColumnNames <- function(data){
  names(data) <- gsub(" ", "_", names(data), fixed = TRUE)
}

data <- readr::read_csv("investors.csv")
dataWithGroupedEu <- readr::read_csv("investors_grouped_eu.csv")
countryData <- readr::read_csv("gdp_investments_population_clean.csv")

#We Group the data by country of the investor. The quantities we take into exam are the average amount invested and
#the number of operations carried out.

byInvestorCountry <- groupByInvestorCountry(data)


#First Plot: Average Amount by investor country
byInvestorCountryTopTwenty <- byInvestorCountry[1:20,]
toPlot <- byInvestorCountryTopTwenty[order(byInvestorCountryTopTwenty$Average_Amount),]
toPlot$FirmCountry[toPlot$FirmCountry == "United Kingdom"] <- "UK"
toPlot$FirmCountry[toPlot$FirmCountry == "United States"] <- "US"
toPlot$FirmCountry[toPlot$FirmCountry == "Netherlands"] <- "NL"
toPlot$FirmCountry <- factor(toPlot$FirmCountry, levels = toPlot$FirmCountry)
byInvestorCountryAmountPlot <- ggplot(toPlot, aes(x=FirmCountry, y=Average_Amount/1000000, fill= FirmCountry))
byInvestorCountryAmountPlot <- byInvestorCountryAmountPlot + geom_bar(stat="identity")
byInvestorCountryAmountPlot <- byInvestorCountryAmountPlot + labs(title = "Average Invested Amount By Country (last 10 years)")
byInvestorCountryAmountPlot <- byInvestorCountryAmountPlot + theme(axis.text = element_text(size =6))
byInvestorCountryAmountPlot <- byInvestorCountryAmountPlot + ylab("Average Amount Invested, in millions of USD")

#Second Plot: Number of Investments by Investor Country

byInvestorCountryTopTwenty <- byInvestorCountry[1:20,]
toPlot <- byInvestorCountryTopTwenty[order(byInvestorCountryTopTwenty$N_Investments),]
names(countryData)[1] <- "FirmCountry"
countryData$GDP <- as.numeric(countryData$GDP)
toPlot <- merge(toPlot, countryData, by = "FirmCountry", all.y = FALSE)
toPlot$FirmCountry[toPlot$FirmCountry == "United Kingdom"] <- "UK"
toPlot$FirmCountry[toPlot$FirmCountry == "United States"] <- "US"
toPlot$FirmCountry[toPlot$FirmCountry == "Netherlands"] <- "NL"
toPlot <- toPlot[order(toPlot$N_Investments/toPlot$GDP),]
toPlot$FirmCountry <- factor(toPlot$FirmCountry, levels = toPlot$FirmCountry)
byInvestorCountryNumberPlot <- ggplot(toPlot, aes(x=FirmCountry, y=N_Investments/GDP, fill= FirmCountry))
byInvestorCountryNumberPlot <- byInvestorCountryNumberPlot + geom_bar(stat="identity")
byInvestorCountryNumberPlot <- byInvestorCountryNumberPlot + labs(title = "Number Of Investments By Country (last 10 years)")
byInvestorCountryNumberPlot <- byInvestorCountryNumberPlot + theme(axis.text = element_text(size =6))
byInvestorCountryNumberPlot <- byInvestorCountryNumberPlot + ylab("Number Of Investments")
toPlot <- dplyr::filter(toPlot, FirmCountry != "Israel")
investorTechInvestmentsCorrelation <- ggplot(toPlot, aes(x = TechInvestments/Population, y = N_Investments/GDP)) + geom_point()
lrData <- data.frame("X" = toPlot$TechInvestments/toPlot$Population, "Y" = toPlot$N_Investments/toPlot$GDP)
#Regression model for investor countries. Israel was excluded because it's an outlier.
investorModel <- lm(Y ~ X, data = lrData)

byInvesteeCountry <- groupByInvesteeCountry(data)


#Third Plot : Average Amount by Investee Country
byInvesteeCountryTopTwenty <- byInvesteeCountry[1:20,]
toPlot <- byInvesteeCountryTopTwenty[order(byInvesteeCountryTopTwenty$Average_Amount),]
toPlot$Country[toPlot$Country == "United Kingdom"] <- "UK"
toPlot$Country[toPlot$Country == "United States"] <- "US"
toPlot$Country[toPlot$Country == "Netherlands"] <- "NL"
toPlot$Country <- factor(toPlot$Country, levels = toPlot$Country)
byInvesteeCountryAmountPlot <- ggplot(toPlot, aes(x=Country, y=Average_Amount/1000000, fill=Country ))
byInvesteeCountryAmountPlot <- byInvesteeCountryAmountPlot + geom_bar(stat="identity")
byInvesteeCountryAmountPlot <- byInvesteeCountryAmountPlot + labs(title = "Average Invested Amount By Country (last 10 years)")
byInvesteeCountryAmountPlot <- byInvesteeCountryAmountPlot + theme(axis.text = element_text(size =6))
byInvesteeCountryAmountPlot <- byInvesteeCountryAmountPlot + ylab("Average Amount Invested, in millions of USD")

#Fourth Plot: Number of Investments by Investee Country

byInvesteeCountryTopTwenty <- byInvesteeCountry[1:20,]
toPlot <- byInvesteeCountryTopTwenty[order(byInvesteeCountryTopTwenty$N_Investments),]
names(countryData)[1] <- "Country"
countryData$GDP <- as.numeric(countryData$GDP)
toPlot <- merge(toPlot, countryData, by = "Country", all.y = FALSE)
toPlot$Country[toPlot$Country == "United Kingdom"] <- "UK"
toPlot$Country[toPlot$Country == "United States"] <- "US"
toPlot$Country[toPlot$Country == "Netherlands"] <- "NL"
toPlot <- toPlot[order(toPlot$N_Investments/toPlot$GDP),]
toPlot$Country <- factor(toPlot$Country, levels = toPlot$Country)
byInvesteeCountryNumberPlot <- ggplot(toPlot, aes(x=Country, y=N_Investments/GDP, fill=Country ))
byInvesteeCountryNumberPlot <- byInvesteeCountryNumberPlot + geom_bar(stat="identity")
byInvesteeCountryNumberPlot <- byInvesteeCountryNumberPlot + labs(title = "Number of Investments By Country (last 10 years)")
byInvesteeCountryNumberPlot <- byInvesteeCountryNumberPlot + theme(axis.text = element_text(size =6))
byInvesteeCountryNumberPlot <- byInvesteeCountryNumberPlot + ylab("Number of Investments")
toPlot <- dplyr::filter(toPlot, Country != "Israel")
investeeTechInvestmentsCorrelation <- ggplot(toPlot, aes(x = TechInvestments/Population, y = N_Investments/GDP)) + geom_point()
lrData <- data.frame("X" = toPlot$TechInvestments/toPlot$Population, "Y" = toPlot$N_Investments/toPlot$GDP)

#Regression model for investee country. Israel was excluded because it's an outlier.
investeeModel <- lm(Y ~ X, data = lrData)


bySegment <- groupBySegment(data)

byInvesteeSegmentTopTwenty <- bySegment[1:20,]
toPlot <- byInvesteeSegmentTopTwenty[order(byInvesteeSegmentTopTwenty$Average_Amount),]
toPlot$IndustrySegment <- factor(toPlot$IndustrySegment, levels = toPlot$IndustrySegment)
byInvesteeSegmentAmountPlot <- ggplot(toPlot, aes(x=IndustrySegment, y=Average_Amount/1000000, fill= IndustrySegment))
byInvesteeSegmentAmountPlot <- byInvesteeSegmentAmountPlot + geom_bar(stat="identity")
byInvesteeSegmentAmountPlot <- byInvesteeSegmentAmountPlot + labs(title = "Average Invested Amount By Country (last 10 years)")
byInvesteeSegmentAmountPlot <- byInvesteeSegmentAmountPlot + theme(axis.text = element_text(size =6))
byInvesteeSegmentAmountPlot <- byInvesteeSegmentAmountPlot + ylab("Average Amount Invested, in millions of USD")

byInvesteeSegmentTopTwenty <- bySegment[1:20,]
toPlot <- byInvesteeSegmentTopTwenty[order(byInvesteeSegmentTopTwenty$N_Investments),]
toPlot$IndustrySegment <- factor(toPlot$IndustrySegment, levels = toPlot$IndustrySegment)
byInvesteeSegmentNumberPlot <- ggplot(toPlot, aes(x=IndustrySegment, y=N_Investments, fill= IndustrySegment))
byInvesteeSegmentNumberPlot <- byInvesteeSegmentNumberPlot + geom_bar(stat="identity")
byInvesteeSegmentNumberPlot <- byInvesteeSegmentNumberPlot + labs(title = "Average Invested Amount By Country (last 10 years)")
byInvesteeSegmentNumberPlot <- byInvesteeSegmentNumberPlot + theme(axis.text = element_text(size =6))
byInvesteeSegmentNumberPlot <- byInvesteeSegmentNumberPlot + ylab("Average Amount Invested, in millions of USD")



#This is just the Average Amount by year
byYear <- groupByYear(data)

byYearPlot <- ggplot(byYear, aes(x = Year, y = Average_Amount/1000000, group = 1)) 
byYearPlot <- byYearPlot + geom_line(color = "blue")
byYearPlot <- byYearPlot + labs(title = "Global Average Investments By Year")
byYearPlot <- byYearPlot + theme(axis.text = element_text(size =6))
byYearPlot <- byYearPlot + ylab("Average Amount Invested, in millions of USD")
ggsave("byYear.pdf", byYearPlot)




