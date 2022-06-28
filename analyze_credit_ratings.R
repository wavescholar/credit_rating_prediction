if(!require(tidyverse)){install.packages( 'tidyverse',dependencies = TRUE) }
if(!require(here)){install.packages( 'here',dependencies = TRUE) }
if(!require(DataExplorer)){install.packages( 'DataExplorer',dependencies = TRUE) }

rm(list = ls())
library(tidyverse)
library(here)
library(DataExplorer)
theme_set(theme_minimal())

rating_data = read_csv(file = here( "data", "corporate_rating.csv"))

#DataExplorer::create_report(rating_data,output_file = "full_data_exporer_report.html")
names <- colnames(rating_data)
# "Rating"                             "Name"                              
# "Symbol"                             "Rating Agency Name"                
# "Date"                               "Sector"                            
# "currentRatio"                       "quickRatio"                        
# "cashRatio"                          "daysOfSalesOutstanding"            
# "netProfitMargin"                    "pretaxProfitMargin"                
# "grossProfitMargin"                  "operatingProfitMargin"             
# "returnOnAssets"                     "returnOnCapitalEmployed"           
# "returnOnEquity"                     "assetTurnover"                     
# "fixedAssetTurnover"                 "debtEquityRatio"                   
# "debtRatio"                          "effectiveTaxRate"                  
# "freeCashFlowOperatingCashFlowRatio" "freeCashFlowPerShare"              
# "cashPerShare"                       "companyEquityMultiplier"           
# "ebitPerRevenue"                     "enterpriseValueMultiple"           
# "operatingCashFlowPerShare"          "operatingCashFlowSalesRatio"       
# "payablesTurnover"                  

#Drop data we won't use in modeling for now
drops <- c("Name","Symbol","Rating Agency Name","Date")
data <-rating_data[ , !(names(rating_data) %in% drops)]

#Create ordinal and numeric response
unique(data$Rating)
ordinal_rating  <- factor(data$Rating, order = TRUE,levels = c("AAA","AA","A","BBB", "BB","B","CCC","CC","C","D"  ))

numeric_rating <-as.numeric(factor(ordinal_rating,levels = c("AAA","AA","A","BBB", "BB","B","CCC","CC","C","D"  )))

data <- cbind(data, ordinal_rating)

data <- cbind(data, numeric_rating)

#DataExplorer::create_report(data,output_file = "model_data_exporer_report.html")

drops <- c("ordinal_rating","Rating")
data <-data[ , !(names(data) %in% drops)]

library(reshape)

meltData <- melt(data)

library(ggplot2)

ggplot(meltData, aes(variable, value)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))

ggplot(meltData, aes(variable, value)) + scale_y_continuous(trans='log10') + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))

to_log10<-as.list(strsplit(colnames(data), " "))

to_log10<- to_log10[to_log10 != "Sector"]
to_log10<- to_log10[to_log10 != "numeric_rating"]

log_offset <- function(x) log10(x+1)
data_cts_logxform <- data.frame( lapply(data[unlist(to_log10)], log_offset)  )
data_cts_logxform<-cbind(data_cts_logxform,numeric_rating)
data_cts_logxform<-cbind(data_cts_logxform,data$Sector)

model.fit <-lm(numeric_rating~ .,data = data_cts_logxform)
summary(model.fit)
plot(model.fit)





