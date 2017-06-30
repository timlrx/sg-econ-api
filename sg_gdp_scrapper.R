library(httr)
library(reshape2)
library(dplyr)

url_va_sa  <- "http://www.tablebuilder.singstat.gov.sg/publicfacing/api/json/title/12411.json"
url_va  <- "http://www.tablebuilder.singstat.gov.sg/publicfacing/api/json/title/12407.json"

raw.result <- GET(url=url_va_sa)
names(raw.result)
date <- gsub("(.+) (.+)", "\\1", raw.result$date)
raw.content <- rawToChar(raw.result$content) # Convert raw bytes to string
raw.content <- content(raw.result)
# levels correspond to the statistics level of aggregation
# 1 - overall economy, 2 - goods / services, 3 - sector level
names(raw.content)

# Converts the list of list into a dataframe
data <- do.call(what = "rbind", args = lapply(raw.content$Level1, as.data.frame))
data.sector <- do.call(what = "rbind", args = lapply(raw.content$Level3, as.data.frame))
data$value <- as.numeric(as.character(data$value))
data.sector$value <- as.numeric(as.character(data.sector$value))
data <- data[,c("quarter","value")]
data.sector <- data.sector[,c("quarter","value","level_3")]
names(data)[names(data) == 'quarter'] <- 'period'
names(data.sector)[names(data.sector) == 'quarter'] <- 'period'
names(data.sector)[names(data.sector) == 'level_3'] <- 'industry'
data$industry <- as.factor("Overall Economy")

#calculate qoq SA annualised growth rate (qoqsaa)
data <- rbind(data, data.sector) %>%
        group_by(industry) %>%
        mutate(qoqsaa = ((value/lag(value))^4-1)*100) %>%
        rename(va.sa = value) %>%
        ungroup()

# Non-seasonally adjusted series for YoY calculations
raw.result <- GET(url=url_va)
names(raw.result)
raw.content <- rawToChar(raw.result$content) # Convert raw bytes to string
raw.content <- content(raw.result)
names(raw.content)
# levels correspond to the statistics level of aggregation
# 1 - overall economy, 2 - goods / services, 3 - sector level

# Converts the list of list into a dataframe
va <- do.call(what = "rbind", args = lapply(raw.content$Level1, as.data.frame))
va.sector <- do.call(what = "rbind", args = lapply(raw.content$Level3, as.data.frame))
va$value <- as.numeric(as.character(va$value))
va.sector$value <- as.numeric(as.character(va.sector$value))
va <- va[,c("quarter","value")]
va.sector <- va.sector[,c("quarter","value","level_3")]
names(va)[names(va) == 'quarter'] <- 'period'
names(va.sector)[names(va.sector) == 'quarter'] <- 'period'
names(va.sector)[names(va.sector) == 'level_3'] <- 'industry'
va$industry <- as.factor("Overall Economy")

#calculate yoy growth rate
va <- rbind(va, va.sector) %>%
      mutate(year = as.numeric(substr(period, 1, 4)), quarter = substr(period, 6, 7)) %>%  
      group_by(industry, quarter) %>%
      mutate(yoy = (value/lag(value)-1)*100) %>%
      rename(va = value) %>%
      ungroup()

# Combine va data from both series
va.sector.comb <- inner_join(data, va, by=c("industry","period"))
va.sector.comb <- arrange(va.sector.comb, year, quarter)
va.sector.comb$industry.short <- va.sector.comb$industry
va.sector.comb$industry.short <- plyr::revalue(va.sector.comb$industry, 
                                          c("Overall Economy"="Overall",
                                         "Manufacturing"="Mfg",
                                         "Other Goods Industries"="Other Goods",
                                         "Wholesale & Retail Trade"="Wholesale Retail",
                                         "Transportation & Storage"="Tpt & Storage",
                                         "Accommodation & Food Services"="Acc & Food",
                                         "Finance & Insurance"="F&I",
                                         "Information & Communications"="Infocomm",
                                         "Business Services"="Business Ser",
                                         "Other Services Industries"="Other Ser"))

save(va.sector.comb, file=paste("data/gdp/",date,"-gdp.RData",sep=""))
