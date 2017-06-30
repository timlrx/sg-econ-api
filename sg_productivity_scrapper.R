library(httr)
library(reshape2)
library(dplyr)

url  <- "http://www.tablebuilder.singstat.gov.sg/publicfacing/api/json/title/13441.json"

raw.result <- GET(url=url)
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
data <- rbind(data, data.sector)
  
# Calculate YoY growth
productivity <- data %>%
    mutate(year = as.numeric(substr(period, 1, 4)), quarter = substr(period, 6, 7)) %>%  
    arrange(year, quarter) %>%
    group_by(industry, quarter) %>%
    mutate(yoy = (value/lag(value)-1)*100) %>%
    ungroup()

productivity <- productivity %>% rename(productivity = value)
# Combine va data from both series
productivity$industry.short <- productivity$industry
### Compared to VA data there is no other good industries
productivity$industry.short <- plyr::revalue(productivity$industry, 
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

save(productivity, file=paste("data/productivity/",date,"-productivity.RData",sep=""))
