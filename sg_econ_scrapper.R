library(httr)
library(reshape2)
library(dplyr)
library(ggplot2)
library(plotly)

url_va_sa  <- "http://www.tablebuilder.singstat.gov.sg/publicfacing/api/json/title/12411.json"
url_va  <- "http://www.tablebuilder.singstat.gov.sg/publicfacing/api/json/title/12407.json"

raw.result <- GET(url=url_va_sa)
names(raw.result)
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

#calculate qoq SA annualised growth rate (qoqsaa)
data<-data %>% mutate(qoqsaa = ((value/lag(value))^4-1)*100)
data.sector<-data.sector %>% group_by(industry) %>% 
             mutate(qoqsaa = ((value/lag(value))^4-1)*100) %>% ungroup()


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



#calculate qoq SA annualised growth rate (qoqsaa)
va<-va %>% mutate(year = as.numeric(substr(period, 1, 4)), quarter = substr(period, 6, 7)) %>%  
           group_by(quarter) %>% mutate(yoy = (value/lag(value)-1)*100) %>% ungroup()
va.sector<-va.sector %>% mutate(year = as.numeric(substr(period, 1, 4)), quarter = substr(period, 6, 7)) %>% 
                         group_by(quarter, industry) %>% mutate(yoy = (value/lag(value)-1)*100) %>%
                         ungroup()

# Combine va data from both series
data$industry<-as.factor("Overall Economy")
va$industry<-as.factor("Overall Economy")
va.sector.comb<-merge(rbind(va, va.sector), rbind(data, data.sector), by=c("industry","period"))
names(va.sector.comb)[names(va.sector.comb) == 'value.x'] <- 'va.sa'
names(va.sector.comb)[names(va.sector.comb) == 'value.y'] <- 'va'
va.sector.comb<-arrange(va.sector.comb, year, quarter)
va.sector.comb$industry.short<-va.sector.comb$industry
va.sector.comb$industry.short<-plyr::revalue(va.sector.comb$industry, 
                                          c("Overall Economy"="Overall",
                                         "Manufacturing"="Mfg",
                                         "Other Goods Industries"="Other Goods",
                                         "Wholesale & Retail Trade"="Wholesale Retail",
                                         "Transportation & Storage"="Tpt & Storage",
                                         "Accommodation & Food Services"="Acc & Food",
                                         "Finance & Insurance"="F&I",
                                         "Information & Communications"="Infocomms",
                                         "Business Services"="Business Ser",
                                         "Other Services Industries"="Other Ser"))

### Descriptive Plots

# Plot aggregate trend over the last 4 years
overall.trend<-ggplot(data[(nrow(data)-16+1):nrow(data),], aes(period, qoqsaa, group=1)) +
  geom_point() + geom_line() + ylab("QoQ SA (%)") + xlab("Period") +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))

#Plot sector level qoq growth bar graph
qoq.sector.bar<-ggplot(data.sector[(nrow(data.sector)-11+1):nrow(data.sector),], aes(industry, qoqsaa)) +
  geom_bar(stat="identity") + ylab("QoQ SA (%)") + xlab("Industry") + coord_flip()

# qoq_trend<-ggplot(data.sector[(nrow(data.sector)-11*16+1):nrow(data.sector),], aes(period, qoqsaa, group=level_3, color=level_3)) +
#  geom_point() + geom_line() + ylab("QoQ SA (%)") + theme_classic() + theme(axis.text.x=element_text(angle=90,hjust=1))

# Plot sector level yoy growth bar graph
yoy.sector.bar<-ggplot(va.sector[(nrow(va.sector)-11+1):nrow(va.sector),], aes(industry, yoy)) +
  geom_bar(stat="identity") + ylab("YoY (%)") + xlab("Industry") + coord_flip()

melt.va.sector.comb <- melt(va.sector.comb[(nrow(va.sector.comb)-11):nrow(va.sector.comb),],
                            measure.vars=c("yoy","qoqsaa"))
names(melt.va.sector.comb)[names(melt.va.sector.comb) == 'variable'] <- 'series'
sector.bar<-ggplot(melt.va.sector.comb,aes(industry.short, value)) +
            geom_bar(aes(fill=series), stat="identity", position=position_dodge(width=1)) +
            ylab("Growth (%)") + xlab("Industry") + theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1),
                                                          axis.title.x=element_blank()) +
            theme_classic() + scale_x_discrete(limits = rev(levels(melt.va.sector.comb$industry.short))) +
            coord_flip()
ggplotly(sector.bar)
layout(temp,margin=c(l=80,r=80,b=10,t=100,pad=0))