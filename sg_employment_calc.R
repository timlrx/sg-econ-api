##### Estimate employment figures from gdp and productivity data #####

library(dplyr)

files_gdp <- list.files("data/gdp/")
files_prd <- list.files("data/productivity/")

date_list_gdp <- gsub("(.+)(-gdp.RData)","\\1",files_gdp)
recent_date_gdp <- date_list_gdp[order(format(as.Date(date_list_gdp),"%m%d"))[length(date_list_gdp)]]
load(paste("data/gdp/",recent_date_gdp,"-gdp.RData", sep=""))
gdp <- va.sector.comb %>%
       select(industry, period, va)

date_list_prd <- gsub("(.+)(-productivity.RData)","\\1",files_prd)
recent_date_prd <- date_list_prd[order(format(as.Date(date_list_prd),"%m%d"))[length(date_list_prd)]]
load(paste("data/productivity/",recent_date_prd,"-productivity.RData", sep=""))
productivity <- productivity %>%
                select(industry, period, industry.short, year, quarter, productivity)

emp <- inner_join(gdp, productivity, by=c("industry","period")) %>%
       mutate(emp = (va*10^6)/productivity) %>%
       select(industry, period, year, quarter, industry.short, emp) %>%
       group_by(industry, quarter) %>%
       mutate(emp_change = emp - lag(emp)) %>%
       ungroup()

date <- ifelse(recent_date_gdp > recent_date_prd,
               recent_date_gdp,
               recent_date_prd)
save(emp, file=paste("data/employment/",date,"-emp.RData",sep=""))
