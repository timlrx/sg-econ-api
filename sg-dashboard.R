### Descriptive Plots

library(dplyr)
library(ggplot2)
library(viridis)

### Find most recent file
#### Load GDP data
files <- list.files("data/gdp/")
date_list <- gsub("(.+)(-gdp.RData)","\\1",files)
recent_date <- date_list[order(format(as.Date(date_list),"%m%d"))[length(date_list)]]
recent_date2 <- date_list[order(format(as.Date(date_list),"%m%d"))[length(date_list)-1]]

load(paste("data/gdp/",recent_date,"-gdp.RData", sep=""))
gdp <- va.sector.comb
load(paste("data/gdp/",recent_date2,"-gdp.RData", sep=""))
gdp2 <- va.sector.comb

#### Load Emp data
files <- list.files("data/employment/")
date_list <- gsub("(.+)(-emp.RData)","\\1",files)
recent_date <- date_list[order(format(as.Date(date_list),"%m%d"))[length(date_list)]]
load(paste("data/employment/",recent_date,"-emp.RData", sep=""))

# Plot aggregate trend over the last 4 years
overall_gdp <- gdp %>%
  filter(industry=="Overall Economy") %>%
  top_n(16, wt=period)

overall_qoq <- ggplot(overall_gdp, aes(period, qoqsaa, group=1)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, linetype=2) +
  ylab("QoQ SA (%)") +
  xlab("Period") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))

overall_yoy <- ggplot(overall_gdp, aes(period, yoy, group=1)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, linetype=2) +
  ylab("YoY (%)") +
  xlab("Period") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))

#Plot sector level qoq growth bar graph
ind_growth <- gdp %>%
  filter(industry!="Other Goods Industries" & industry!="Other Services Industries" ) %>%
  top_n(1, wt=period) %>%
  melt(measure.vars=c("yoy","qoqsaa"), value.name="growth", variable.name="series") %>%
  arrange(industry)

ind_growth$industry.short <- factor(ind_growth$industry.short,
                                    levels=rev(levels(ind_growth$industry.short)))

sector.bar <- ggplot(ind_growth, aes(industry.short, growth)) +
  geom_col(aes(fill=series), position=position_dodge(width=1)) +
  ylab("Growth (%)") +
  xlab("Industry") +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1),
        axis.title.x=element_blank()) +
  theme_classic() + 
  coord_flip()

ggplotly(sector.bar)

### Interesting fact - number of consecutive -ve / +ve YoY quarters

count_streak <- function(x){ tmp<-cumsum(x);tmp-cummax((!x)*tmp)}

va_streak <- gdp %>%
  filter(industry!="Other Goods Industries" & industry!="Other Services Industries" ) %>%
  group_by(industry) %>% 
  filter(year > 2000) %>%
  mutate(neg_yoy = yoy < 0,
         pos_yoy = yoy > 0,
         neg_yoy_streak = count_streak(neg_yoy),
         pos_yoy_streak = count_streak(pos_yoy)) %>%
  ungroup() %>%
  top_n(10, wt=period) %>%
  select(industry, neg_yoy_streak, pos_yoy_streak) %>%
  arrange(industry)

va_streakindustry <- as.character(va_streak$industry)
va_streak$industry <- gsub("&", "and", va_streak$industry)

va_streak$industry <- as.character(va_streak$industry)
colnames(va_streak) <- c("industry", "-ve", "+ve")

### Compare data revisions

revision_period <- gdp2$period[dim(gdp2)[1]]

#### Take the previous quarter
updated_va <- gdp %>%
  filter(industry!="Other Goods Industries" & industry!="Other Services Industries" ) %>%
  top_n(20, wt=period) %>%
  filter(row_number() <= 10) %>%
  select(industry, yoy) %>%
  rename(updated_yoy = yoy)

#### Compare against last quarter's data
old_va <- gdp2 %>%
  filter(industry!="Other Goods Industries" & industry!="Other Services Industries" ) %>%
  top_n(10, wt=period) %>%
  select(industry, yoy) %>%
  rename(old_yoy = yoy)

#### Take overall economy and 3 biggest revisions
compare_tbl <- updated_va %>%
               inner_join(old_va, by="industry") %>%
               mutate(revision = updated_yoy - old_yoy,
                      abs_revision = abs(revision)) %>%
               arrange(desc(abs_revision)) %>%
               select(industry, updated_yoy, revision) %>%
               filter(industry=="Overall Economy" |
                      row_number() <= 3)


####### Employment graphs #######

#### Yearly Employment and year to date

latest_quarter <- emp$quarter[dim(emp)[1]]

ytd_emp <- emp %>% 
           group_by(industry, year) %>%
           mutate(avg_emp = mean(emp)) %>%
           filter(quarter == latest_quarter) %>%
           group_by(industry) %>%
           mutate(change_emp = avg_emp - lag(avg_emp)) %>%
           ungroup()
  
#### Plot graph of change in employment since 2000

overall_change_emp <- emp %>%
  filter(year >= 2000 & industry=="Overall Economy") %>%
  mutate(period_lab=ifelse(quarter!="Q1","",period))

ggplot(overall_change_emp, aes(period, pct_change_emp, group=industry, color=industry)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, linetype=2) +
  ylab("YoY change in employment (%)") +
  xlab("Period") +
  scale_x_discrete(breaks=overall_change_emp$period_lab) +
  theme_classic() +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))

pct_change_emp <- emp %>%
    filter(year >= 2014 & industry.short!="Overall" & industry.short!="Other Ser") %>%
    ggplot(aes(period, pct_change_emp, group=industry, color=industry)) +
           geom_point() +
           geom_line() +
           geom_hline(yintercept = 0, linetype=2) +
           ylab("YoY change in employment (%)") +
           xlab("Period") +
           theme_classic() +
           scale_color_discrete +
           theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))

### Alternative
### Let 2011 be the stock and the changes will be relative to 2011
#ytd_emp$change_emp <- ifelse(ytd_emp$year==2011, ytd_emp$avg_emp, ytd_emp$change_emp)

### Change in emp since 2010
change_emp_year <- ytd_emp %>%
  filter(industry.short!="Overall" & industry.short!="Other Ser") %>%
  filter(year >= 2011) %>%
  mutate(year = factor(year), change_emp = change_emp/1000) %>%
  arrange(desc(year)) %>%
  ggplot(aes(x=industry)) +
  geom_col(aes(y = change_emp, fill = year),  
           position = position_stack(reverse = TRUE)) +
  coord_flip() +
  ylab("Change in Employment Since 2010 (thousands)") +
  xlab("Industry") +
  scale_y_continuous(breaks=c(-40,-20,0,20,40,60,80,100)) +
  theme_classic() 

##############
## Old code ##
##############

# Examine a particular sector (e.g trend in other goods)
other.goods.data <- va.sector.comb %>% filter(year>=2012 & industry=="Other Goods Industries") %>% 
  select(period, yoy, qoqsaa) %>% melt(measure.vars=c("yoy","qoqsaa"))

other.goods.plot<-ggplot(other.goods.data, aes(period, value, group=variable, color=variable)) +
  geom_point() + geom_line() + ylab("QoQ SA (%)") + xlab("Period") + theme_classic() +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))

# Examine the finance and insurance sector
fi.data <- va.sector.comb %>% filter(year>=2010 & industry=="Finance & Insurance") %>% 
  select(period, yoy, qoqsaa, quarter) %>% melt(measure.vars=c("yoy","qoqsaa"))

## Looks like there is some seasonality in the data
fi.plot.yoy<-ggplot(fi.data[fi.data$variable=="yoy",], aes(period, value,group=1)) +
  geom_point() + geom_line() + ylab("QoQ SA (%)") + xlab("Period") + theme_classic() +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))

library(ggrepel)
fi.plot.qoq<-ggplot(fi.data[fi.data$variable=="qoqsaa",], aes(period, value,group=1)) +
  geom_point() + geom_line() + geom_text_repel(aes(label=quarter))  + ylab("QoQ SA (%)") + xlab("Period") + theme_classic() +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))