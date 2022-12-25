home_insurance=read.csv(file.choose())
library(readr) # CSV fxile I/O, e.g. the read_csv function
library(knitr)
library(car)
library(data.table)
library(plotrix)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(lubridate)

###Showing the 6 first records of the Home Insurance to get more familiar with the data


home_insurance=data.table(read.csv("C:/Users/sagar/OneDrive/Documents/HOME_INSURANCE.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL")))
top5=t(head(home_insurance))

##I will start by removing the features that are not useful.

home_insurance$CAMPAIGN_DESC <- NULL
home_insurance$i <- NULL

##Showing the 8 first records of the Home Insurance to get more familiar with the data
t(head(home_insurance, 8))

### identifying total rows of detaset
total= nrow(home_insurance)
total

## getting all column heads
str(home_insurance)
###Data Wrangling
status_groups = home_insurance[, .(count=.N, percent=round((.N/total)*100, 2)), by = POL_STATUS]
status_groups = status_groups[order(count, decreasing = TRUE)]
status_groups
###3d pie chart
pieLabels =paste(status_groups$POL_STATUS,' ', status_groups$percent, '%')
pie3D(status_groups$count,labels=pieLabels,explode=0.1, radius=0.8,height=0.1, col=rainbow(length(status_groups$POL_STATUS)),
      main="Pie Chart of Policy Status ")



## getting count of resiliation & non resiliation

status_groups[POL_STATUS != 'Lapsed', POL_STATUS:= "Non Resiliated"]
status_groups[POL_STATUS == 'Lapsed', POL_STATUS:= "Resiliated"]
status_groups =status_groups[, .(count=sum(count), percent = round((.N*100)/sum(count), 2)), by = POL_STATUS]
status_groups[,percent := round((count*100)/sum(count), 2)]
status_groups = status_groups[order(count, decreasing = TRUE)]
status_groups

###3d pie chart
pieLabels = paste(status_groups$POL_STATUS,' ', status_groups$percent, '%')
pie =pie3D(status_groups$percent,labels=pieLabels,explode=0.1, radius=0.8,height=0.1, col=rainbow(length(status_groups$POL_STATUS)),
           main="Pie Chart of Resiliation")


##For create the models later i will save this information in the original dataset too.
home_insurance$Resiliated[home_insurance$POL_STATUS == 'Lapsed'] <- 1
home_insurance$Resiliated[home_insurance$POL_STATUS != 'Lapsed'] <- 0
top3=t(head(home_insurance, 3))

#### policy covered
total_coverage=home_insurance[, .(SUM_INSURED_BUILDINGS, SUM_INSURED_CONTENTS, SPEC_SUM_INSURED)]
name ='total_coverage'
home_insurance[, (name):= SUM_INSURED_BUILDINGS+ SUM_INSURED_CONTENTS+ SPEC_SUM_INSURED]
ordered_table = home_insurance[order(total_coverage, decreasing = TRUE), .(Police, SUM_INSURED_BUILDINGS, SUM_INSURED_CONTENTS, SPEC_SUM_INSURED, total_coverage)]

ordered_table =data.table(ordered_table)
head(ordered_table, 8)### top 8 of orderd table

###Client's professional status
status_client = home_insurance[!is.na(P1_EMP_STATUS), .(count=.N), by = P1_EMP_STATUS]
status_client = status_client[order(count, decreasing = TRUE)]
status_client

### bar plot of client's proffessional status

palette_colors <-  c("#5e482c", "#f7d2a7", "#df45a4", "#d1223d", "#fbdb50", "#cd500d", "#d5addf", "#206536", "#b98d9b", "#ebaa7b", "#85a664", "#ef99fa")
ordered_status <- status_client$P1_EMP_STATUS
ggbarplot(status_client, x= "P1_EMP_STATUS", y= "count", xlab="Client's professional status", ylab ="quantity policies", fill = "P1_EMP_STATUS", label=TRUE, title = "Clients professional status", label.pos = "out", order = ordered_status, palette = palette_colors)


##Correlations

ggscatter(home_insurance, x = "SUM_INSURED_BUILDINGS", y = "RISK_RATED_AREA_B", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Assured Sum - Building", ylab = "Geographical Classification of Risk - Building")


##premium clients with Total for the previous year bonus
risk = home_insurance[LAST_ANN_PREM_GROSS > 0]
#head(risk, 2)
ggqqplot(home_insurance$LAST_ANN_PREM_GROSS, ylab = "Premium - Total for the previous year")

## most popular and most successful months and day for quotation and cover start.



month_order = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
day_order =c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
home_insurance$quotemonth_n = month(as.POSIXlt(home_insurance$QUOTE_DATE, format="%m/%d/%Y"))
home_insurance$covermonth_n = month(as.POSIXlt(home_insurance$COVER_START, format="%d/%m/%Y"))
#i name the columns
quotesmonthDF = data.frame(month_n = home_insurance$quotemonth_n )

#head(quotesmonthDF, 2)
#i avoid the null values and make the group by each month to get the monthly total
quotesmonthgroup = data.table(quotesmonthDF)
quotesmonthgroup =quotesmonthgroup[month_n <= 12]
quotesmonthgroup =quotesmonthgroup[(order(month_n)), .(count=.N), by=month_n]


#i add the name of the month
quotesmonthgroup$month_s = month_order[quotesmonthgroup$month_n ]

head(quotesmonthgroup, 12)


home_insurance$quotemonth_s <- month_order[home_insurance$quotemonth_n ]

###### barplot of most successfull month quotation month
ggbarplot(quotesmonthgroup, x= "month_s" , y= "count", xlab="Month", ylab ="quantity policies", label=TRUE, title = "Most successful months in Quotation date", label.pos = "out", fill = "month_s", color = "month_s", palette = palette_colors)



###Number of buildings by year
year_built <- home_insurance[home_insurance$YEARBUILT != 'null' & home_insurance$YEARBUILT > 0, .(count=.N), by = YEARBUILT]
year_built <- year_built[order(YEARBUILT)]
year_built


qplot(x = year_built$YEARBUILT, y = year_built$count, xlab="Year of building construction", ylab="Number of Policies", main = "Number of buildings by year construction" , geom="line")





###House susceptible to floods

flooding=subset(home_insurance,FLOODING=="Y")
FL_TOP5=head(flooding, 5)
flood_groups = home_insurance[, .(count=.N, percent=round((.N/total)*100, 2)), by = FLOODING]
flood_groups = flood_groups[order(count, decreasing = TRUE)]
flood_groups
###3d pie chart
pieLabels =paste(flood_groups$FLOODING,' ', flood_groups$percent, '%')
pie3D(flood_groups$count,labels=pieLabels,explode=0.1, radius=0.8,height=0.1, col=rainbow(length(flood_groups$FLOODING)),
      main="Pie Chart of flood Status ")




###PAYING GUESTS NOT = TO 0
PG=subset(home_insurance,PAYING_GUESTS!="0")
PG_5=head(PG,5)



##PAYMENT METHOD & FREQUENCY
PAYMENT=home_insurance%>%filter(PAYMENT_METHOD=="PureDD"&PAYMENT_FREQUENCY==1)
PAYMENT_5=head(PAYMENT,5)
PAYMENT_groups = home_insurance[, .(count=.N, percent=round((.N/total)*100, 2)), by = PAYMENT_METHOD]
PAYMENT_groups = PAYMENT_groups[order(count, decreasing = TRUE)]
PAYMENT_groups
pieLabels =paste(PAYMENT_groups$PAYMENT_METHOD,' ', PAYMENT_groups$percent, '%')
pie3D(PAYMENT_groups$count,labels=pieLabels,explode=0.1, radius=0.8,height=0.1, col=rainbow(length(PAYMENT_groups$PAYMENT_METHOD)),
      main="Pie Chart of PAYMENT METHOD ")



##OWENERSHIP TYPE
OWENER_TYPE=home_insurance%>%select(OWNERSHIP_TYPE,Police)%>%filter(OWNERSHIP_TYPE=="3")
OT_5=head(OWENER_TYPE,5)

