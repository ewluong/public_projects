# load libraries
library(readxl)
library(dplyr)
library(utils)
library(base)
library(dplyr)
library(ggplot2)
library(lubridate)
library(Metrics)
library(caret)
library(readr)
library(plotly)
library(mass)
library(HistData)
library(Hmisc)
library(lattice)
library(survival)
library(Formula)
library(jiebaRD)
#load data
book <- read_csv('/Users/ericluong/public_projects/amazon_bestsellers/bestsellers with categories.csv')
View(book)

# Data observation for better understanding 

head(book) 
# head() returns us the first n rows of the data. In this particular dataset there are 7 coloumns 

colnames(book) 
# colnames returns the columns names of the data set.

str(book)
# str() returns the structure of the data in a list format which includes the datatype of the columns, values in the columns.In the
# data set there are no POSIXct type columns, char type columns and num type columns. There are a total of 550 rows.

summary(book) 
# summary provides the summary stats of the data set.We can observe the quartile deviation of the data but this is only useful if the data 
# is continuous otherwise the information may be inaccurate.                                                                                              columns

# Check for Null or NA values and if exists find total null values in the data set.
sum(is.na(book))
# There are 0 null values in the data set.

#Check for any duplicate rows
duplicated(book)
# duplicated() returns a logical vector where TRUE specifies which elements of a vector or data frame are duplicates. 
sum(duplicated(book))
# we use sum() to calculate the number of times TRUE appeared in the vector list. In the data set there are now duplicates.

# Find various categories
categories <- unique(book$Genre)
categories
# There are 2 Generes - FICTION and NON-FICTION

authors<- unique(book$Author)
authors

# Which kind of books is more popular?
Genre <- book$Genre
piepercent = paste(round(100*table(Genre)/sum(table(Genre))), "%")
cols = c("#40DCA7","#106E85")
pie(table(Genre), label = paste(piepercent, table(Genre), sep=" , "), col=cols, main = "Genre")
legend("topleft", c("Fiction","Non Fiction "), cex=0.8, fill=cols)
# Non-fiction books are more popular

# Which price range do people prefer to buy?
Price <- book$Price
Price <- as.numeric(Price)
Price2 <- table(Price)
Price2 <- as_tibble(Price2)
plot_ly(Price2, x = ~Price, y = ~n, type = 'scatter',
        mode = "markers", marker = list(color = c("#F31")))
# Book with prices between $0 and $20 are the best sellers.

# Which book is the cheapest?
cheapest_bookname<-book$Name[which.min(Price)]
cheapest_bookauthor<-book$Author[which.min(Price)]
cheapest_bookprice<-min(Price)
cheapest_bookname
cheapest_bookauthor
cheapest_bookprice
#The cheapest book is Cabin Fever (Diary of a Wimpy Kid, Book 6) by Jeff Kinney, priced at $0

# What is the most expensive book?
mostexpensive_bookname<-book$Name[which.max(Price)] 
mostexpensive_bookauthor<-book$Author[which.max(Price)]
mostexpensive_bookprice<-max(Price)
mostexpensive_bookname
mostexpensive_bookauthor
mostexpensive_bookprice
#The most expensive book is Diagnostic and Statistical Manual of Mental Disorders, 5th Edition:
# DSM-5 by American Psychiatric Association, priced at $105

# Analyzing the Reviews & Year for trends
  ## What’s the relationship between Reviews & Year?
RY <- book[ ,c(6,4)]
RY$Reviews <- as.numeric(RY$Reviews)
plot_ly(RY, x = ~Year, y = ~Reviews, type = 'scatter',
        mode = "markers", marker = list(color = c("#009")))

 ## What’s the average reviews of each year?
avgrevyear<-book%>%group_by(Year)%>% summarise(Reviews=mean(Reviews))
plot_ly(avgrevyear, x = ~Year, y = ~Reviews, type = 'bar', marker = list(color = c("#40DCA7")))
# 

 ## What’s distribution of reviews more than 4000?
sum_of_review <- function(n){
  return(sum(subset(RY, (Year %in% n))$Reviews >= 4000))
}
sumofreview_over4000 <- c(sum_of_review(2009),sum_of_review(2010),sum_of_review(2011),sum_of_review(2012),
                          sum_of_review(2013),sum_of_review(2014),sum_of_review(2015),sum_of_review(2016),
                          sum_of_review(2017),sum_of_review(2018),sum_of_review(2019))
year <-c(2009:2019)
plot_ly(data.frame(year, sumofreview_over4000), x = ~year, y = ~sumofreview_over4000, type = 'scatter', mode = "markers", marker = list(color = c("red")))

#

 ## What’s the distribution of User Rating?
User_Rating <- book$`User Rating`
User_Rating <- as.numeric(User_Rating)
prop = prop.table(User_Rating)
plot_ly((book), x = ~User_Rating, y = ~prop, type = 'bar', marker = list(color = c("#40A")))
# The distribution of User Rating follows a left skew, while the peak 4.8 is the rating score that most people choose.

# What themes of books is more popular?
