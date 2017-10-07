library(data.table)
library(ggplot2)
library(zoo)


lc_07_11 <- fread(input = "~/Desktop/lc_extracts/LoanStats_2007_2011.csv", 
               header = TRUE)

lc_12_13 <- fread(input = "~/Desktop/lc_extracts/LoanStats2012_2013.csv", 
                  header = TRUE)

lc_14 <- fread(input = "~/Desktop/lc_extracts/LoanStats2014.csv", 
                  header = TRUE)

lc_15 <- fread(input = "~/Desktop/lc_extracts/LoanStats2015.csv", 
               header = TRUE)

lc_16q1 <- fread(input = "~/Desktop/lc_extracts/LoanStats_2016Q1.csv", 
               header = TRUE)
lc_16q2 <- fread(input = "~/Desktop/lc_extracts/LoanStats_2016Q2.csv", 
                 header = TRUE)
lc_16q3 <- fread(input = "~/Desktop/lc_extracts/LoanStats_2016Q3.csv", 
                 header = TRUE)
lc_16q4 <- fread(input = "~/Desktop/lc_extracts/LoanStats_2016Q4.csv", 
                 header = TRUE)

lc_17q1 <- fread(input = "~/Desktop/lc_extracts/LoanStats_2017Q1.csv", 
                 header = TRUE)
lc_17q2 <- fread(input = "~/Desktop/lc_extracts/LoanStats_2017Q2.csv", 
                 header = TRUE)

loandata = rbind(lc_07_11, lc_12_13, lc_14, lc_15, lc_16q1, lc_16q2, lc_16q3, lc_16q4, lc_17q1, lc_17q2)

#unique(names(lc_07_11)) == unique(names(lc_12_13)) == unique(names(lc_14)) == unique(names(lc_15)) == unique(names(lc_16q1)) == unique(names(lc_16q2)) == unique(names(lc_16q3)) == unique(names(lc_16q4)) == unique(names(lc_17q1)) == unique(names(lc_17q2))
#save(LC, file = "LC.RData")
#rm(list=ls())

#clean data
loandata$int_rate = as.numeric(gsub('%','',loandata$int_rate))
loandata$term = as.numeric(gsub('months','',loandata$term))
loandata$id <- NULL
loandata$member_id <- NULL
loandata$emp_title <- NULL
loandata$emp_length <- gsub('years','',loandata$emp_length)
loandata$emp_length <- gsub('[[:punct:]]','',loandata$emp_length)
loandata$emp_length <- gsub('<','',loandata$emp_length)
loandata$emp_length <- gsub(' 1 year','0',loandata$emp_length)
loandata$emp_length <- gsub('1 year','1',loandata$emp_length)
loandata$emp_length <- gsub('n/a',NA,loandata$emp_length)
loandata$emp_length = as.numeric(loandata$emp_length)
loandata$annual_inc <- round(as.numeric(loandata$annual_inc),0)
loandata$issued_month <- substr(loandata$issue_d,1,3)
loandata$issued_yr <- substr(loandata$issue_d,5,8)
loandata$issue_d <-  NULL
loandata$loan_amnt <- as.numeric(loandata$loan_amnt)
loandata$installment <- NULL
loandata$url <- NULL
loandata$desc <- NULL
loandata$title <- NULL
loandata <- loandata[substr(loandata$zip_code,4,5) != "",]
loandata$dti <- as.numeric(loandata$dti)
loandata$delinq_2yrs <- as.numeric(loandata$delinq_2yrs)
loandata$earliest_cr_line <-  as.Date(paste("01-", loandata$earliest_cr_line, sep = ""), format = "%d-%b-%Y")
loandata$credit_age <- as.numeric((as.Date("2017-10-07") - loandata$earliest_cr_line)/365)
loandata$earliest_cr_line <- NULL
loandata <-  loandata[loandata$inq_last_6mths != "",]

View(loandata)
dim(loandata)

class(loandata$delinq_2yrs)
unique(loandata$earliest_cr_line)

m <- "Jan-01"
as.Date(paste("01-", m, sep = ""), format = "%y-%b-%d")



loandata$earliest_cr_line[1]
 - as.Date("2017-10-06")

as.yearmon(m, "%b-%y")
class(as.yearmon(m, "%b-%y"))

test1 <- group_by(loandata, earliest_cr_line) %>% summarise(avg_int = mean(int_rate)) 
ggplot(test1, aes(x = earliest_cr_line, y = avg_int)) + geom_point()

unique(loandata$inq_last_6mths)
unique(loandata$pymnt_plan)
unique(loandata$loan_status)

View(loandata[loandata$inq_last_6mths != "",])
names(loandata)

#standard group by function
group_by(loandata, loandata$inq_last_6mths) %>% summarise(count = n()) %>% arrange(desc(count))
group_by(loandata,issued_month, issued_yr) %>% summarise(total_loan = sum(loan_amnt), count = n()) %>% arrange(desc(total_loan)) %>% filter(issued_yr > 2010)

bymon <- group_by(loandata,issued_month, issued_yr) %>% summarise(loan = sum(loan_amnt), count = n())
byyr <- group_by(loandata,issued_yr) %>% summarise(total_loan = sum(loan_amnt), total_count = n())

scandal <- left_join(bymon,byyr,by="issued_yr")
scandal <- mutate(scandal, loan_perc = loan/total_loan, count_perc = count/total_count)



scandal %>% arrange(desc(loan_perc))
test1 <- filter(loandata, issued_yr > 2010)

ggplot(test1, aes(x = issued_month, y = loan_amnt))  + 
  geom_point(stat = "identity") + geom_smooth()
  facet_grid(issued_yr ~ .) +
  scale_x_discrete(limits=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul","Aug","Sep","Oct","Nov","Dec"))
  
View(scandal)

ggplot(test1, aes(x = issued_month, y = total_loan)) + geom_bar(stat = "identity") + 
  scale_x_discrete(limits=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul","Aug","Sep","Oct","Nov","Dec")) +
  facet_grid(issued_yr ~ .) 

class(loandata$loan_amnt)

unique(is.na(loandata$emp_length))
unique(loandata$verification_status)
class(loandata$issue_d)
unique(loandata$issue_d)

#graphs for later: 
ggplot(group_by(loandata, credit_age) %>% summarise(avg_int = mean(int_rate)),
       aes(x = credit_age, y = avg_int)) + geom_point()
ggplot(group_by(loandata, inq_last_6mths) %>% summarise(avg_int = mean(int_rate)),
       aes(x = inq_last_6mths, y = avg_int)) + geom_point()

