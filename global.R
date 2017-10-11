library(shiny)
library(ggplot2)
library(googleVis)
library(shinythemes)
library(dplyr)
library(readr)
library(shinydashboard)
library(stats)
library(data.table)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(shinythemes)
library(xts)
library(dygraphs)
library(plotly)



lc <- fread(input = "lendingclub_finaldata.csv")
lc <- select(lc, loan_amnt, funded_amnt, term, int_rate, grade, sub_grade, emp_length, annual_inc, loan_status,
       loan_status, purpose, addr_state, dti, delinq_2yrs, fico_range_low, inq_last_6mths,
       collections_12_mths_ex_med, policy_code, application_type, open_il_24m, avg_cur_bal, delinq_amnt,
       num_tl_op_past_12m, pub_rec_bankruptcies, tot_hi_cred_lim, issued_month, issued_yr, credit_age) %>%
       filter(application_type == "INDIVIDUAL")

lc <- select(lc, -funded_amnt, -policy_code, -application_type)

subgrade_int <- group_by(lc, grade, sub_grade) %>% summarise(avg_int = mean(int_rate))
scandal_table1 <- group_by(lc, issued_month, issued_yr) %>% summarise(total_loan_amount = sum(loan_amnt), count = n()) %>% arrange(desc(count))
grade_facet <- select(lc, grade, int_rate) 


by_status <- group_by(lc, issued_yr, loan_status, grade, term) %>% summarise(count = n())
total_status <- group_by(lc, issued_yr, grade, term) %>% summarise(total_count = n())
default_rate1 <- left_join(by_status,total_status,by = c("issued_yr","grade","term")) %>% mutate(prop = count/total_count) %>% filter(loan_status == "Charged Off")

lc$issue_d <- paste0(lc$issued_month, "-", lc$issued_yr)
lc$issue_d <- as.Date(gsub("^", "01-", lc$issue_d), format="%d-%b-%Y")

amnt_evol <- group_by(lc, issue_d) %>% summarise(total_amount = sum(loan_amnt))


test1 <- group_by(lc, issued_yr) %>% summarise(total_loan = sum(loan_amnt))
ggplot(test1, aes(x = issued_yr, y = total_loan)) + geom_bar(stat = "identity")

test2 <- filter(lc, grade == "A") %>% group_by(issued_yr) %>% summarise(avg_int = mean(int_rate)) 
ggplot(test2, aes(x = issued_yr, y = avg_int)) + geom_line(stat = "identity") 

test3 <- group_by(lc, grade, issued_yr) %>% summarise(total_loan = sum(loan_amnt))
ggplot(test3, aes(x = issued_yr, y = total_loan)) + geom_bar(stat = "identity", aes(fill = grade))

test4 <- group_by(lc, issued_yr) %>% summarise(annual_loan = sum(loan_amnt))
final_test <- left_join(test3, test4, by = "issued_yr") %>% mutate(prop = total_loan/annual_loan) %>% select(grade, issued_yr, prop)
ggplot(final_test, aes(x = issued_yr, y = prop)) + geom_bar(stat = "identity", aes(fill = grade)) +
  theme(plot.background = element_rect(fill = 'lightgrey', colour = 'red'))

dytest2 <- group_by(lc, issue_d) %>% summarise(total_loan = sum(loan_amnt))
time_series2 <- xts(dytest2, order.by = dytest2$issue_d)
dygraph(time_series2, main = "Growth in Loans funded") %>%
  dyEvent("2014-8-07", "IPO", labelLoc = "bottom") %>%
  dyEvent("2016-5-01", "CEO resigns", labelLoc = "bottom") %>%
  dyOptions(maxNumberWidth = 21)
  
test1 = group_by(lc, issued_yr) %>% summarise(avg_int = mean(int_rate))
                                                                      
 #  scale_x_continuous(name="annual income", labels = scales::comma)

#cor(lc$int_rate, lc$annual_inc)


