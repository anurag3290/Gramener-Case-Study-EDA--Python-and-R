## include libraries
library(tidyr)
library(ggplot2)
library(dplyr)
library(data.table)
library(gsubfn)
library(e1071)

## Upload the dataset
loan <- read.csv("loan.csv",stringsAsFactors = F)
## Data Cleaning
## Unnecessary Columns - We can remove the columns of the dataframe that have same values across all the rows
loan <-  loan[sapply(loan, function(x) length(unique(x))>1)]
## We can also remove the columns of the dataframe that either have 0 values or have blank values
loan$tax_liens <- NULL
loan$chargeoff_within_12_mths <- NULL
loan$collections_12_mths_ex_med <- NULL
## We can remove those columns which have majorily blank values and the existing value is of no use
loan$next_pymnt_d <- NULL
## we can also remove the title, desc and url
loan$title <- NULL
loan$desc <- NULL
loan$url <- NULL
## We can remove outstanding Pricipal as it is only for current payment behaviour(not needed)
loan$out_prncp <- NULL
loan$out_prncp_inv <- NULL
## Similarly we can remove Recoveries as it is only for Charged Off status and cant be compared
loan$recoveries <- NULL
## COlumn containing multiple data values - Separating date (already standardized) into months and date for analysis
loan <- separate(loan, earliest_cr_line,into=c('earliest_cr_line.month','earliest_cr_line.date'),sep="-")
loan <- separate(loan, issue_d,into=c('issue_d.month','issue_d.date'),sep="-")
## similarly
##loan <- separate(loan, last_pymnt_d,into=c('last_pymnt_d.month','last_pymnt_d.date'),sep="-")
##loan <- separate(loan, last_credit_pull_d,into=c('last_credit_pull_d.month','last_credit_pull_d.date'),sep="-")

## Identifying NA values
## emp_length have 1075 'n/a' values
loan <- loan[!(loan$emp_length == 'n/a'),]
loan <- subset(loan, !is.na(loan$revol_util))
## emp_length contain 2.7% of 'n/a' values
## revol_util contain 0.13% of total data 
## corresponding rows can be removed

## However 2 columns mths_since_last_delinq and mths_since_last_record have large amount of NA values
sum(is.na(loan$mths_since_last_delinq))
sum(is.na(loan$mths_since_last_record))
## they can be removed from dataframe too
loan$mths_since_last_delinq <- NULL
loan$mths_since_last_record <- NULL
## Standardise Text -Removing Extra character -Removing trailing % sign in int_rate and revol_util column
loan$int_rate <- gsub("%","",as.character(loan$int_rate))
loan$int_rate <- as.numeric(as.character(loan$int_rate))
loan$revol_util <- gsub("%","",as.character(loan$revol_util))
loan$revol_util <- as.numeric(as.character(loan$revol_util))

## We identified 16 major columns which factors of interest 
## loan_amount funded_amount annual_inc term funded_amount_inv
## inq_last_6mths open_acc verification_status emp_length installment
## int_rate home_ownership dti purpose grade revol_util

## Starting with Univariate analysis, we can first take some insight from 'purpose'
## For deep insight we need to create a temporary dataframe called - 'purpose'
purpose <-  as.data.frame.matrix(with(loan, table(purpose, loan_status)))
setDT(purpose, keep.rownames = T)
colnames(purpose)[1] <- 'Purpose'
## purpose dataframe clearly shows that maximum number of loan (for all three status) is taken for 'debt_consolidation'
## But if we find the % of charged off for each purpose
purpose <- purpose %>% mutate(prcnt_charged_off = (purpose$`Charged Off`)/(purpose$`Charged Off` +purpose$`Fully Paid` + purpose$Current)*100)
ggplot(purpose, aes(x=Purpose,y=prcnt_charged_off))+geom_bar(stat = "identity")
## the plot shows that "small business" tends to be the hisghest percentage in loan defaulters
## Purging the temporary dataframe
remove(purpose)

## Similary we are analysisng how the grade effect on defaulters
grades <-  as.data.frame.matrix(with(loan, table(grade, loan_status)))
setDT(grades, keep.rownames = T)
colnames(grades)[1] <- 'Grades'
## purpose dataframe clearly shows that maximum number of loan (for all three status) is taken for Grade= 'B'
## But if we find the % of charged off for each Grades
grades <- grades %>% mutate(prcnt_charged_off = (grades$`Charged Off`)/(grades$`Charged Off` +grades$`Fully Paid` + grades$Current)*100)
ggplot(grades, aes(x=Grades,y=prcnt_charged_off))+geom_bar(stat = "identity")
## the plot shows that Grade='G' tends to be the hisghest percentage in loan defaulters
## Purging the temporary dataframe
remove(grades)


## Analysing States
states <-  as.data.frame.matrix(with(loan, table(addr_state, loan_status)))
setDT(states, keep.rownames = T)
colnames(states)[1] <- 'States'
## state dataframe clearly shows that maximum number of loan (for all three status) is taken for State= 'CA'
## But if we find the % of charged off for each Grades
states <- states %>% mutate(prcnt_charged_off = (states$`Charged Off`)/(states$`Charged Off` +states$`Fully Paid` + states$Current)*100)
ggplot(states, aes(x=States,y=prcnt_charged_off))+geom_bar(stat = "identity")
## the plot shows that State='NE' tends to be the hisghest percentage in loan defaulters
## Purging the temporary dataframe
remove(states)

## Analysing Verification
verification <-  as.data.frame.matrix(with(loan, table(verification_status, loan_status)))
setDT(verification, keep.rownames = T)
colnames(verification)[1] <- 'Status'
## state dataframe clearly shows that maximum number of loan defaulters are Not Verified
## But if we find the % of charged off for each Verification Status
verification <- verification %>% mutate(prcnt_charged_off = (verification$`Charged Off`)/(verification$`Charged Off` +verification$`Fully Paid` + verification$Current)*100)
ggplot(verification, aes(x=Status,y=prcnt_charged_off))+geom_bar(stat = "identity")
## the plot shows that thos whose income has been verified by LC tends to be the hisghest percentage in loan defaulters
## Purging the temporary dataframe
remove(verification)

## Analysing the Term for which loan is approved for
term <-  as.data.frame.matrix(with(loan, table(term, loan_status)))
setDT(term, keep.rownames = T)
colnames(term)[1] <- 'Terms'
## term dataframe clearly shows that maximum number of loan defaulters have term of 36 months
## But if we find the % of charged off for each term
term <- term %>% mutate(prcnt_charged_off = (term$`Charged Off`)/(term$`Charged Off` +term$`Fully Paid`)*100)
ggplot(term, aes(x=Terms,y=prcnt_charged_off))+geom_bar(stat = "identity")
## the plot shows that those who has loan for term 60 months tends to be the hisghest percentage in loan defaulters
## Purging the temporary dataframe
ggplot(loan,aes(x=loan$term,y=loan$dti,color = loan$loan_status))+geom_bar(stat = 'identity')+xlab('Term')+ylab('DTI')+ggtitle('Current Loans Only for 60 months Term')
## One interesting thing to note from above grpah is that there is no Current Loan for 36 month term
remove(term)

## Analysing for dti_range
## defining a rank column based on the range of dti as dti_range
loan$dti_range <- cut(loan$dti,seq(0,30,by=5))
levels(loan$dti_range) <- c('0..5','6..10','11..15','16..20','21..25','26..30')
dti_ranges <-  as.data.frame.matrix(with(loan, table(dti_range, loan_status)))
setDT(dti_ranges, keep.rownames = T)
colnames(dti_ranges)[1] <- 'Range'
## dti_ranges dataframe clearly shows that maximum number of loan defaulters have dti between 11-15
## But if we find the % of charged off for each dti
dti_ranges <- dti_ranges %>% mutate(prcnt_charged_off = (dti_ranges$`Charged Off`)/(dti_ranges$`Charged Off` +dti_ranges$`Fully Paid`)*100)
ggplot(dti_ranges, aes(x=Range,y=prcnt_charged_off))+geom_bar(stat = "identity")
## the plot shows that those who has dti 21-25 tends to be the hisghest percentage in loan defaulters
## Purging the temporary dataframe
remove(dti_ranges)

## Analysing for annual income
## defining a rank column based on the range of annual income as inc_range
loan$inc_range <- cut(loan$annual_inc,c(0,10000,20000,30000,40000,50000,60000,70000,80000,90000,100000,500000,1000000,2000000))
levels(loan$inc_range) <- c('0-10K','10-20K','20-30K','30-40K','30-40K','40-50K','50-60K','60-70K','70-80K','80-90K','90-100K','100-500K','500-1000K')
inc_ranges <-  as.data.frame.matrix(with(loan, table(inc_range, loan_status)))
setDT(inc_ranges, keep.rownames = T)
colnames(inc_ranges)[1] <- 'Range'
## inc_ranges dataframe clearly shows that maximum number of loan defaulters have income raanges between 30-40K
## But if we find the % of charged off for each range of Income
inc_ranges <- inc_ranges %>% mutate(prcnt_charged_off = (inc_ranges$`Charged Off`)/(inc_ranges$`Charged Off` +inc_ranges$`Fully Paid`)*100)
ggplot(inc_ranges, aes(x=Range,y=prcnt_charged_off))+geom_bar(stat = "identity")
## the plot shows that those who has income 0-20K tends to be the hisghest percentage in loan defaulters
## Purging the temporary dataframe
remove(inc_ranges)

## calculating Skewness for continous variable
## installment -  Right skewed
skewness(loan$installment)

## Loan amount - Right skewed
skewness(loan$loan_amnt)

## Open Acc - Right Skewed
skewness(loan$open_acc)

## Annual Income - Strongly Right Skewed
skewness(loan$annual_inc)

## Revolving Utilization - close to Normal distribution
skewness(loan$revol_util)

## Interest Rate - Right Skewed
skewness(loan$int_rate) 


## In Bivariate Analysis, we first analyse the effect of average interest rate on purpose in case of loan defaulters
## Creating temporary df int_purpose 
int_purpose <- aggregate(loan$int_rate, by=list(loan$purpose,loan$loan_status),FUN=mean)
ggplot(int_purpose, aes(x=int_purpose$Group.1, y=int_purpose$x,color=int_purpose$Group.2))+geom_point()
## Three interesting facts i.e.
## 1. the average interest rate for the loan defaulters is higher than the average interest rate for Fully paid
## 2. The average interest rate for the current is highest. The reason being the recovery. For recovery purpose the interest rate have been continously increasing
## 3. In case of housing loans, the gap between avg int rate for defauters and fully paid is vry high. And contrasting to others, the avg rate for current is lower than the defaulters
## purging temporary df
remove(int_purpose)

## Analysing Of Rate of Interest with Various variables
hist(filter(loan,loan_status=='Charged Off')$int_rate)
## For Charged Off loan interest rate is higher for 11-13
## Analysis of Interst Rate with Grade
ggplot(loan,aes(x=loan$grade,y=loan$int_rate,color=loan$loan_status))+geom_bar(stat='identity')+xlab('Grade')+ylab('Rate Of Interest')+ggtitle('Rate Of Interest Vs Grade')
## Grade B tends to be the higher Fully Paid borrowers for high interest of rate

## Sum of Rate Of Interest vs Home Ownership
ggplot(loan,aes(x=loan$home_ownership,y=loan$int_rate,color=loan$loan_status))+geom_bar(stat='identity')+xlab('Home Ownership')+ylab('Rate Of Interest')+ggtitle('Rate Of Interest Vs Home Ownership')
## Rent and Mortgage tends to have High tendency to Fully Paid the loan for HIgh Interest

## Correlation between loan_amnt and funded_amnt
## For Charged Off
cor((filter(loan, loan_status =="Charged Off"))$loan_amnt,(filter(loan, loan_status =="Charged Off"))$funded_amnt)
## For Fully Piad
cor((filter(loan, loan_status =="Fully Paid"))$loan_amnt,(filter(loan, loan_status =="Fully Paid"))$funded_amnt)
## For Current
cor((filter(loan, loan_status =="Current"))$loan_amnt,(filter(loan, loan_status =="Current"))$funded_amnt)

## Correlation between loan_amnt and funded_amnt_inv
## For Charged Off
cor((filter(loan, loan_status =="Charged Off"))$loan_amnt,(filter(loan, loan_status =="Charged Off"))$funded_amnt_inv)
## For Fully Paid
cor((filter(loan, loan_status =="Fully Paid"))$loan_amnt,(filter(loan, loan_status =="Fully Paid"))$funded_amnt_inv)
## For Current
cor((filter(loan, loan_status =="Current"))$loan_amnt,(filter(loan, loan_status =="Current"))$funded_amnt_inv)

## Correlation between funded_amnt and funded_amnt_inv
## For Charged Off
cor((filter(loan, loan_status =="Charged Off"))$funded_amnt,(filter(loan, loan_status =="Charged Off"))$funded_amnt_inv)
## For Fully Paid
cor((filter(loan, loan_status =="Fully Paid"))$funded_amnt,(filter(loan, loan_status =="Fully Paid"))$funded_amnt_inv)
## For Current
cor((filter(loan, loan_status =="Current"))$funded_amnt,(filter(loan, loan_status =="Current"))$funded_amnt_inv)

## Seeing the above correlation among three, It can deduce that all three are highly correlated measure
## Except funded_amnt_inv shows slightly less correlation for Charged Off than for Fully Paid and Current
## It can be deduce that correlation funded_amnt_inv (agreed by investors) with loan amount applied should be greater than or equal to 95%

## Correlation between Interest Rate and Revolving Utilization rate
## Utilization rate is an imporatnt factor and contribute almost 30% in calculating credit score
## To Maximize credit score, we want Revolving Utilization Rate as low as possible
## For Charged Off
cor((filter(loan, loan_status =="Charged Off"))$int_rate,(filter(loan, loan_status =="Charged Off"))$revol_util)
## For Fully Paid
cor((filter(loan, loan_status =="Fully Paid"))$int_rate,(filter(loan, loan_status =="Fully Paid"))$revol_util)
## For Current
cor((filter(loan, loan_status =="Current"))$int_rate,(filter(loan, loan_status =="Current"))$revol_util)
ggplot(loan,aes(x=loan$int_rate,y=loan$revol_util,color = loan$loan_status))+ylab('Revolving Utilization Rate')+xlab('Interest Rate')+geom_smooth()+ggtitle('Revolving Utilization Rate Vs Interest Rate')
## Seeing the above correlation and graph folowing points can deduce:
## a. There ia a quite difference in the correlation between for charged off and For Fully Paid
## b. At interest rate 0-10% Utilization Rate for current loans decreases which is contrasting to other loan status 
## c. We conclude that 5-10% interest rate is an interest rate to which LC can consider for applications having Low Revolving Utilization Rate