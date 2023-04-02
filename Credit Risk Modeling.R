library(dplyr)
library(stringr)
library(ggplot2)
library(caTools)
library(Information)
library(betareg)

# Import data
loandata <- read.csv(file.choose(), stringsAsFactors=FALSE)
str(loandata)
summary(loandata)
head(loandata)

# Unique values of loan status
unique(loandata$loan_status)

# Remove records for loan statuses Current, In Grace Period and Late (16-30 days)
loandata <- subset(loandata, loan_status!='In Grace Period')
loandata <- subset(loandata, loan_status!='Late (16-30 days)')
loandata <- subset(loandata, loan_status!='Current')

# Count of each loan status
loandata %>% group_by(loan_status) %>% summarise(count=n())

# Combine Charged Off, Default and Late (31-120 days) to a single category: Default
loandata$loan_status <- ifelse(str_detect(loandata$loan_status, "Paid"), loandata$loan_status, "Default")

loandata %>% group_by(loan_status) %>% summarise(count=n())

(g <- ggplot(loandata, aes(x=loan_status, fill=loan_status)) + geom_bar())

# Default Rate for each Loan Grade
grade1 <- loandata %>% filter(loan_status == "Default") %>% group_by(grade) %>% summarise(default_count=n())
grade1
# Calculate default rate in each grade
grade2 <- loandata %>% group_by(grade) %>% summarise(count=n())
grade3 <- cbind(grade1,grade2)
grade3 <- grade3[,2:4]
grade3 <- mutate(grade3, default_rate=100*(grade3$default_count/grade3$count))
grade3

# Plot of default rates by grade
ggplot(grade3, aes(x=grade, y=default_rate, fill=grade)) + geom_bar(stat="identity")

# Loan Grade vs Interest Rate
loandata$int_rate = (as.numeric(gsub(pattern="%",replacement="", x=loandata$int_rate)))

# Group the data by grade and their mean interest rates
x12 <- loandata %>% filter(loan_status=="Default") %>% group_by(grade) %>% summarise(int_rate=mean(int_rate))
ggplot(x12, aes(x=grade, y=int_rate, fill=grade)) + geom_bar(stat="identity", position="dodge")

str(loandata)

# Discarding Unnecessary Attributes
discard_column = c("collection_recovery_fee", "emp_title",
                   "funded_amnt_inv", "id",
                   "installment", "last_credit_pull_d",
                   "last_pymnt_amnt", "last_pymnt_d",
                   "loan_amnt", "member_id",
                   "next_pymnt_d", "num_tl_120dpd_2m",
                   "num_tl_30dpd", "out_prncp",
                   "out_prncp_inv",
                   "total_pymnt", "total_pymnt_inv",
                   "total_rec_int", "total_rec_late_fee",
                   "url", "zip_code")

loandata <- (loandata[, !(names(loandata) %in% discard_column)])
dim(loandata)

# Grade attribute is included with sub_grade variable
loandata$grade = NULL

# Discard columns with too many NAs - more than 50% NA values
loandata <- loandata[, -which(colMeans(is.na(loandata))>0.5)]

dim(loandata)

str(loandata)
# Remove additional columns based on general observation
discard_column <- c("hardship_flag", "hardship_type",
                    "hardship_reason", "hardship_status",
                    "hardship_start_date", "hardship_end_date",
                    "payment_plan_start_date", "hardship_loan_status",
                    "disbursement_method", "dept_settlement_flag",
                    "debt_settlement_flag_date", "settlement_status",
                    "settlement_date", "earliest_cr_line")

loandata <- (loandata[, !(names(loandata) %in% discard_column)])
dim(loandata)

# Convert revol_util to numeric date type
loandata$revol_util <- (as.numeric(gsub(pattern="%", replacement="", x=loandata$int_rate)))

# Review default rate by issued month
loandata$issue_m <- sapply(loandata$issue_d, function(x){str_split(x,"-")[[1]][1]})
loandata %>% group_by(issue_m) %>% summarise(count=n())

# Drop issue dates
loandata$issue_m <- NULL
loandata$issue_d <- NULL
dim(loandata)

# Attributes with zero-variance have less predictive power

# Returns the numeric columns from a dataset
getNumericColumns <- function(t) {
  tn = sapply(t, function(x){ is.numeric(x)})
  return(names(tn)[which(tn)])
}

# Returns the character columns from a dataset
getCharColumns <- function(x) {
  tn = sapply(t, function(x){ is.character(x)})
  return(names(tn)[which(tn)])
}

# Returns the factor columns in a dataset
getFactorColumns <- function(t) {
  tn = sapply(t, function(x) {is.factor(x)})
  return(names(tn)[which(tn)])
}

# Returns index of column along with column names
getIndexOfColumns <- function(t, column_names) {
  return(match(column_names, colnames(t)))
}

# Find character columns with same value and numeric columns with zero-variance
tmp <- apply(loandata[getCharColumns(loandata)], 2, function(x) { length(unique(x))})
tmp <- tmp[tmp==1]
tmp
tmp2 <- apply(loandata[getNumericColumns(loandata)], 2, function(x){(sd(x))})
tmp2 <- tmp2[tmp2==0]
tmp2
discard_column <- c(names(tmp), names(tmp2))
discard_column
loandata <- (loandata[, (!names(loandata) %in% discard_column)])
dim(loandata)

# attributes title and purpose
table(loandata$purpose)
table(loandata$title)
# the variable title and purpose have the same information
# drop the title variable
loandata$title <- NULL
dim(loandata)

str(loandata$desc)
unique(loandata$desc)
loandata$desc <- NULL # this variable is not significant
dim(loandata)

# Default rates by state
tmp <- loandata %>% filter(loan_status=="Default") %>% group_by(addr_state) %>% summarise(default_count=n())
tmp2 <- loandata %>% group_by(addr_state) %>% summarise(count=n())
tmp3 <- tmp2 %>% left_join(tmp) %>% mutate(default_rate = default_count/count)
tmp3 <- data.frame(tmp3)
tmp3
plot(tmp3$default_rate)

# Numeric features
str(loandata[getNumericColumns(loandata)])

# Transform annual_inc, revol_bal, avg_cur_bal, bc_open_to_buy
loandata$annual_inc <- loandata$annual_inc/loandata$funded_amnt
loandata$revol_bal <- loandata$revol_bal/loandata$funded_amnt
loandata$avg_cur_bal <- loandata$avg_cur_bal/loandata$funded_amnt
loandata$bc_open_to_buy <- loandata$bc_open_to_buy/loandata$funded_amnt

# Look at all featrues
str(loandata)

# Remove the verification status joint
unique(loandata$verification_status_joint)
loandata$verification_status_joint <- NULL

# Home ownership variable
unique(loandata$home_ownership)
table(loandata$home_ownership)

# Payment Plan variable
unique(loandata$pymnt_plan)
table(loandata$pymnt_plan)

tmp <- loandata %>% filter(loan_status=="Default") %>% group_by(pymnt_plan) %>% summarise(default_count=n())
tmp2 <- loandata %>% group_by(pymnt_plan) %>% summarise(count=n())
tmp3 <- tmp2 %>% left_join(tmp) %>% mutate(default_rate = default_count/count)
tmp3 # all payment plans defaulted
loandata$pymnt_plan <- NULL # dropping variable
dim(loandata)

# Term variable
unique(loandata$term)
table(loandata$term)

tmp <- loandata %>% filter(loan_status=="Default") %>% group_by(term) %>% summarise(default_count=n())
tmp2 <- loandata %>% group_by(term) %>% summarise(count=n())
tmp3 <- tmp2 %>% left_join(tmp) %>% mutate(default_rate = default_count/count)
tmp3 # 60 months term have a higher default rate than 36 months

loandata$term <- as.factor(loandata$term)

# Interest rate variable
hist(loandata$int_rate)
loandata %>% filter(loan_status=="Fully Paid") %>% summarise(int_rate=mean(int_rate))
loandata %>% filter(loan_status=="Default") %>% summarise(int_rate=mean(int_rate))
# The average interst rate on Defaulted loans are higher than Fully Paid loans

loandata$sub_grade <- as.factor(loandata$sub_grade)

# Employee Length variable
unique(loandata$emp_length)
table(loandata$emp_length)
loandata$emp_length <- as.factor(loandata$emp_length)

unique(loandata$home_ownership)
loandata$home_ownership <- as.factor(loandata$home_ownership)
tmp <- loandata %>% filter(loan_status=="Default") %>% group_by(home_ownership) %>% summarise(default_count=n())
tmp2 <- loandata %>% group_by(home_ownership) %>% summarise(count=n())
tmp3 <- tmp2 %>% left_join(tmp) %>% mutate(default_rate = default_count/count)
tmp3

# Annual Income variable
loandata %>% filter(loan_status=="Fully Paid") %>% summarise(ave_income=mean(annual_inc))
loandata %>% filter(loan_status=="Default") %>% summarise(ave_income=mean(annual_inc))
# The average income on Defaulted loans are lower than Fully Paid loans

# Verification status variable
unique(loandata$verification_status)
loandata$verification_status <- as.factor(loandata$verification_status)
tmp <- loandata %>% filter(loan_status=="Default") %>% group_by(verification_status) %>% summarise(default_count=n())
tmp2 <- loandata %>% group_by(verification_status) %>% summarise(count=n())
tmp3 <- tmp2 %>% left_join(tmp) %>% mutate(default_rate = default_count/count)
tmp3 

# Loan Status variable
unique(loandata$loan_status)
table(loandata$loan_status)

# Purpose variable
unique(loandata$purpose)
table(loandata$purpose)
loandata$purpose <- as.factor(loandata$purpose)

# State variable
unique(loandata$addr_state)
table(loandata$addr_state)
loandata$addr_state <- as.factor(loandata$addr_state)

# Debt-to-income ratio variable
(dti_fp <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_dti=mean(dti, na.rm=T)))
(dti_d <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_dti=mean(dti, na.rm=T)))
# The average DTI is higher for defaulted loans
loandata$dti[is.na(loandata$dti)] <- mean(loandata$dti,na.rm=TRUE)
anyNA(loandata$dti)

# Delinquent variable
unique(loandata$delinq_2yrs)
table(loandata$delinq_2yrs)

# Inquisition variable
unique(loandata$inq_last_6mths)
table(loandata$inq_last_6mths)
(inq1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_inq=mean(inq_last_6mths, na.rm=T)))
(inq2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_inq=mean(inq_last_6mths, na.rm=T)))
# Defaulted loans have a highter number of inquistions in the last 6 months on average
loandata$inq_last_6mths[is.na(loandata$inq_last_6mths)] <- mean(loandata$inq_last_6mths,na.rm=TRUE)
anyNA(loandata$inq_last_6mths)

# Months since last delinq
unique(loandata$mths_since_last_delinq)
(inq1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_mths=mean(mths_since_last_delinq, na.rm=T)))
(inq2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_mths=mean(mths_since_last_delinq, na.rm=T)))
# mths_since_last_delinq attribute is not significant 
loandata$mths_since_last_delinq <- NULL
dim(loandata)

# open_acc variable
unique(loandata$open_acc)
(oa1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_oa=mean(open_acc, na.rm=T)))
(oa2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_oa=mean(open_acc, na.rm=T)))
# open_acc attribute is not significant 
loandata$open_acc <- NULL
dim(loandata)

# pub_rec variable
unique(loandata$pub_rec)
(pr1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_pr=mean(pub_rec, na.rm=T)))
(pr2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_pr=mean(pub_rec, na.rm=T)))

# revol_bal variable
unique(loandata$revol_bal)
anyNA(loandata$revol_bal)
(rb1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_rb=mean(revol_bal, na.rm=T)))
(rb2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_rb=mean(revol_bal, na.rm=T)))

# revol_util variable
unique(loandata$revol_util)
anyNA(loandata$revol_util)
(ru1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_rb=mean(revol_util, na.rm=T)))
(ru2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_rb=mean(revol_util, na.rm=T)))

# total_acc variable
unique(loandata$total_acc)
anyNA(loandata$total_acc)
(ta1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_ta=mean(total_acc, na.rm=T)))
(ta2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_ta=mean(total_acc, na.rm=T)))
# total_acc attribute is not significant 
loandata$total_acc <- NULL
dim(loandata)

# initial_list_status variable
unique(loandata$initial_list_status)
tmp <- loandata %>% filter(loan_status=="Default") %>% group_by(initial_list_status) %>% summarise(default_count=n())
tmp2 <- loandata %>% group_by(initial_list_status) %>% summarise(count=n())
tmp3 <- tmp2 %>% left_join(tmp) %>% mutate(default_rate = default_count/count)
tmp3
loandata$initial_list_status <- as.factor(loandata$initial_list_status)

# collections_12_mths_ex_med variable
unique(loandata$collections_12_mths_ex_med)
anyNA(loandata$collections_12_mths_ex_med)

# application_type variable
unique(loandata$application_type)
tmp <- loandata %>% filter(loan_status=="Default") %>% group_by(application_type) %>% summarise(default_count=n())
tmp2 <- loandata %>% group_by(application_type) %>% summarise(count=n())
tmp3 <- tmp2 %>% left_join(tmp) %>% mutate(default_rate = default_count/count)
tmp3
loandata$application_type <- NULL
dim(loandata)

# acc_now_delinq variable
unique(loandata$acc_now_delinq)
tmp <- loandata %>% filter(loan_status=="Default") %>% group_by(acc_now_delinq) %>% summarise(default_count=n())
tmp2 <- loandata %>% group_by(acc_now_delinq) %>% summarise(count=n())
tmp3 <- tmp2 %>% left_join(tmp) %>% mutate(default_rate = default_count/count)
tmp3
loandata$acc_now_delinq <- NULL # variable is not significant
dim(loandata)

# tot_coll_amt variable
unique(loandata$tot_coll_amt)
anyNA(loandata$tot_coll_amt)
(tca1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_tca=mean(tot_coll_amt, na.rm=T)))
(tca2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_tca=mean(tot_coll_amt, na.rm=T)))

# tot_cur_bal variable
unique(loandata$tot_cur_bal)
anyNA(loandata$tot_cur_bal)
(tcb1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_tcb=mean(tot_cur_bal, na.rm=T)))
(tcb2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_tcb=mean(tot_cur_bal, na.rm=T)))

# il_util variable
unique(loandata$il_util)
anyNA(loandata$il_util)
(iu1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_iu=mean(il_util, na.rm=T)))
(iu2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_iu=mean(il_util, na.rm=T)))
loandata$il_util <- NULL # variable is not significant
dim(loandata)

# open_acc_6m variable
unique(loandata$open_acc_6m)
anyNA(loandata$open_acc_6m)
(oa6m1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_oa=mean(open_acc_6m, na.rm=T)))
(oa6m2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_oa=mean(open_acc_6m, na.rm=T)))
loandata$open_acc_6m[is.na(loandata$open_acc_6m)] <- mean(loandata$open_acc_6m,na.rm=TRUE)
anyNA(loandata$open_acc_6m)

# open_act_il variable
unique(loandata$open_act_il)
anyNA(loandata$open_act_il)
(oa1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_oa=mean(open_act_il, na.rm=T)))
(oa2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_oa=mean(open_act_il, na.rm=T)))
loandata$open_act_il <- NULL # variable is not significant
dim(loandata)

# open_il_12m  and open_il_24m variable
unique(loandata$open_il_12m)
anyNA(loandata$open_il_12m)
(oi1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_oi=mean(open_il_12m, na.rm=T)))
(oi2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_oi=mean(open_il_12m, na.rm=T)))
loandata$open_il_12m[is.na(loandata$open_il_12m)] <- mean(loandata$open_il_12m,na.rm=TRUE)
unique(loandata$open_il_24m)
anyNA(loandata$open_il_24m)
(oi1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_oi=mean(open_il_24m, na.rm=T)))
(oi2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_oi=mean(open_il_24m, na.rm=T)))
loandata$open_il_24m <- NULL # variable is not significant
dim(loandata)

# mths_since_rcnt_il variable
unique(loandata$mths_since_rcnt_il)
anyNA(loandata$mths_since_rcnt_il)
(ri1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_ri=mean(mths_since_rcnt_il, na.rm=T)))
(ri2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_ri=mean(mths_since_rcnt_il, na.rm=T)))
loandata$mths_since_rcnt_il[is.na(loandata$mths_since_rcnt_il)] <- mean(loandata$mths_since_rcnt_il,na.rm=TRUE)

# total_bal_il variable
unique(loandata$total_bal_il)
anyNA(loandata$total_bal_il)
(tbi1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_tbi=mean(total_bal_il, na.rm=T)))
(tbi2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_tbi=mean(total_bal_il, na.rm=T)))
loandata$total_bal_il <- NULL # variable is not significant
dim(loandata)

# open_rv_12m and open_rv_24m variables
unique(loandata$open_rv_12m)
anyNA(loandata$open_rv_12m)
unique(loandata$open_rv_24m)
anyNA(loandata$open_rv_24m)
(or1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_or=mean(open_rv_12m, na.rm=T)))
(or2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_or=mean(open_rv_12m, na.rm=T)))
(or1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_or=mean(open_rv_24m, na.rm=T)))
(or2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_or=mean(open_rv_24m, na.rm=T)))
loandata$open_rv_12m[is.na(loandata$open_rv_12m)] <- mean(loandata$open_rv_12m,na.rm=TRUE)
loandata$open_rv_24m[is.na(loandata$open_rv_24m)] <- mean(loandata$open_rv_24m,na.rm=TRUE)
cor(loandata$open_rv_12m, loandata$open_rv_24m)
loandata$open_rv_24m <- NULL # both variables are highly correlated
dim(loandata)

# max_bal_bc variable
unique(loandata$max_bal_bc)
anyNA(loandata$max_bal_bc)
(mb1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_mb=mean(max_bal_bc, na.rm=T)))
(mb2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_mb=mean(max_bal_bc, na.rm=T)))
loandata$max_bal_bc[is.na(loandata$max_bal_bc)] <- mean(loandata$max_bal_bc,na.rm=TRUE)

# all_util variable
unique(loandata$all_util)
anyNA(loandata$all_util)
(au1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_au=mean(all_util, na.rm=T)))
(au2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_au=mean(all_util, na.rm=T)))
loandata$all_util <- NULL

# total_rev_hi_lim 
unique(loandata$total_rev_hi_lim)
anyNA(loandata$total_rev_hi_lim)
(tr1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_tr=mean(total_rev_hi_lim, na.rm=T)))
(tr2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_tr=mean(total_rev_hi_lim, na.rm=T)))

# inq_fi variable
unique(loandata$inq_fi)
anyNA(loandata$inq_fi)
(if1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_tr=mean(inq_fi, na.rm=T)))
(if2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_tr=mean(inq_fi, na.rm=T)))
loandata$inq_fi[is.na(loandata$inq_fi)] <- mean(loandata$inq_fi,na.rm=TRUE)

# total_cu_tl variable
unique(loandata$total_cu_tl)
anyNA(loandata$total_cu_tl)
(tct1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_tct=mean(total_cu_tl, na.rm=T)))
(tct2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_tct=mean(total_cu_tl, na.rm=T)))
loandata$total_cu_tl <- NULL

# inq_last_12m variable
anyNA(loandata$inq_last_12m)
loandata$inq_last_12m[is.na(loandata$inq_last_12m)] <- mean(loandata$inq_last_12m,na.rm=TRUE)
cor(loandata$inq_last_12m, loandata$inq_last_6m) # Not highly correlated
cor(loandata$inq_fi, loandata$inq_last_6m) # Not highly correlated
cor(loandata$inq_last_12m, loandata$inq_fi) # Not highly correlated

# acc_open_past_24mths variable
unique(loandata$acc_open_past_24mths)
anyNA(loandata$acc_open_past_24mths)
cor(loandata$acc_open_past_24mths, loandata$open_acc_6m) # Not highly correlated
(aop1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_tct=mean(acc_open_past_24mths, na.rm=T)))
(aop2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_tct=mean(acc_open_past_24mths, na.rm=T)))

# avg_cur_bal variable
unique(loandata$avg_cur_bal)
anyNA(loandata$avg_cur_bal)
(acb1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_acb=mean(avg_cur_bal, na.rm=T)))
(acb2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_acb=mean(avg_cur_bal, na.rm=T)))

# bc_open_to_buy variable
unique(loandata$bc_open_to_buy)
anyNA(loandata$bc_open_to_buy)
(botb1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_botb=mean(bc_open_to_buy, na.rm=T)))
(botb2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_botb=mean(bc_open_to_buy, na.rm=T)))
loandata$bc_open_to_buy[is.na(loandata$bc_open_to_buy)] <- mean(loandata$bc_open_to_buy,na.rm=TRUE)

# bc_util variable
unique(loandata$bc_util)
anyNA(loandata$bc_util)
(bu1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_bu=mean(bc_util, na.rm=T)))
(bu2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_bu=mean(bc_util, na.rm=T)))
loandata$bc_util[is.na(loandata$bc_util)] <- mean(loandata$bc_util,na.rm=TRUE)

# chargeoff_within_12_mths variable
unique(loandata$chargeoff_within_12_mths)
anyNA(loandata$chargeoff_within_12_mths)
(co1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_co=mean(chargeoff_within_12_mths, na.rm=T)))
(co2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_co=mean(chargeoff_within_12_mths, na.rm=T)))

# delinq_amnt variable
unique(loandata$delinq_amnt)
anyNA(loandata$delinq_amnt)
(dav1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_dav=mean(delinq_amnt, na.rm=T)))
(dav2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_dav=mean(delinq_amnt, na.rm=T)))

# mo_sin_old_il_acct variable
unique(loandata$mo_sin_old_il_acct)
anyNA(loandata$mo_sin_old_il_acct)
(mos1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_mos=mean(mo_sin_old_il_acct, na.rm=T)))
(mos2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_mos=mean(mo_sin_old_il_acct, na.rm=T)))
loandata$mo_sin_old_il_acct <- NULL

# mo_sin_old_rev_tl_op variable 
unique(loandata$mo_sin_old_rev_tl_op)
anyNA(loandata$mo_sin_old_rev_tl_op)
(ms1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_ms=mean(mo_sin_old_rev_tl_op, na.rm=T)))
(ms2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_ms=mean(mo_sin_old_rev_tl_op, na.rm=T)))
loandata$mo_sin_old_rev_tl_op <- NULL

# mo_sin_rcnt_rev_tl_op variable
unique(loandata$mo_sin_rcnt_rev_tl_op)
anyNA(loandata$mo_sin_rcnt_rev_tl_op)
(msr1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_msr=mean(mo_sin_rcnt_rev_tl_op, na.rm=T)))
(msr2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_msr=mean(mo_sin_rcnt_rev_tl_op, na.rm=T)))
cor(loandata$mths_since_rcnt_il, loandata$mo_sin_rcnt_rev_tl_op)

# mo_sin_rcnt_tl variable
unique(loandata$mo_sin_rcnt_rev_tl_op)
anyNA(loandata$mo_sin_rcnt_rev_tl_op)
cor(loandata$mo_sin_rcnt_tl, loandata$mo_sin_rcnt_rev_tl_op)
(msr1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_msr=mean(mo_sin_rcnt_tl, na.rm=T)))
(msr2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_msr=mean(mo_sin_rcnt_tl, na.rm=T)))

# mort_acc variable
unique(loandata$mort_acc)
anyNA(loandata$mort_acc)
(ma1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_ma=mean(mort_acc, na.rm=T)))
(ma2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_ma=mean(mort_acc, na.rm=T)))

# mths_since_recent_bc variable
unique(loandata$mths_since_recent_bc)
anyNA(loandata$mths_since_recent_bc)
(msr1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_msr=mean(mths_since_recent_bc, na.rm=T)))
(msr2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_msr=mean(mths_since_recent_bc, na.rm=T)))
loandata$mths_since_recent_bc[is.na(loandata$mths_since_recent_bc)] <- mean(loandata$mths_since_recent_bc,na.rm=TRUE)

# mths_since_recent_inq variable
unique(loandata$mths_since_recent_inq)
anyNA(loandata$mths_since_recent_inq)
(msr1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_msr=mean(mths_since_recent_inq, na.rm=T)))
(msr2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_msr=mean(mths_since_recent_inq, na.rm=T)))
loandata$mths_since_recent_inq[is.na(loandata$mths_since_recent_inq)] <- mean(loandata$mths_since_recent_inq,na.rm=TRUE)

# num_accts_ever_120_pd variable
unique(loandata$num_accts_ever_120_pd)
anyNA(loandata$num_accts_ever_120_pd)
(var1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_var=mean(num_accts_ever_120_pd, na.rm=T)))
(var2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_var=mean(num_accts_ever_120_pd, na.rm=T)))
loandata$num_accts_ever_120_pd <- NULL
dim(loandata)

# num_actv_bc_tl
unique(loandata$num_actv_bc_tl)
anyNA(loandata$num_actv_bc_tl)
(var1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_var=mean(num_actv_bc_tl, na.rm=T)))
(var2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_var=mean(num_actv_bc_tl, na.rm=T)))
loandata$num_actv_bc_tl <- NULL
dim(loandata)

# num_actv_rev_tl variable
unique(loandata$num_actv_rev_tl)
anyNA(loandata$num_actv_rev_tl)
(var1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_var=mean(num_actv_rev_tl, na.rm=T)))
(var2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_var=mean(num_actv_rev_tl, na.rm=T)))

# num_bc_sats variable
unique(loandata$num_bc_sats)
anyNA(loandata$num_bc_sats)
(var1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_var=mean(num_bc_sats, na.rm=T)))
(var2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_var=mean(num_bc_sats, na.rm=T)))
loandata$num_bc_sats <- NULL
dim(loandata)

# num_bc_tl variable
unique(loandata$num_bc_tl)
anyNA(loandata$num_bc_tl)
(var1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_var=mean(num_bc_tl, na.rm=T)))
(var2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_var=mean(num_bc_tl, na.rm=T)))
loandata$num_bc_tl <- NULL
dim(loandata)

# num_il_tl variable
unique(loandata$num_il_tl)
anyNA(loandata$num_il_tl)
(var1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_var=mean(num_il_tl, na.rm=T)))
(var2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_var=mean(num_il_tl, na.rm=T)))
loandata$num_bc_tl <- NULL
dim(loandata)

# num_op_rev_tl variable
unique(loandata$num_op_rev_tl)
anyNA(loandata$num_op_rev_tl)
(var1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_var=mean(num_op_rev_tl, na.rm=T)))
(var2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_var=mean(num_op_rev_tl, na.rm=T)))
loandata$num_op_rev_tl <- NULL
dim(loandata)

# num_rev_accts variable
unique(loandata$num_rev_accts)
anyNA(loandata$num_rev_accts)
(var1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_var=mean(num_rev_accts, na.rm=T)))
(var2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_var=mean(num_rev_accts, na.rm=T)))
loandata$num_rev_accts <- NULL
dim(loandata)

# num_rev_tl_bal_gt_0 variable
unique(loandata$num_rev_tl_bal_gt_0)
anyNA(loandata$num_rev_tl_bal_gt_0)
(var1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_var=mean(num_rev_tl_bal_gt_0, na.rm=T)))
(var2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_var=mean(num_rev_tl_bal_gt_0, na.rm=T)))

# num_sats variable
unique(loandata$num_sats)
anyNA(loandata$num_sats)
(var1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_var=mean(num_sats, na.rm=T)))
(var2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_var=mean(num_sats, na.rm=T)))
loandata$num_sats <- NULL
dim(loandata)

# num_tl_90g_dpd_24m variable
unique(loandata$num_tl_90g_dpd_24m)
anyNA(loandata$num_tl_90g_dpd_24m)
(var1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_var=mean(num_tl_90g_dpd_24m, na.rm=T)))
(var2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_var=mean(num_tl_90g_dpd_24m, na.rm=T)))

# num_tl_op_past_12m variable
unique(loandata$num_tl_op_past_12m)
anyNA(loandata$num_tl_op_past_12m)
(var1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_var=mean(num_tl_op_past_12m, na.rm=T)))
(var2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_var=mean(num_tl_op_past_12m, na.rm=T)))

# pct_tl_nvr_dlq variable
unique(loandata$pct_tl_nvr_dlq)
anyNA(loandata$pct_tl_nvr_dlq)
(var1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_var=mean(pct_tl_nvr_dlq, na.rm=T)))
(var2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_var=mean(pct_tl_nvr_dlq, na.rm=T)))
loandata$pct_tl_nvr_dlq <- NULL
dim(loandata)

# percent_bc_gt_75 variable
unique(loandata$percent_bc_gt_75)
anyNA(loandata$percent_bc_gt_75)
(var1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_var=mean(percent_bc_gt_75, na.rm=T)))
(var2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_var=mean(percent_bc_gt_75, na.rm=T)))
loandata$percent_bc_gt_75[is.na(loandata$percent_bc_gt_75)] <- mean(loandata$percent_bc_gt_75,na.rm=TRUE)

# pub_rec_bankruptcies variable
unique(loandata$pub_rec_bankruptcies)
anyNA(loandata$pub_rec_bankruptcies)
(var1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_var=mean(pub_rec_bankruptcies, na.rm=T)))
(var2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_var=mean(pub_rec_bankruptcies, na.rm=T)))

# tax_liens variable
unique(loandata$tax_liens)
anyNA(loandata$tax_liens)
(var1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_var=mean(tax_liens, na.rm=T)))
(var2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_var=mean(tax_liens, na.rm=T)))
loandata$tax_liens[is.na(loandata$tax_liens)] <- mean(loandata$tax_liens,na.rm=TRUE)

# tot_hi_cred_lim variable
unique(loandata$tot_hi_cred_lim)
anyNA(loandata$tot_hi_cred_lim)
(var1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_var=mean(tot_hi_cred_lim, na.rm=T)))
(var2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_var=mean(tot_hi_cred_lim, na.rm=T)))

# total_bal_ex_mort variable
unique(loandata$total_bal_ex_mort)
anyNA(loandata$total_bal_ex_mort)
(var1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_var=mean(total_bal_ex_mort, na.rm=T)))
(var2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_var=mean(total_bal_ex_mort, na.rm=T)))
loandata$total_bal_ex_mort <- NULL
dim(loandata)

# total_bc_limit variable
unique(loandata$total_bc_limit)
anyNA(loandata$total_bc_limit)
(var1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_var=mean(total_bc_limit, na.rm=T)))
(var2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_var=mean(total_bc_limit, na.rm=T)))

# total_il_high_credit_limit variable
unique(loandata$total_il_high_credit_limit)
anyNA(loandata$total_il_high_credit_limit)
(var1 <- loandata %>% filter(loan_status=="Fully Paid") %>% summarise(avg_var=mean(total_il_high_credit_limit, na.rm=T)))
(var2 <- loandata %>% filter(loan_status=="Default") %>% summarise(avg_var=mean(total_il_high_credit_limit, na.rm=T)))
loandata$total_il_high_credit_limit <- NULL
dim(loandata)

# debt_settlement_flag variable
unique(loandata$debt_settlement_flag)
anyNA(loandata$debt_settlement_flag)
tmp <- loandata %>% filter(loan_status=="Default") %>% group_by(debt_settlement_flag) %>% summarise(default_count=n())
tmp2 <- loandata %>% group_by(debt_settlement_flag) %>% summarise(count=n())
tmp3 <- tmp2 %>% left_join(tmp) %>% mutate(default_rate = default_count/count)
tmp3
loandata$debt_settlement_flag <- NULL

dim(loandata)

# Investigate Correlations
library(corrplot)
corrplot(cor(loandata[getNumericColumns(loandata)], use="na.or.complete"))

cor(loandata$revol_util, loandata$int_rate)
loandata$revol_util <- NULL

cor(loandata$tot_hi_cred_li, loandata$tot_cur_bal)
loandata$tot_hi_cred_li <- NULL

loandata$num_rev_tl_bal_gt_0 <- NULL

# Convert loan status to values 0 or 1: Default=0 and Fully Paid=1
loandata$loan_status <- ifelse(str_detect(loandata$loan_status, "Paid"), 1, 0)

anyNA(loandata$recoveries)

defaults <- loandata %>% filter(loan_status==0)
# recovery_rate = recoveries / funded_amnt
defaults <- mutate(defaults, recovery_rate=recoveries / funded_amnt)
min(defaults$recovery_rate)
max(defaults$recovery_rate)
defaults$recovery_rate <- ifelse(defaults$recovery_rate>1, 1, defaults$recovery_rate)

# CCF = (funded_amnt - total_rec_prncp) / funded_amnt
defaults <- mutate(defaults, CCF=(funded_amnt - total_rec_prncp) / funded_amnt)
min(defaults$CCF)
max(defaults$CCF)

# Feature selection using Information Value
Information::create_infotables(data=loandata, y="loan_status", parallel=FALSE)
pred.var <- c("sub_grade", "int_rate", "dti", "term")

# PD Model (Probability of Default)
split = sample.split(loandata$loan_status, SplitRatio=0.7)
PD_train = subset(loandata, split == TRUE) #Train
PD_test = subset(loandata, split == FALSE) #Test
dim(PD_train)
dim(PD_test)
PD.model <- glm(loan_status~sub_grade+int_rate+dti+term, data=PD_train)
summary(PD.model)
PD.pred <- predict(PD.model, newdata=PD_test)
Pred_loan_status <- ifelse(PD.pred>0.62, 1, 0)
confusion_matrix <- table(Pred_loan_status, PD_test$loan_status)
(confusion_matrix[1]+confusion_matrix[4])/sum(confusion_matrix) # accuracy

# LGD Model (Loss Given Default)
# Train-test split
split = sample.split(defaults$recovery_rate, SplitRatio=0.7)
LGD_train = subset(defaults, split == TRUE) #Train
LGD_test = subset(defaults, split == FALSE) #Test
dim(LGD_train)
dim(LGD_test)
LGD.model <- lm(recovery_rate~sub_grade+int_rate+dti+term, data=LGD_train)
summary(LGD.model)
df_e <- data.frame(resid=residuals(LGD.model), pred=predict(LGD.model,newdata=LGD_train))
ggplot(df_e, aes(pred, abs(resid)))+geom_point()+geom_smooth()
sqrt(mean(LGD_train$recovery_rate-df_e$pred)^2) #rmse on train data
hist(df_e$resid)

LGD.pred <- predict(LGD.model, newdata=LGD_test)
df_e <- data.frame(resid=LGD_test$recovery_rate-LGD.pred, pred=LGD.pred)
ggplot(df_e, aes(pred, abs(resid)))+geom_point()+geom_smooth()
sqrt(mean(LGD_test$recovery_rate-LGD.pred)^2) #rmse on test data
hist(df_e$resid)

# EAD Model (Exposure at Default)
# Train-test split
split = sample.split(defaults$CCF, SplitRatio=0.7)
EAD_train = subset(defaults, split == TRUE) #Train
EAD_test = subset(defaults, split == FALSE) #Test
dim(EAD_train)
dim(EAD_test)
EAD.model <- lm(CCF~sub_grade+int_rate+dti+term, data=EAD_train)
summary(EAD.model)
df_e <- data.frame(resid=residuals(EAD.model), pred=predict(EAD.model,newdata=EAD_train))
ggplot(df_e, aes(pred, abs(resid)))+geom_point()+geom_smooth()
sqrt(mean(EAD_train$CCF-df_e$pred)^2) #rmse on train data
hist(df_e$resid)

EAD.pred <- predict(EAD.model, newdata=EAD_test)
df_e <- data.frame(resid=EAD_test$CCF-EAD.pred, pred=EAD.pred)
ggplot(df_e, aes(pred, abs(resid)))+geom_point()+geom_smooth()
sqrt(mean(EAD_test$CCF-EAD.pred)^2) #rmse on test data
hist(df_e$resid)

# Expected Loss
variables <- c("sub_grade", "int_rate", "dti", "term", "funded_amnt")
x.data <- loandata[variables]
y.data <- loandata$loan_status
recovery_rate <- predict(LGD.model, x.data)
x.data$recovery_rate <- recovery_rate
ccf <- predict(EAD.model, x.data)
x.data$ccf <- ccf
x.data <- x.data %>% mutate(x.data, LGD=1-recovery_rate)
x.data <- x.data %>% mutate(x.data, EAD=ccf*funded_amnt)
PD <- predict(PD.model, x.data)
PD <- ifelse(PD>0.62, 0, 1)
x.data$PD <- PD
x.data <- x.data %>% mutate(x.data, EL=PD*LGD*EAD)

sum(x.data$EL) # Total Expected Loss for all loans
sum(x.data$EL)/sum(x.data$funded_amnt) # % of EL on all loans