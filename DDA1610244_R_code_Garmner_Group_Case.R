
# Checkpoint-1: Data Cleaning and preparation-----------------------------------------

#	Load the file loan.csv into data frame loan
loan <- read.csv("loan.csv",header = T,stringsAsFactors = F)
View(loan)

#Impute the NA values for all driver variables
#Checking of NA values for all driver variables 
#1. annual_inc 
length(which(is.na(loan$annual_inc)==T))

#boxplot(loan$annual_inc)
#boxplot.stats(loan$annual_inc)

# As this has many outliers so we will replace this 4 rows with Median values 
#Storing the Row number having NA values in annual_inc column 
rowno_loan_na <- which(is.na(loan$annual_inc)==T)

#Finding the median of non NA values in annual_inc column 
loan_non_na <- subset(loan,is.na(loan$annual_inc)==F)
median_loan_non_na  <-  median(loan_non_na$annual_inc)

#Replacing the NA values with median 
loan$annual_inc[rowno_loan_na]<- median_loan_non_na

#2.loan_amnt 
length(which(is.na(loan$loan_amnt)==T))

#---- # Remove rows with loan_status  = "Fully Paid"

loan$loan_status <- gsub("Does not meet the credit policy. Status:Fully Paid","Fully Paid",loan$loan_status)
loan$loan_status <- gsub("Does not meet the credit policy. Status:Charged Off","Charged Off",loan$loan_status)


loan <- subset(loan, loan$loan_status!="Fully Paid")
#32950
length(which(loan$loan_status=="Fully Paid"))

# Emp_length has n/a values so we have to remove that 
loan_emp_len_na <- which(loan$emp_length == "n/a")
loan <- subset(loan, loan$emp_length != "n/a")
#9278
#7309

#Create a new column named loan_status_1  with three levels current_new, default_new and late such that
#rows with loan_status as "current" or "in grace period" are tagged as  "current_new"
#rows with loan_status as " default" or "charged off" are tagged as "default_new"
#rows with loan_status as " late 16- 30 days" "late 31-120 days" are tagged as "late"

unique(loan$loan_status)
loan$loan_status <-trimws(loan$loan_status)

for (i in 1:nrow(loan)) {
  if (loan$loan_status[i]=="Current" | loan$loan_status[i]=="In Grace Period")
  {
    loan$loan_status_1[i] <- "current_new"
  }
  else if (loan$loan_status[i]=="Default" |loan$loan_status[i]=="Charged Off")
  {
    loan$loan_status_1[i] <- "default_new"
  }
  else if (loan$loan_status[i]=="Late (16-30 days)" |loan$loan_status[i]=="Late (31-120 days)")
  {
    loan$loan_status_1[i] <- "late"
  }
  
}

# .	Create new bin variables for int_rate and emp_length respectively as follows:
#Create int_rate_grp such that int_rate < 10 is tagged "Low"; int_rate (10-18) 
#is tagged "Medium"; int_rate (>18) is tagged "High"

class(loan$int_rate)
#character

loan$int_rate <- gsub("%","",loan$int_rate)
loan$int_rate <- trimws(loan$int_rate)
loan$int_rate <- as.numeric(loan$int_rate)



length(which(loan$int_rate_grp == "Medium"))
#7065
length(which(loan$int_rate < 10))
#0
length(which(loan$int_rate_grp=="High"))
#2213

for (x  in 1:nrow(loan)) {
  
  if (loan$int_rate[x] < 10)
  {
    loan$int_rate_grp[x] <- "Low" 
  }
  else if (loan$int_rate[x] >= 10 & loan$int_rate[x] <=18)
  {
    loan$int_rate_grp[x] <- "Medium"
  }
  else 
    {loan$int_rate_grp[x] <- "High"}
    
  }



#Create emp_len_grp such that emp_length (0-4) is tagged as "Junior";
#emp_length (5-8) is tagged as "Mid-level"; emp_length (>8) is tagged as "Senior".

unique(loan$emp_length)

for (y in 1:nrow(loan)) {
  if (loan$emp_length[y]=="< 1 year" | loan$emp_length[y]== "1 year" | loan$emp_length[y]== "4 years"|loan$emp_length[y]== "3 years"|loan$emp_length[y]== "2 years")
  {
    loan$emp_len_grp[y] <- "Junior"
    
  }
  
  else if (loan$emp_length[y] == "5 years" | loan$emp_length[y] =="6 years"| loan$emp_length[y] == "7 years"|loan$emp_length[y] == "8 years")
  {
    loan$emp_len_grp[y] <- "Mid-Level"
    
  }
  else 
  {
    loan$emp_len_grp[y] <- "Senior"
    
  }
  
}

#################################Checkpoint2##################################################
# START OF --- Checkpoint-2: Exploratory Data Analysis ---------------


# Univariate Analysis (for all drivers)

summary(loan$annual_inc)
summary(loan$funded_amnt)
summary(loan$loan_amnt)
summary(loan$int_rate)
summary(loan$grade)
summary(loan$dti)
summary(loan$emp_length)
summary(loan$purpose)
summary(loan$home_ownership)
summary(loan$loan_status)



# Distribution plot (Histogram)

library(ggplot2)

# plot annual_inc
plot_annual_inc <- ggplot(loan, aes(x=annual_inc)) + geom_histogram(bins=30)
plot_annual_inc

plot_funded_amt <- ggplot(loan, aes(x=funded_amnt)) +labs(x="Funded amount", y="Count") + geom_histogram(bins=30)
plot_funded_amt

plot_loan_amt <- ggplot(loan, aes(x=loan_amnt)) +labs(x="Loan amount", y="Count") + geom_histogram(bins=30)
plot_loan_amt

plot_loan_dti <- ggplot(loan, aes(x=dti)) +labs(x="Loan dti", y="Count") + geom_histogram(bins=30)
plot_loan_dti

#Outlier treatment (boxplot) OR IQR (hinge)

# Check outliers

boxplot(loan$funded_amnt, loan$loan_amnt)

boxplot(loan$funded_amnt, loan$loan_amnt)

boxplot(loan$funded_amnt, loan$loan_amnt, loan$annual_inc)


boxplot(loan$funded_amnt,horizontal = T, boxwex=0.5)

#Get min value for outliers
outliers_funded_amt <- boxplot.stats(loan$funded_amnt)$out
summary(outliers_funded_amt)

outliers_loan_amt <- boxplot.stats(loan$loan_amnt)$out
#check for outliers in dti attribute
boxplot(loan$dti,horizontal = T, boxwex=0.5)

#treat outliersby eliminating obs > min value for outliers (i.e. 33600)
loan <- subset(loan,loan$funded_amnt<=33600)



#write.csv(loan,"loan_chck1.csv",row.names = F)
  
str(loan$loan_status_1)

summary(loan$emp_len_grp)
library(moments)
skewness(loan$annual_inc)
#6.3621

skewness(loan$loan_amnt)
#0.8257

skewness(loan$funded_amnt)
#0.849
skewness(loan$dti)
#-0.149


#Multi variate Analysis 
# 	Finding correlations for all different pairs of continuous variables

Loan <- data.frame(loan$annual_inc,loan$loan_amnt,loan$funded_amnt,loan$dti)

colnames(Loan) <- c("annual_inc","loan_amnt","funded_amnt","dti")

cor(Loan)

library(GGally)
#ggpairs(Loan)



# 3.	loan_status_1: Make plots to show how the continuous variables vary across the three levels
#of loan_status_1; 
#for e.g. how annual_inc is distributed across the levelsdefault_new, late and current_new.

# annual_inc  vs loan_status_1

library(ggplot2)
plot_1 <- ggplot(loan ,aes(x= annual_inc,fill=loan_status_1))
plot_1 + geom_histogram(bins = 30)

# loan_status_1 vs loan_amnt
plot_2 <- ggplot(loan ,aes(x= loan_amnt,fill=loan_status_1))
plot_2 + geom_histogram(bins = 30)

# loan_status_1 vs funded_amnt
plot_3 <- ggplot(loan ,aes(x= funded_amnt,fill=loan_status_1))
plot_3 + geom_histogram(bins = 30)


# loan_status_1 vs dti
plot_4 <- ggplot(loan ,aes(x = dti,fill=loan_status_1)) 
plot_4 + geom_histogram(bins = 30)

#4.	 int_rate_grp: Make plots to show how the continuous variables vary across the three levels
#of int_rate_grp; for e.g. how annual_inc is distributed across the levels Low, Medium and High


int_plot_1<-ggplot(loan,aes(x=loan_amnt,fill=int_rate_grp)) + geom_histogram(bins = 30)
int_plot_1

int_plot_2<-ggplot(loan,aes(x=funded_amnt,fill=int_rate_grp)) + geom_histogram(bins = 30)
int_plot_2

int_plot_3<-ggplot(loan,aes(x=dti,fill=int_rate_grp)) + geom_histogram(bins = 30)
int_plot_3

int_plot_4<-ggplot(loan,aes(x=annual_inc,fill=int_rate_grp)) + geom_histogram(bins = 30)
int_plot_4

#Hypothesis testing

# Considering two levels of loan status i.e. default_new & current_new
loan_hypo_1<-subset(loan,loan$loan_status_1=='default_new')
mean(loan_hypo_1$loan_amnt)
mean(loan_hypo_1$funded_amnt)
mean(loan_hypo_1$dti)
mean(loan_hypo_1$annual_inc)

loan_hypo_2<-subset(loan,loan$loan_status_1=='current_new')

mean(loan_hypo_2$loan_amnt)
mean(loan_hypo_2$funded_amnt)
mean(loan_hypo_2$dti)
mean(loan_hypo_2$annual_inc)


# H0 = mean of continious variable are not equal for both the status(default_new,current_new)
# means continious variable are not significant contributor to the loan default 

# H1 = mean of continious variable are  equal for both the status(default_new,current_new)
# means continious variable are significant contributor to the loan default


#Considering two levels of int_rate_grp i.e. High & Low
loan_hypo_3<-subset(loan,loan$int_rate_grp=='High')
loan_hypo_4<-subset(loan,loan$int_rate_grp=='Low')

#Welch Two Sample t-test of loan_hypo_1 & loan_hypo_2 for each of the continous driver variables
test_1<-t.test(loan_hypo_1$loan_amnt,loan_hypo_2$loan_amnt,conf.level = 0.95,alternative = "two.sided",paired=FALSE)
test_1
 

test_2<-t.test(loan_hypo_1$funded_amnt,loan_hypo_2$funded_amnt,conf.level = 0.95,alternative = "two.sided",paired=FALSE)
test_2

test_3<-t.test(loan_hypo_1$annual_inc,loan_hypo_2$annual_inc,conf.level = 0.95,alternative = "two.sided",paired=FALSE)
test_3

test_4<-t.test(loan_hypo_1$dti,loan_hypo_2$dti,conf.level = 0.95,alternative = "two.sided",paired=FALSE)
test_4

############ int_rate_grp##############
# H0 = mean of continious variable are not equal for both the INTEREST GROUP ( HIGH , LOW)
# i.e continious variable are not significant contributor to the loan default 

# H1 = REJECTED NULL HYPOTHESIS SINCE P VALUE IS VERY LOW 
# i.e continious variable are significant contributor to the loan default   


#Welch Two Sample t-test of loan_hypo_3 & loan_hypo_4 for each of the continous driver variables
test_5<-t.test(loan_hypo_3$loan_amnt,loan_hypo_4$loan_amnt,conf.level = 0.95,alternative = "two.sided",paired=FALSE)
test_5

test_6<-t.test(loan_hypo_3$funded_amnt,loan_hypo_4$funded_amnt,conf.level = 0.95,alternative = "two.sided",paired=FALSE)
test_6

test_7<-t.test(loan_hypo_3$annual_inc,loan_hypo_4$annual_inc,conf.level = 0.95,alternative = "two.sided",paired=FALSE)
test_7

test_8<-t.test(loan_hypo_3$dti,loan_hypo_4$dti,conf.level = 0.95,alternative = "two.sided",paired=FALSE)
test_8

# Removing the extra columns from the loan dataset for visualization in Tableau
loan_tableau <- data.frame(loan$member_id,loan$annual_inc,loan$loan_amnt,loan$funded_amnt,loan$int_rate,
                     loan$grade,loan$dti,loan$emp_length,loan$purpose,
                     loan$home_ownership,loan$loan_status_1,
                     loan$emp_len_grp,loan$int_rate_grp)
