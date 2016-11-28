#############################################ASSUMPTIONS########################################################

#1.Eliminated all the records where status=closed

#2.Eliminated records where raised_amount_usd=NA

#3.Eliminated rows where funding round type are as follows: 
#"undisclosed","convertible_note","debt_financing","grant","equity_crowdfunding", 
#"post_ipo_equity","post_ipo_debt","product_crowdfunding","secondary_market","non_equity_assistance" 

#4.Country code downloaded from www.webservicex.net/country.asmx

#5.Top 3 English speaking countries identified from the pdf provided

##########################################################################################################

#Checkpoint 1: Data Cleaning 1
#---------------------------------------------------------------------------------------------------------
#Loading companies.txt into companies dataframe
companies<-read.delim("companies.txt",header=T,sep="\t",stringsAsFactors = F)
View(companies)
# 66368 records with 10 variables

#Loading rounds2.csv into rounds2 dataframe
rounds2<-read.csv("rounds2.csv",header=T,stringsAsFactors = F)
View(rounds2)
# 114949 records with 6 variables

#Converting the common column (company_permalink,permalink) to lower case as R is case sensitive
rounds2$company_permalink<-tolower(rounds2$company_permalink)
companies$permalink<-tolower(companies$permalink)

#Unique companies in rounds2
unique_companies_rounds2<-unique(rounds2$company_permalink)
length(unique_companies_rounds2)
#66368 unique companies in rounds2


#Unique companies in companies
unique_companies<-unique(companies$permalink)
length(unique_companies)
# All 66368 records have unique companies in companies

#Removal of companies whose status is closed
companies_closed<-subset(companies,companies$status=="closed")
length(unique(companies_closed$name))
#6234 companies have closed status
companies<-subset(companies,companies$status!="closed")
#60130 records
length(unique(companies$name))
#59921 companies have non-closed status

#merging of companies & rounds2 dataframe
master_frame<-merge(companies,rounds2,by.x ="permalink",by.y="company_permalink",all=T)
View(master_frame)
#114949 records
#---------------------------------------------------------------------------------------------------------

#Checkpoint 2:Data Cleaning 2
#---------------------------------------------------------------------------------------------------------  
master_frame_not_na<-subset(master_frame,!is.na(master_frame$raised_amount_usd))
#94959 records


master_frame_na<-subset(master_frame,is.na(master_frame$raised_amount_usd))
#19990 records

#Removing NA values in raised_amount_usd column
master_frame<-subset(master_frame,!is.na(master_frame$raised_amount_usd))
#94959 records

#Cross-check whether any NA records are available in raised_amount_usd column in master_frame
sum(is.na(master_frame$raised_amount_usd))
#---------------------------------------------------------------------------------------------------------

#Checkpoint 3:Funding Type Analysis
#---------------------------------------------------------------------------------------------------------
#Names of all the unique round types
unique_round_types<-unique(master_frame$funding_round_type)
#14 unique round types

#Creating a dataset containing the records of the 4 round types(venture,angel,seed,private_equity)
master_frame_round_type<-subset(master_frame,master_frame$funding_round_type=="venture" | master_frame$funding_round_type=="angel"|master_frame$funding_round_type=="seed" |master_frame$funding_round_type=="private_equity")
#80627 records


#Average funding amount of the 4 round types(venture,angel,seed,private_equity)
master_frame_round_type_average<-aggregate(raised_amount_usd~funding_round_type,data=master_frame_round_type,mean)
#Renaming the 2nd column
names(master_frame_round_type_average)[2]<-"average_raised_amount_usd"

#Dataset of venture funding round type
master_frame_venture<-subset(master_frame_round_type,master_frame_round_type$funding_round_type=="venture")
#50228 records
#--------------------------------------------------------------------------------------------------------
#Checkpoint 4:Country Analysis
#--------------------------------------------------------------------------------------------------------  
#Determination of top 9 countries in venture round type on the basis of total funding amount
venture_countries<-aggregate(raised_amount_usd~country_code,data=master_frame_venture,sum)

#Renaming the 2nd column of venture_countries dataframe
names(venture_countries)[2]<-"total_raised_amount_usd"

venture_countries<- subset(venture_countries,venture_countries$country_code!="")
venture_countries<- subset(venture_countries,!is.na(venture_countries$country_code))
top9<-head(venture_countries[order(venture_countries$total_raised_amount_usd, decreasing= T),], n =9)
View(top9)
#Top 3 English speaking countries
top_3<-top9[c(1,3:4),]

#Dataset of venture funding round type in top 3 english speaking countries
master_frame_venture<-subset(master_frame_venture,master_frame_venture$country_code==top_3$country_code[1] | master_frame_venture$country_code==top_3$country_code[2] | master_frame_venture$country_code==top_3$country_code[3]) 
#36446 records
#Cross check whether the top 3 english speaking countries are in the master_frame_venture dataframe
unique(master_frame_venture$country_code)
#--------------------------------------------------------------------------------------------------------
#Checkpoint 5 : Sector Analysis 1
#--------------------------------------------------------------------------------------------------------  
#Removing delimiters & the elements after the first delimiter
master_frame_venture$category_list<- c(gsub("\\|.*","",master_frame_venture$category_list))


View(master_frame_venture)
#36446 records

length(unique(master_frame_venture$category_list))  
#552 records

#Removal of NA values from category_list column
master_frame_venture<-subset(master_frame_venture,!is.na(master_frame_venture$category_list))
#36446 records
length(unique(master_frame_venture$category_list))
#552 records

#Checking of blank values in category_list column
blank_category_list<-subset(master_frame_venture,master_frame_venture$category_list=="")
#139 records

#Removing blank values in category_list column
master_frame_venture<-subset(master_frame_venture,master_frame_venture$category_list!="")
#36307 records

length(unique(master_frame_venture$category_list))
#551 records

#Loading mapping_file
mapping_file<-read.csv("mapping_file.csv",header=T,stringsAsFactors = F)
View(mapping_file)
#688 records

#Checking of NA & blank values in mapping_file & removing them
mapping_file<-subset(mapping_file,mapping_file$category_list!="")
#687 records
mapping_file<-subset(mapping_file,!is.na(mapping_file$category_list))
#687 records
mapping_file<-subset(mapping_file,mapping_file$main_sector!="")
#687 records
mapping_file<-subset(mapping_file,!is.na(mapping_file$main_sector))
#687 records

#Merging of master_frame_venture & mapping_file
master_frame_venture_merge<-merge(master_frame_venture,mapping_file,by.x="category_list")
#36300 records
length(unique(master_frame_venture_merge$category_list))
#545 records
#--------------------------------------------------------------------------------------------------------
  
#Checkpoint 6: Sector Analysis 2
#--------------------------------------------------------------------------------------------------------
#Dividing the master_frame_merge dataset into subsets according to top 3 English speaking countries  
master_frame_venture_merge_US<-subset(master_frame_venture_merge,master_frame_venture_merge$country_code=="USA")
master_frame_venture_merge_US<-subset(master_frame_venture_merge_US,master_frame_venture_merge_US$raised_amount_usd>=5000000 & master_frame_venture_merge_US$raised_amount_usd<=15000000)
View(master_frame_venture_merge_US)
#11286 records

master_frame_venture_merge_GBR<-subset(master_frame_venture_merge,master_frame_venture_merge$country_code=="GBR")
master_frame_venture_merge_GBR<-subset(master_frame_venture_merge_GBR,master_frame_venture_merge_GBR$raised_amount_usd>=5000000 & master_frame_venture_merge_GBR$raised_amount_usd<=15000000)
View(master_frame_venture_merge_GBR)
#582 records


master_frame_venture_merge_IND<-subset(master_frame_venture_merge,master_frame_venture_merge$country_code=="IND")
master_frame_venture_merge_IND<-subset(master_frame_venture_merge_IND,master_frame_venture_merge_IND$raised_amount_usd>=5000000 & master_frame_venture_merge_IND$raised_amount_usd<=15000000)
View(master_frame_venture_merge_IND)
#315 records

# Creating a column containing the sum of investments in each main sector
investments_US<-aggregate(raised_amount_usd~main_sector,data=master_frame_venture_merge_US,sum)
names(investments_US)<-c("main_sector","sum_investments_USA")
nrow(investments_US)

investments_GBR<-aggregate(raised_amount_usd~main_sector,data=master_frame_venture_merge_GBR,sum)
names(investments_GBR)<-c("main_sector","sum_investments_GBR")
nrow(investments_GBR)

investments_IND<-aggregate(raised_amount_usd~main_sector,data=master_frame_venture_merge_IND,sum)
names(investments_IND)<-c("main_sector","sum_investments_IND")
nrow(investments_IND)

# Creating a column containing the count of investments in each main sector
#install.packages("plyr")
library(plyr)
count_investments_USA<-count(master_frame_venture_merge_US,"main_sector")
names(count_investments_USA)<-c("main_sector","count_investments_USA")

count_investments_GBR<-count(master_frame_venture_merge_GBR,"main_sector")
names(count_investments_GBR)<-c("main_sector","count_investments_GBR")


count_investments_IND<-count(master_frame_venture_merge_IND,"main_sector")
names(count_investments_IND)<-c("main_sector","count_investments_IND")


#Merging of the two newly created columns with the other columns from the merged dataset
#D1,D2 & D3 refers to the datasets of USA,GBR & IND respectively (inclusive of two new columns)
d1<-merge(investments_US,count_investments_USA,by="main_sector",all=T)
D1<-merge(master_frame_venture_merge_US,d1,by="main_sector",all=T)
View(D1)

d2<-merge(investments_GBR,count_investments_GBR,by="main_sector",all=T)
D2<-merge(master_frame_venture_merge_GBR,d2,by="main_sector",all=T)
View(D2)

d3<-merge(investments_IND,count_investments_IND,by="main_sector",all=T)
D3<-merge(master_frame_venture_merge_IND,d3,by="main_sector",all=T)
View(D3)

#Sum of count of investments for each of the three countries 
sum(d1$count_investments_USA)
sum(d3$count_investments_IND)
sum(d2$count_investments_GBR)

#Sum of the sum of investments for each of the three countries
sum(d1$sum_investments_USA)
sum(d3$sum_investments_IND)
sum(d2$sum_investments_GBR)

#Top 3 sectors in each English speaking country
d1_top_3_sectors<-d1$main_sector[head(order(d1$count_investments_USA,decreasing=T),n=3)]
d2_top_3_sectors<-d2$main_sector[head(order(d2$count_investments_GBR,decreasing=T),n=3)]
d3_top_3_sectors<-d3$main_sector[head(order(d3$count_investments_IND,decreasing=T),n=3)]

#Top 3 sectors & their number of investments in each English speaking country
d1_top_3<-subset(d1,d1$main_sector==d1_top_3_sectors[1] |d1$main_sector==d1_top_3_sectors[2] |d1$main_sector==d1_top_3_sectors[3])
d1_top_3<-d1_top_3[order(d1_top_3$count_investments_USA,decreasing = T),]

d2_top_3<-subset(d2,d2$main_sector==d2_top_3_sectors[1] |d2$main_sector==d2_top_3_sectors[2] |d2$main_sector==d2_top_3_sectors[3])
d2_top_3<-d2_top_3[order(d2_top_3$count_investments_GBR,decreasing = T),]

d3_top_3<-subset(d3,d3$main_sector==d3_top_3_sectors[1] |d3$main_sector==d3_top_3_sectors[2] |d3$main_sector==d3_top_3_sectors[3])
d3_top_3<-d3_top_3[order(d3_top_3$count_investments_IND,decreasing = T),]

#Maximum count of investments from D1 dataset
D1_max_count_investment<-subset(D1,D1$count_investments_USA==max(count_investments_USA))

View(D1_max_count_investment)

#Companies with maximum count of investments from D1_max_count_investment dataset
D1_max_count_investment_companies<-aggregate(raised_amount_usd~name,data=D1_max_count_investment,sum)

#Renaming the name of the 2nd column
names(D1_max_count_investment_companies)[2]<-"total_raised_amount_usd"

#For the best sector count-wise, which company received the highest investment in US?
D1_top_company_sector<-D1_max_count_investment_companies$name[head(order(D1_max_count_investment_companies$total_raised_amount_usd,decreasing=T),n=1)]
#Virtustream

#Second maximum count of investments from D1 dataset
D1_second_max_count_investment<-subset(D1,D1$count_investments_USA!=max(count_investments_USA))
D1_second_max_count_investment<-subset(D1_second_max_count_investment,D1_second_max_count_investment$count_investments_USA==max(count_investments_USA))

#Companies with second maximum count of investments from D1_max_count_investment dataset
D1_second_max_count_investment_companies<-aggregate(raised_amount_usd~name,data=D1_second_max_count_investment,sum)

#Renaming the name of the 2nd column
names(D1_second_max_count_investment_companies)[2]<-"total_raised_amount_usd"

#For second best sector count-wise, which company received the highest investment in US?
D1_top_company_second_sector<-D1_second_max_count_investment_companies$name[head(order(D1_second_max_count_investment_companies$total_raised_amount_usd,decreasing=T),n=1)]
#SST Inc. (Formerly ShotSpotter)

#Maximum count of investments from D2 dataset
D2_max_count_investment<-subset(D2,D2$count_investments_GBR==max(count_investments_GBR))

View(D2_max_count_investment)

#Companies with maximum count of investments from D2_max_count_investment dataset
D2_max_count_investment_companies<-aggregate(raised_amount_usd~name,data=D2_max_count_investment,sum)
names(D2_max_count_investment_companies)[2]<-"total_raised_amount_usd"


#For the best sector count-wise, which company received the highest investment in GBR?
D2_top_company_sector<-D2_max_count_investment_companies$name[head(order(D2_max_count_investment_companies$total_raised_amount_usd,decreasing=T),n=1)]
#Electric Cloud

#Second maximum count of investments from D2 dataset
D2_second_max_count_investment<-subset(D2,D2$count_investments_GBR!=max(count_investments_GBR))
D2_second_max_count_investment<-subset(D2_second_max_count_investment,D2_second_max_count_investment$count_investments_GBR==max(count_investments_GBR))

#Companies with second maximum count of investments from D1_max_count_investment dataset
D2_second_max_count_investment_companies<-aggregate(raised_amount_usd~name,data=D2_second_max_count_investment,sum)

#Renaming the name of the 2nd column
names(D2_second_max_count_investment_companies)[2]<-"total_raised_amount_usd"

#For second best sector count-wise, which company received the highest investment in GBR?
D2_top_company_second_sector<-D2_second_max_count_investment_companies$name[head(order(D2_second_max_count_investment_companies$total_raised_amount_usd,decreasing=T),n=1)]
#Celltick Technologies

#Maximum count of investments from D3 dataset
D3_max_count_investment<-subset(D3,D3$count_investments_IND==max(count_investments_IND))

View(D3_max_count_investment)

#Companies with maximum count of investments from D3_max_count_investment dataset
D3_max_count_investment_companies<-aggregate(raised_amount_usd~name,data=D3_max_count_investment,sum)
names(D3_max_count_investment_companies)[2]<-"total_raised_amount_usd"

#For the best sector count-wise, which company received the highest investment in IND?
D3_top_company_sector<-D3_max_count_investment_companies$name[head(order(D3_max_count_investment_companies$total_raised_amount_usd,decreasing=T),n=1)]
#FirstCry.com

#Second maximum count of investments from D3 dataset
D3_second_max_count_investment<-subset(D3,D3$count_investments_IND!=max(count_investments_IND))
D3_second_max_count_investment<-subset(D3_second_max_count_investment,D3_second_max_count_investment$count_investments_IND==max(count_investments_IND))

#Companies with second maximum count of investments from D1_max_count_investment dataset
D3_second_max_count_investment_companies<-aggregate(raised_amount_usd~name,data=D3_second_max_count_investment,sum)

#Renaming the name of the 2nd column
names(D3_second_max_count_investment_companies)[2]<-"total_raised_amount_usd"

#For second best sector count-wise, which company received the highest investment in IND?
D3_top_company_second_sector<-D3_second_max_count_investment_companies$name[head(order(D3_second_max_count_investment_companies$total_raised_amount_usd,decreasing=T),n=1)]
#Manthan Systems

#---------------------------------------------------------------------------------------------------------
#Checkpoint 7:Plots
#--------------------------------------------------------------------------------------------------------
#Code for plot 1

#Created a dataset containing the three funding round type(seed,venture,private_equity) from master_frame dataset
master_frame_funding<-subset(master_frame,master_frame$funding_round_type=="venture"|master_frame$funding_round_type=="seed" |master_frame$funding_round_type=="private_equity")

#Total investment funding_round_type wise
funding<-aggregate(raised_amount_usd~funding_round_type,data=master_frame_funding,sum)

#Renamed the raised_amoungt_usd column of funding dataframe
names(funding)[2]<-"total_raised_amount_usd"

amount<-(funding$total_raised_amount_usd)/1000
round_type<-funding$funding_round_type
pct<-round(amount/sum(amount)*100)
round_type<-paste(round_type,pct)
round_type<-paste(round_type,"%",sep="")

#Average investment funding_round_type wise
master_frame_funding_average<-aggregate(raised_amount_usd~funding_round_type,data=master_frame_funding,mean)

#Renamed the raised_amoungt_usd column of master_frame_funding_average dataframe
names(master_frame_funding_average)[2]<-"average_raised_amount_usd"

#Created a vector named avg containing the average raised_amount_usd of the three funding round type
avg<-master_frame_funding_average$average_raised_amount_usd
avg<-avg/1000000
avg<-round(avg,digits=2)
funding_avg<-paste("Funding:",avg)
round_type<-paste(round_type,funding_avg,sep="\n")
round_type

plot1<-pie(amount,labels=round_type,col=rainbow(length(round_type)))

#Code for plot 2

#install.packages("ggplot2")
library(ggplot2)

plot2<-ggplot(top9,aes(y=total_raised_amount_usd/1000000000,x=country_code,fill=factor(country_code))) + geom_bar(stat ="identity")

#Code for plot 3

#Created 3 vectors containing the country code 
country_code_1<-D1$country_code[head(order(unique(D1$count_investments_USA),decreasing = T),n=3)]
country_code_2<-D2$country_code[head(order(unique(D2$count_investments_GBR),decreasing = T),n=3)]
country_code_3<-D3$country_code[head(order(unique(D3$count_investments_IND),decreasing = T),n=3)]

#Created 3 vectors containing the top 3 count of investments for each top 3 countries
top_3_count_investment_USA<-head(unique(sort(d1$count_investments_USA,decreasing =T)),n=3)
top_3_count_investment_GBR<-head(unique(sort(d2$count_investments_GBR,decreasing =T)),n=3)
top_3_count_investment_IND<-head(unique(sort(d3$count_investments_IND,decreasing =T)),n=3)

#Created 3 vectors containing the top 3 sectors in terms of count of investments for each top 3 countries
top_3_sectors_USA<-d1$main_sector[head(order(d1$count_investments_USA,decreasing =T),n=3)]
top_3_sectors_GBR<-d2$main_sector[head(order(d2$count_investments_GBR,decreasing =T),n=3)]
top_3_sectors_IND<-d3$main_sector[head(order(d3$count_investments_IND,decreasing =T),n=3)]

#Created 3 columns for a new data frame
count_investment<-c(top_3_count_investment_USA,top_3_count_investment_GBR,top_3_count_investment_IND)
sectors<-c(top_3_sectors_USA,top_3_sectors_GBR,top_3_sectors_IND)
countries<-c(country_code_1,country_code_2,country_code_3)

#New data frame creation
countries3_sectors3_investments3<-data.frame(countries,sectors,count_investment)
View(countries3_sectors3_investments3)

plot3_1<-ggplot(countries3_sectors3_investments3,aes(x=countries3_sectors3_investments3$countries,y=countries3_sectors3_investments3$count_investment,col=countries3_sectors3_investments3$sectors)) + geom_point(position="jitter",size=4)
plot3<-plot3_1 + labs(x="Countries",y="Investment") + labs(col="Sectors")



