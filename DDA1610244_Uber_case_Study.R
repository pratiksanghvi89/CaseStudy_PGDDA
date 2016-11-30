
#----------------------------------Loading Data in R workspace --------------------------------------------------
uber <- read.csv("Uber request data.csv",header = T,stringsAsFactors = F)

#--DATA PREPARATION---------------------------------------------------------------------------------------------------------

# 1. Make a grouped bar chart depicting the hour-wise trip request made at city and airport respectively. 
#    You can aggregate the data for all 5 days on the same axis of 24 hours. 
#    Each bar should correspond to an hour and pick-up point (city / airport) should be displayed in two colours?

#------Convert Request_Time to the required Format-----------------------------

uber$Request.time <- gsub(":.*","",uber$Request.time)

library(ggplot2)
#     Grouped bar chart depicting the hour-wise trip request made at city and airport respectively
uber_hourwise_trip_request <- ggplot(uber,aes(x=Request.time,fill=factor(Pickup.point)))
uber_hourwise_trip_request + geom_bar()+labs(x="Hour of the day",y="Number of Request",fill="Pickup Point")


#2 ----------Divide the request-time into 5 time-slots - an Additional Column Time_slot----------------------------------------

#------------Code for adding the new column Time_slot in the original data set------------------- 

#Extracting the Hour from Request_Time and storing it in a data frame
Request.id <- uber$Request.id
Request.time <- uber$Request.time
Time_slot <- "Pre_Morning"

uber_time_id <- data.frame(Request.id,Request.time,Time_slot)

uber_time_id$Request.time <- as.character(uber_time_id$Request.time)

uber_time_id$Time_slot <- as.character(uber_time_id$Time_slot)


# Code for Assigning 5 Time_slots to the Request Time ------------------

for (x in 1:nrow(uber)) {
  
  if(uber$Request.time[x]>="03" & uber$Request.time[x]<"07")
    
  {uber_time_id$Time_slot[x] <- "Pre_Morning"}
  
  else if(uber$Request.time[x]>="07" & uber$Request.time[x]<"11")
  {
    uber_time_id$Time_slot[x] <- "Morning_Rush"
    
  }
  
  else if(uber$Request.time[x]>="11" & uber$Request.time[x]<"17")
  {
    
    uber_time_id$Time_slot[x] <- "Day_Time"
    
  }
  else if(uber$Request.time[x]>="17" & uber$Request.time[x]<"23")
  {
    
    uber_time_id$Time_slot[x]<- "Evening_Rush"
    
  }
  
  else
  {
    
    uber_time_id$Time_slot[x] <- "Late_Night"
  }
  
}


#-----Merging the original dataset with the newly created on in order to add a new column---------
uber_time_id$Request.time <- NULL
uber <- merge(uber,uber_time_id,by="Request.id",all = F)

# Making an subset to number of trips completed 
uber_trip_completed <- subset(uber,uber$Status=="Trip Completed")

#--------Also give the count of the number of trips made during different time slots you have decided?
uber_pre_morning  <- length(which(uber$Time_slot=="Pre_Morning" & uber$Status=="Trip Completed"))
#491
uber_morning_rush <- length(which(uber$Time_slot=="Morning_Rush" & uber$Status=="Trip Completed"))
#618
uber_day_time     <- length(which(uber$Time_slot=="Day_Time" & uber$Status=="Trip Completed"))
#598
uber_evening_rush <- length(which(uber$Time_slot=="Evening_Rush" & uber$Status=="Trip Completed"))
#942
uber_late_night   <- length(which(uber$Time_slot=="Late_Night" & uber$Status=="Trip Completed"))
#203

#Plot stacked bar chart where each bar represents a time slot and 
# the y-axis shows the frequency of completed request


Trip_Completed <- count(uber_trip_completed,"Time_slot")
uber_number_trips_Time_Slot <- ggplot(Trip_Completed, aes(x=reorder(Time_slot,freq),y=freq))
uber_number_trips_Time_Slot<- uber_number_trips_Time_Slot + geom_bar(stat = "identity",fill="orange") + labs(x="Time Slots",y="Number of Trips Made",fill=" Trips Completed")
uber_number_trips_Time_Slot + geom_text(stat = "identity",aes(label=..y..),vjust=-0.5)


##############################  PROBLEM IDENTIFICATION:    ##################################################
#---------------------------------------------------------------------------------
# 1. Make a stacked bar chart where each bar represents a time slot and y axis shows the frequency of requests. 
#    Different proportions of bars should represent the completed, cancelled and no cars 
#    available out of the total customer requests.
library(plyr)
uber_time_slot_request_status <- ggplot(uber,aes(x=Time_slot,fill = Status))
uber_time_slot_request_status <- uber_time_slot_request_status +geom_bar()+labs(x="Time Slots",y="Number of Requests")
uber_time_slot_request_status + geom_text(stat = "count",aes(label=..count..),size = 3, hjust = 0.5, vjust = 3,position = "stack")
#------------------------------------------------------------------------------------

# Problem 1 - During the Morning Rush hour Most number of trips are cancelled

#-------------Making an subset of the original dataset to get the data related to Problem 1-------

uber_cancel_morning <- subset(uber,uber$Status=="Cancelled" & uber$Time_slot== "Morning_Rush")
#--------------------------------------------------------------------------------------------------

#1  Plot a stacked bar chart to find out if the problem is more severe 
#   for pick-up requests made at the airport or the city

library(scales)

uber_var1 <- ggplot(uber_cancel_morning,aes(x=Request.time ,fill=factor(Pickup.point))) +geom_bar(aes(y = (..count..)/sum(..count..))) + labs(x="Time at which Request Made",y="Number of Trips Cancelled During Morning Rush",fill="Pickup Point")
uber_var2 <- uber_var1 + labs(x="Time at which Request Made",y="Number of Trips Cancelled During Morning Rush",fill="Pickup Point")
uber_p1  <- uber_var2 + geom_text(stat='count',aes(y = ((..count..)/sum(..count..)),label =scales::percent((..count..)/sum(..count..))),size = 3,vjust = 1, hjust = 0.5) 
uber_p1 + scale_y_continuous(labels=percent)




#2 No. of trip requests made in city: 1159

no_trip_req_city <- length(which(uber$Pickup.point=="City" & uber$Time_slot=="Morning_Rush"))


#No. of trips completed from city to airport: 353
no_trip_req_city_airport <- length(which(uber$Pickup.point=="City" & uber$Status== "Trip Completed"& uber$Time_slot=="Morning_Rush"))


#---------------Problem 2: During Evening Rush Maximum time No cars are available-------

#-------------Making an subset of the original dataset to get the data related to Problem 2-------

uber_Evening_No_trip <- subset(uber,uber$Status=="No Cars Available" & uber$Time_slot== "Evening_Rush")
#---------------------------------------------------------------------------------------------

#1 Plot the stacked bar chart to find out if the issue is for pick-up request made
# at the airport or the city
library(scales)
uber_var_evening1 <- ggplot(uber_Evening_No_trip,aes(x=Request.time,fill=factor(Pickup.point))) + geom_bar(aes(y = (..count..)/sum(..count..))) + labs(x="Time at which Request Made",y="Number of No Cars Available During Evening Rush",fill="Pickup Point")
uber_var_evening2<-uber_var_evening1 + geom_text(aes(y = ((..count..)/sum(..count..)), label =scales::percent((..count..)/sum(..count..))), stat = "count", vjust = 1) +facet_grid(~Pickup.point)
uber_p2<-uber_var_evening2 + scale_y_continuous(labels = percent)
uber_p2

#2 No. of trip requests made at the airport: 1990
no_req_airport_evng_rush <- length(which(uber$Pickup.point=="Airport" & uber$Time_slot == "Evening_Rush"))


#No. of trips completed from airport to the city:460
no_req_airporttocity_evng_rush <- length(which(uber$Pickup.point=="Airport" & uber$Status== "Trip Completed"& uber$Time_slot == "Evening_Rush"))


#----------------------------------------------------------------------------------------------------------------------------  