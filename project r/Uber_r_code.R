
uber_masterdata <- read.csv("Uber request data.csv",stringsAsFactors = FALSE,na.strings = TRUE)
uber_masterdata$request_hour <- sub(":.*","",uber_masterdata$Request.time)
uber_masterdata$request_hour <- as.numeric(uber_masterdata$request_hour)
require(dplyr)
require(ggplot2)
require(scales)
hourwise_request_count <- ggplot(uber_masterdata,aes(x=factor(request_hour),fill=factor(Pickup.point)))
plot1 <- hourwise_request_count+geom_bar(stat='count',position = "dodge")+
              ggtitle("Hourly Demand for Uber Cabs")+
                labs(x="Time in Hours", y="Number of Cabs Requested")+
                  labs(fill="Pickup Point")

plot1
# Generate a sequence of numbers from 0 to 23 
request_hour <- c(0:23)
Time_Slot1 <- c("Pre_Morning","Morning_Rush","Day_Time","Evening_Rush","Late_Night")
times <- c(4,6,7,5,2)
Time_Slot <- rep(Time_Slot1,times)
new_frame <- data.frame(Time_Slot,request_hour)
uber_masterdata <- merge(uber_masterdata,new_frame,by="request_hour",all.x=TRUE)
uber_masterdata <- uber_masterdata[,c(2,3,4,5,6,7,8,1,9)]
trips_completed <- subset(uber_masterdata,uber_masterdata$Status=="Trip Completed")
Timeslot_bar <- ggplot(trips_completed,aes(x=Time_Slot))


plot2 <- Timeslot_bar+geom_bar(stat="count",col="black",fill="green")+
          ggtitle("Trips completed during different Time Slots")+
            labs(x="Time Slots",y="Trips Completed")+
              geom_text(stat='count',aes(label=..count..),vjust=-1)+
                guides(fill=FALSE)+
                  scale_x_discrete(limits=c("Morning_Rush","Evening_Rush","Day_Time",
                                            "Late_Night","Pre_Morning"))
      
                    
plot2


timeslot_request_count <- ggplot(uber_masterdata,aes(x=factor(Time_Slot),fill=factor(Status)))
plot3 <- timeslot_request_count+geom_bar(stat="count",position = "stack",col="black")+
          ggtitle("Trips during Different Time Slots")+
            scale_x_discrete(limits=c("Evening_Rush","Morning_Rush","Day_Time",
               "Late_Night","Pre_Morning"))+
              labs(x="Time Slots",y="Number of Requests")+labs(fill="Trip Status")+
                scale_fill_discrete(limits=c("Trip Completed","No Cars Available","Cancelled"))
              
#view the plot
plot3

#problem 1. Large number of service requests got cancelled during the Morning_Rush Time slot
Problem_df <- subset(uber_masterdata,uber_masterdata$Time_Slot=="Morning_Rush")
Problem1_count <- ggplot(Problem_df,aes(x=factor(Status),fill=factor(Pickup.point)))
plot4 <- Problem1_count+geom_bar(stat="count",position = "stack")+
  ggtitle("Morning Rush Cab Status")+
  labs(x="Status",y="Total count")+
  labs(fill="Pickup Point")+scale_x_discrete(limits=c("Trip Completed","Cancelled","No Cars Available"))+
  annotate("text", x=-Inf,y=Inf,label="Airport - 2.88% & City = 97.11%", hjust=-.1,vjust=1)
#view the plot
plot4

total_trip_cancel <- length(which(Problem_df$Status=="Cancelled"))
airport_trip_cancel <- length(which((Problem_df$Pickup.point=="Airport") & (Problem_df$Status == "Cancelled")))
city_trip_cancel <- length(which((Problem_df$Pickup.point=="City") & (Problem_df$Status == "Cancelled")))
percent_trip_cancel_city <- (city_trip_cancel/total_trip_cancel*100)
percent_trip_cancel_airport <- (airport_trip_cancel/total_trip_cancel*100)
demand_trip_request_city <- length(which(Problem_df$Pickup.point=="City"))
demand_trip_city_completed <- length(which((Problem_df$Pickup.point=="City")& (Problem_df$Status=="Trip Completed")))





Problem2_df <- subset(subset(uber_masterdata,uber_masterdata$Time_Slot=="Evening_Rush"))
Problem2_count <- ggplot(Problem2_df,aes(x=factor(Status),fill=factor(Pickup.point)))
plot5 <- Problem2_count+geom_bar(stat="count",position = "stack")+
  ggtitle("Evening Rush Cabs Status")+
  labs(x="Status",y="Total count")+
  labs(fill="Pickup Point")+scale_x_discrete(limits=c("No Cars Available","Trip Completed","Cancelled"))+
  annotate("text", x=-Inf,y=Inf,label="Airport - 95.41% & City = 4.59%", hjust=-.1,vjust=1)  
#view the plot
plot5
