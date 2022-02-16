####              Analysis Task             ####
#- Import data into R environment.

library(magrittr)
library(stringi)
library(plyr)
library(dplyr)
library(ggplot2)
library(NbClust)
library(scales)
library(readxl)
library(lubridate)
library(nycflights13)
library(stringr)
library(tidyverse)
library(gridExtra)

getwd()

Data <- read.csv("/home/labsuser/Comcast Telecom Complaints data.csv")
head(Data,4)
str(Data)

li<-parse_date_time(x = Data$Date,orders = c("d m y", "d B Y", "m/d/y"),locale = Sys.getlocale("LC_TIME"))
data2<-Data
data2$Date <- li

data2$Month <- format(as.Date(data2$Date), "%m")
data2$Month<- month.abb[as.integer(data2$Month)]
head(data2)


empty_values <- is.na(Data)
length(empty_values[empty_values==TRUE])
unique(Data$Ticket)
unique(Data$CustomerComplaint)
unique(Data$ReceivedVia)
unique(Data$City)
Data$City <-  str_replace(Data$City,"Albuquerqur", "Albuquerque")
Data$City <-  str_replace(Data$City,"Colorado Spring", "Colorado Springs")
unique(Data$State)
Data$State <-  str_replace(Data$State,"District Of Columbia", "District of Columbia")
unique(Data$State)
unique(Data$Zipcode)
unique(Data$Status)
unique(Data$FilingonBehalfofSomeone)

#- Provide the trend chart for the number of complaints at monthly and daily granularity levels.
#- Provide the trend chart for the number of complaints at monthly granularity levels.

data_date<-data2 %>% group_by(Date) %>% 
  dplyr::summarise(frequency = n())
df <-data_date[order(-data_date$frequency),]
dff<-head(df)
dff

data_month<-data2 %>% 
  group_by(Month) %>% dplyr :: summarise(frequency = n())
data_month

data2$Month <- as.factor(data2$Month)
levels(data2$Month)

ggplot(data_month, aes(Month, frequency, group = 1)) + 
  geom_point(color="red") + 
  geom_line(color="blue") +
  xlab("Month") + 
  ylab("Number of Complaints")

monthly_c <- data2 %>%
  group_by(Month = as.integer(month(Date))) %>%
  dplyr::summarise(Total_c = n())
monthly_c <- arrange(monthly_c, Month)

ggplot(monthly_c, aes(Month, Total_c, label= Total_c)) +
  geom_line(size = 1, color="blue") +
  labs(title = "Number of Complaints per Month",
       x = "Month", y = "Number of Complaints") +
  scale_x_continuous(breaks = monthly_c$Month) + 
  theme(plot.title = element_text(hjust=0.5)) +
  geom_label(position = position_dodge(0.3))

#- Provide the trend chart for the number of complaints at daily granularity levels.

daily_c <- data2 %>%
  group_by(Date) %>%
  dplyr::summarise(n_comp = n())

ggplot(data_date, aes(Date, frequency, group = 1)) + 
  geom_point(color="red") + 
  geom_line(color="blue") +
  xlab("Date") + 
  ylab("Number of Complaints")

ggplot(dff, aes(Date, frequency, group = 1)) + 
  geom_point(color="red") + 
  geom_line(color="blue") +
  xlab("Date") + 
  ylab("Number of Complaints")


ggplot(daily_c, aes(as.POSIXct(Date), n_comp)) +
  geom_line(color="blue") + geom_point(color="red") +
  labs(title= "Number of Complaints per Day",
       x = "Days", y = " Number of Complaints") +
  scale_x_datetime(breaks = "1 weeks", date_labels ="%d/%m") +
  theme(plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(angle=90))


# - Provide a table with the frequency of complaint types.

data3<-data2%>% mutate(Customer.Complaint = tolower(Customer.Complaint))
CustTable <- table(data3$Customer.Complaint)
CustTable <- data.frame(CustTable)
filtered<-CustTable %>% 
  rename(CustomerComplaintType = Var1, Frequency = Freq
  )
final <- filtered %>% arrange(desc(Frequency))

final_most<-head(final,20)
final_most

t_internet <- contains(Data$Customer.Complaint, match="internet", ignore.case = "T")
t_payment <- contains(Data$Customer.Complaint, match="payment", ignore.case = "T")
t_xfinity <- contains(Data$Customer.Complaint, match="xfinity", ignore.case = "T")
t_billing <- contains(Data$Customer.Complaint, match="billing", ignore.case = "T")
t_datacap <- contains(Data$Customer.Complaint, match="data cap", ignore.case = "T")
t_fee <- contains(Data$Customer.Complaint, match="fee", ignore.case = "T")
t_bcustomer <- contains(Data$Customer.Complaint, match="customer", ignore.case = "T")
t_hbo <- contains(Data$Customer.Complaint, match="hbo", ignore.case = "T")
t_price <- contains(Data$Customer.Complaint, match="price", ignore.case = "T")
t_contract <- contains(Data$Customer.Complaint, match="contract", ignore.case = "T")
t_bait <- contains(Data$Customer.Complaint, match="bait", ignore.case = "T")
t_charges <- contains(Data$Customer.Complaint, match="charges", ignore.case = "T")

Data$Type[t_internet] <- "Internet"
Data$Type[t_payment] <- "Payment"
Data$Type[t_xfinity] <- "Xfinity"
Data$Type[t_billing] <- "Billing"
Data$Type[t_datacap] <- "Data Cap"
Data$Type[t_fee] <- "Fee"
Data$Type[t_bcustomer] <- "BadCustomerService"
Data$Type[t_hbo] <- "HBO"
Data$Type[t_price] <- "Price"
Data$Type[t_contract] <- "Contract"
Data$Type[t_bait] <- "Bait"
Data$Type[t_charges] <- "Charges"

#  Which complaint types are maximum i.e., around internet, network issues, or across any other domains.

Data$Type[-c(t_internet,t_payment,t_xfinity,t_billing,t_datacap, t_fee, t_bcustomer, t_hbo,
                   t_price,t_contract,t_bait,t_charges)]<- "Other"

f_Data <- Data %>%
  group_by(Type)%>%
  dplyr::summarise(Frequency = n())

f_Data<- arrange(f_Data,(-Frequency))
f_Data

ggplot(final_most, aes(CustomerComplaintType, Frequency)) + geom_bar(stat = "identity") + 
  theme(plot.title = element_text(hjust= 0.5), axis.text.x = element_text(angle = 90))


ggplot(f_Data, aes(Type, Frequency)) + geom_bar(stat = "identity") + 
  theme(plot.title = element_text(hjust= 0.5), axis.text.x = element_text(angle = 90))



# - Create a new categorical variable with value as Open and Closed. Open & Pending is to be categorized as Open and Closed & Solved is to be categorized as Closed.
# - Provide state wise status of complaints in a stacked bar chart. Use the categorized variable from Q3. Provide insights on:
levels(Data$Status)

Data$Complaint_Status<-revalue(Data$Status, c(Pending = "Open", Solved = "Closed"))
head(Data)

open_complaints <- (Data$Status =="Open" | Data$Status == "Pending")
closed_complaints <- (Data$Status =="Closed" | Data$Status == "Solved")
Data$NewStatus[open_complaints]<- "Open"
Data$NewStatus[closed_complaints]<- "Closed"

complaints_state <- Data %>%
  group_by(State,NewStatus) %>%
  dplyr::summarise(Count_status = n())



# Which state has the maximum complaints
tab <- table(Data$State,Data$Complaint_Status)
tab <- cbind(tab, Total = rowSums(tab))
head(tab,15)

ggplot(Data, aes(y = State)) + geom_bar(aes(fill = Complaint_Status)) + scale_fill_manual(values = c("Open"="firebrick", "Closed"= "steelblue"))


# Which state has the highest percentage of unresolved complaints

max_open <- filter(complaints_state, NewStatus== "Open")
max_open <- arrange(max_open, desc(Count_status))
max_open

p_unresolved <- summarise(max_open, Perc_u = Count_status/nrow(max_open))
p_unresolved


#- Provide the percentage of complaints resolved till date, which were received through theInternet and customer care calls.

resolved <- filter(complaints_state, NewStatus== "Closed")
resolved
p_resolved <- summarise(resolved, Perc_r = Count_status/nrow(resolved))
p_resolved

t_complaints <- group_by(Data, NewStatus)
p_open_close <- summarise(t_complaints,percentage=(n()/nrow(t_complaints)))
p_open_close



levels(Data$Received.Via)
ggplot(Data, aes(y = Received.Via )) + geom_bar(aes(fill = Complaint_Status))

df1 <- table(Data$Received.Via, Data$Status)
df1 <- cbind(df1, Total = rowSums(df1))
df1

slices <- c(864, 255)
lbls <- c("Closed", "Open")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),main="Pie Chart of Received Via Call")

slices <- c(843, 262)
lbls <- c("Closed", "Open")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),main="Pie Chart of Received Via Internet")

slices3 <- c(864, 843)
names3 <- c("Customer Care Call", "Internet")
slices_per3 <- round(slices3/sum(slices3)*100)
slices_per3 <- as.character(slices_per3)
slices_names_per3 <- paste(names3, slices_per3,"%")
pie(x=slices3,labels = slices_names_per3, col=rainbow(length(names3)),
    main="Total Closed Complaints received via Internet and Calls")

