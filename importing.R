library(dplyr)


#- Import data into R environment.
telecom <- read.csv("F:/data science/R/Datasets/Project_Simplilearn/Comcast Telecom Consumer Complaints/Comcast Telecom Complaints data.csv",
                   stringsAsFactors = FALSE )

telecom %>% View()

#- Provide the trend chart for the number of complaints at monthly and daily granularity levels.
# Analysis
sapply(telecom, class)
telecom %>% select(Customer.Complaint ,Date ) %>% View()



#formatting the Date Column of character class with 
#multiple format into 1 single date format
newdate1 <- strptime(telecom$Date , "%d/%m/%Y" )
newdate1
newdate2 <-strptime(telecom$Date , "%d-%m-%Y" )
newdate2



newdate1[is.na(newdate1)] <- newdate2[!is.na(newdate2)]
newdate1

newtelecom <- cbind(telecom[,1:which(names(telecom)=="Date")-1], newdate1, 
                    telecom[,which(names(telecom)=="Date"):ncol(telecom) ])
newtelecom %>% View()


#Analysis
newtelecom %>% select(Ticket..,Customer.Complaint,newdate1) %>% arrange(newdate1)%>% View()

month <- format(newdate1 , "%m")
month
 
newtelecom <-cbind(newtelecom[,1:which(names(newtelecom)=="newdate1")-1],
                   month,
                   newtelecom[,which(names(newtelecom)=="newdate1"):ncol(telecom)]
                   )
newtelecom %>% select(Ticket..,Customer.Complaint,newdate1,month) %>% arrange(newdate1)%>% View()


total_complaints <- table(newtelecom$month)
total_complaints
months <- sort(unique(newtelecom$month))
months


library(RColorBrewer)
display.brewer.all()
no.of.complaints.per.month <- barplot(total_complaints,
        col = brewer.pal(12, "Paired"),
        xlab = "Months of the Year",
        ylab = "Number of complaints",
        names.arg = c("jan","feb","mar","apr","may","june","july",
                      "aug","sep","oct","nov","dec"),
        ylim = c(0,1200))
text(x=no.of.complaints.per.month,y= total_complaints +25, labels = total_complaints)





# - Provide a table with the frequency of complaint types.
# Which complaint types are maximum i.e., around internet, network issues, 
# or across any other domains.
View(telecom)

Service_Issue <- grep("ervice", telecom$Customer.Complaint, ignore.case = FALSE) 
Internet_Issue <- grep("internet", telecom$Customer.Complaint, ignore.case = FALSE)
Overbilling_Issue <- grep("bill", telecom$Customer.Complaint,ignore.case = FALSE)
View(Service_Issue)
View(Internet_Issue)
View(Overbilling_Issue)
length(Service_Issue)
complaint_types <- c(length(Service_Issue), length(Internet_Issue), length(Overbilling_Issue))

display.brewer.all()
complaint.types <- barplot(complaint_types,
        col = brewer.pal(3 , "Set2"),
        xlab = "Types of complaints",
        ylab = "Frequency",
        names.arg= c("Service", "Internet", "billing"),
        ylim = c(0,800))
text(x= complaint.types , y=complaint_types +20 ,labels = complaint_types )
  
  

# - Create a new categorical variable with value as Open and Closed. 
#   Open & Pending is to be categorized as Open and Closed & 
#   Solved is to be categorized as Closed.
# - Provide state wise status of complaints in a stacked bar chart. 
#   Use the categorized variable from Q3.

View(telecom)  
  

table(newtelecom$Status)
#replace(telecom$Status, telecom$Status=="Pending", "Closed")

newtelecom[newtelecom$Status=="Solved", "Status"] <- "Closed"
newtelecom[newtelecom$Status=="Pending" ,"Status" ] <- "Open"
Status.State <- table(newtelecom$Status, newtelecom$State)
length(unique(newtelecom$State))


barplot(Status.State)


a=data.frame(Status.State)
# Which state has the maximum complaints
filter(a, Var1=="Open") %>% arrange(desc(Freq)) %>% View() 
# Which state has the highest percentage of unresolved complaints
a["Percentage"] <- (a$Freq/nrow(telecom))*100
filter(a,Var1=="Open") %>% arrange(desc(Percentage)) %>% View() 
# - Provide the percentage of complaints resolved till date, 
# which were received through the Internet and customer care calls.
View(newtelecom)
names(newtelecom)
table(newtelecom$Received.Via, newtelecom$Status)
(sum(newtelecom$Status=="Closed")/nrow(newtelecom))*100







