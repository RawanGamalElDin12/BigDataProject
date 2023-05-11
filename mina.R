dataset<-read.csv("Clean_Dataset.csv")
head(dataset)


par(mfrow=c(3,3))
#################
numberOfOccurence<-table(dataset$source_city)
barplot(numberOfOccurence,main = "number of occurence of source city",ylim = c(0,70000),col =" red")
###############
numberOfOccurencedest<-table(dataset$destination_city)
barplot(numberOfOccurencedest,main = "number of occurence of destination city",ylim = c(0,70000),col =" blue")

#############
classtable<-table(dataset$class)
barplot(classtable/10000 ,main="frequency of economy and business flights",xlab = " flight type", ylab = "number of reservations (x10000)",ylim = c(0,30),col=rainbow((6)))
############
airlineTable<-table(dataset$airline)
barplot(airlineTable/1000 , main="number of flights of each airline",ylim = c(0,150) ,xlab = "airline name", ylab = "number of flights(x1000)", col=rainbow(6))
table(data$airline)


#############
departuretimeTable<-table(dataset$departure_time)
barplot(departuretimeTable , main="Frequency of Departure Time",xlab = "Departure Time",ylab="Frequency", col=rainbow(6))

#############
ArrivaltimeTable<-table(dataset$arrival_time)
barplot(ArrivaltimeTable , main="Frequency of Arrival Time",xlab = "Arrival Time",ylab="Frequency", col=rainbow(6))

#############

Stops_table<-table(dataset$stops)
barplot(Stops_table , main="Frequency of Stops",xlab = "Stops",ylab="Frequency", col=rainbow(6))

#reset
par(mfrow = c(1, 1))
