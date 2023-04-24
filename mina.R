data<-read.csv("Clean_Dataset.csv")
head(dataset)


#################
dev.new(width = 10, height = 8)
numberOfOccurence<-table(dataset$source_city)
barplot(numberOfOccurence,main = "number of occurence of source city",ylim = c(0,70000),col =" red")
###############
dev.new(width = 10, height = 8)
numberOfOccurencedest<-table(dataset$destination_city)
barplot(numberOfOccurencedest,main = "number of occurence of destination city",ylim = c(0,70000),col =" blue")

#############
dev.new(width = 10, height = 8)
classtable<-table(dataset$class)
barplot(classtable/10000 ,main="frequency of economy and business flights",xlab = " flight type", ylab = "number of reservations (x10000)",ylim = c(0,30))
############
airlineTable<-table(dataset$airline)
dev.new(width = 10, height = 8)
barplot(airlineTable/1000 , main="number of flights of each airline",ylim = c(0,150) ,xlab = "airline name", ylab = "number of flights(x1000)")
table(data$airline)



