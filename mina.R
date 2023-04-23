data<-read.csv("Clean_Dataset.csv")
head(dataset)
numberOfOccurence<-table(dataset$source_city)
numberOfOccurencedest<-table(dataset$destination_city)


barplot(numberOfOccurence,main = "number of occurence of source city",ylim = c(0,70000),col =" red")
barplot(numberOfOccurencedest,main = "number of occurence of destination city",ylim = c(0,70000),col =" red")
