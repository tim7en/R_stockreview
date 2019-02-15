# Get quantmod
if (!require("quantmod")) {
  install.packages("quantmod")
  library(quantmod)
}
Dats <- 'BABA'
M <- 'February'
D <- c('Friday')

start <- as.Date("1930-01-01")
end <- as.Date(Sys.Date())

# Let's get Apple stock data; Apple's ticker symbol is AAPL. We use the
# quantmod function getSymbols, and pass a string as a first argument to
# identify the desired ticker symbol, pass 'yahoo' to src for Yahoo!
# Finance, and from and to specify date ranges

# The default behavior for getSymbols is to load data directly into the
# global environment, with the object being named after the loaded ticker
# symbol. This feature may become deprecated in the future, but we exploit
# it now.

getSymbols(as.character(Dats), src = "yahoo", from = start, to = end)
Dats <- eval(parse(text = Dats))
candleChart(Dats, up.col = "black", dn.col = "red", theme = "white")


dif <- (Dats[,1] - Dats[,4])#/13 #from 9:30 to 4, every 30 minutes #there is an increase or decrease in the day on average
colnames (dif) <- 'difHL' #on average in the day per every 30 minutes this was a change in the value it was eather up or down

library ('data.table')
dT <- data.frame (dif)
dT$Date <- as.Date (rownames(dT))
dT <- data.table(dT)
dT$Month <- months(dT$Date)
dT$day <- weekdays(dT$Date)

if (is.null(D)){
  dT_sub <- dT[which(dT$Month == M),]
} else {
  dT_sub <- dT[which(dT$Month == M & dT$day == D),]
}

#Statistically, there are more Wednesdays in February with closing price lower then opening price
GIN <- length (which (dT_sub$difHL>0))/length(dT_sub$difHL)
LSE <- length (which (dT_sub$difHL<0))/length(dT_sub$difHL)
NOD <- length (which (dT_sub$difHL==0))/length(dT_sub$difHL)



#Lets plot out cumulative 30 min increase or decrease over this time
plot (cumsum (dT_sub$difHL[which (dT_sub$difHL>0)]))
plot (cumsum (dT_sub$difHL[which (dT_sub$difHL<0)]))

#When the cumulative sum overperforms cumulative null, 
#meaning that if we would invest in the stocks of this company
#we would gain profit more than 50% of the time


#What is the statistics of gain that occured at that day of the month?
round (summary (dT_sub$difHL[which (dT_sub$difHL>0)]), 3)

#What is the statistics of the loss that occured at that day of the month ?
round (summary (dT_sub$difHL[which (dT_sub$difHL<0)]), 3)

#print, chances
print (paste0('Chances of gain: ', round (GIN,3)))
print (paste0('Chances of loss: ', round (LSE, 3)))
print (paste0('Chances of net neutrality: ', round (NOD, 3)))



#Get chances of gain or loss by month for each month

#Statistically, there are more Wednesdays in February with closing price lower then opening price
GIN <- dT[which (dT$difHL>0),]
LSE <- dT[which (dT$difHL<0),]
NOD <- dT[which (dT$difHL==0),]

#Aggregate by month
stata_GIN <- aggregate(difHL~Month, GIN,mean)
stata_LSE <- aggregate(difHL~Month, LSE,mean)

datas <- merge (stata_GIN, stata_LSE, by = 'Month')
colnames (datas) <- c('Month', 'Gain', 'Loss')
datas <- t(datas[,-1])
colnames(datas) <- month.name
data3=melt(datas,id="group")
colnames(data3) <- c('group', 'variable', 'value')

require(ggplot2)
# ggplot(data=data3,aes(x=variable,y=value,fill=group))+
#   geom_bar(stat="identity")

# ggplot(data=data3,aes(x=variable,y=value,fill=group))+
#   geom_bar(stat="identity")+
#   scale_fill_brewer(palette="Greens")+xlab("")+ylab("")


# ggplot(data=data3,aes(x=variable,y=value,fill=group))+
#   geom_bar(stat="identity")+
#   coord_polar()+
#   scale_fill_brewer(palette="Set1")+xlab("")+ylab("")

ggplot(data=data3,aes(x=variable,y=value,fill=group))+
  geom_bar(stat="identity",width=1,colour="black",size=0.1)+
  coord_polar()+
  scale_fill_brewer(palette="Set1")+
  xlab("")+ylab("")

# ggplot(data=data3,aes(x=variable,y=group,fill=value))+
#   geom_tile(colour="black",size=0.1)+
#   scale_fill_gradientn(colours=c("white","steelblue"))+
#   coord_polar()+xlab("")+ylab("")

#Aggregate by weekday of the year

#Aggregate by month
stata_GIN <- aggregate(difHL~Month+day, GIN,mean)
stata_LSE <- aggregate(difHL~Month+day, LSE,mean)
stata_GIN$id <- rownames(stata_GIN)
stata_LSE$id <- rownames(stata_LSE)
datas <- merge (stata_GIN, stata_LSE, by = 'id')
datas <- datas[,-c(1,5,6)]
datas$date <- paste(datas$Month.x,datas$day.x, sep = ':')
datas <- datas[,-c(1,2)]
colnames (datas) <- c('Gain', 'Loss', 'Date')
datas$Date <- as.factor(datas$Date)

datas_t <- t(datas[,c(1,2)])
colnames(datas_t) <- datas$Date

data3=melt(datas_t,id="group")
colnames(data3) <- c('group', 'variable', 'value')

ggplot(data=data3,aes(x=variable,y=value,fill=group))+
  geom_bar(stat="identity",width=1,colour="black",size=0.1)+
  coord_polar()+
  scale_fill_brewer(palette="Set1")+
  xlab("")+ylab("")+
  ggtitle ('BABA group shares distribution')+
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))
