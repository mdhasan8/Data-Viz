
data<-read.csv(file="owid-covid-data.csv",header=TRUE)
dim(data)
colnames(data)
#data <- data.frame(data)
data1 <- data[data$iso_code == "USA", ] 
dim(data1)
data2 <-data1[,c(1,4,35,36,37,38,39,40,41,42,43)]
colnames(data2)

data3 <- data[69885:69962,]
data3 <-data3[,c(1,4,35,36,37,38,39,40,41,42,43)]
colnames(data3)

miss.info<- function(dat, filename=NULL){  
  
  vnames <- colnames(dat); vnames 
  
  n <- nrow(dat)  
  
  out <- NULL  
  
  for (j in 1: ncol(dat)){  
    
    vname <- colnames(dat)[j]  
    
    x <- as.vector(dat[,j]) 
    
    n1 <- sum(is.na(x), na.rm=T)  
    
    n2 <- sum(x=="NA", na.rm=T)  
    
    n3 <- sum(x=="", na.rm=T)  
    
    nmiss <- n1 + n2 + n3  
    
    ncomplete <- n-nmiss 
    
    out <- rbind(out, c(col.number=j, vname=vname, mode=mode(x), n.levels=length(unique(x)), ncomplete=ncomplete, miss.perc=nmiss/n)) } 
  
  out <- as.data.frame(out)  
  
  row.names(out) <- NULL  
  
  return(out) 
  
}  

miss.info(data3) 

library(ggplot2)
library(reshape2)
library(scales)

# people vaccinated
ggplot(data3, aes(x = as.Date(date), y = people_vaccinated)) +
  xlab("Months") + ylab("People Vaccinated")+
  geom_bar(stat="identity")+
  scale_x_date(labels = date_format("%m-%d-%Y"))+
  geom_col()


ggplot(data3, aes(x = as.Date(date), y = people_vaccinated)) +
    xlab("Months") + ylab("People Vaccinated")+
    geom_bar(stat="identity", fill="red")+
    scale_x_date(labels = date_format("%m-%d-%Y"))

# people fully vaccinated

ggplot(data3, aes(x = as.Date(date), y = people_fully_vaccinated)) +
  xlab("Months") + ylab("People fully Vaccinated")+
  geom_bar(stat="identity", fill="red")+
  scale_x_date(labels = date_format("%m-%d-%Y"))

# combined

dfm <- melt(data3[,c('date','people_vaccinated','people_fully_vaccinated')],id.vars = 1)

ggplot(dfm,aes(x = as.Date(date),y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity") + 
  scale_x_date(labels = date_format("%m-%d-%Y"))+
  labs(x = "date")+
  labs(y = "Number of people vaccinated and fully vaccinated")





# people vaccinated line graph
ggplot(data3, aes(x=as.Date(date))) + 
  geom_line(aes(y = people_vaccinated), color = "darkred") + 
  geom_line(aes(y = people_fully_vaccinated), color="steelblue", linetype="twodash")+
  scale_x_date(labels = date_format("%m-%d-%Y"))


p = ggplot() + 
  geom_line(data = data3, aes(x = as.Date(date), y = people_vaccinated), color = "blue", size = 2) +
  geom_line(data = data3, aes(x = as.Date(date), y = people_fully_vaccinated), color = "red", size = 2) +
  xlab('Dates') +
  ylab('percent.change')+
  scale_x_date(labels = date_format("%m-%d-%Y"))

print(p)


ss <- subset(data3, date > as.Date("2021-01-01"))
ggplot(data = ss, aes(x = date, y = people_vaccinated)) + 
  geom_line(color = "#FC4E07", size = 2)
  
ggplot(data3, aes(x = as.Date(date), y = people_fully_vaccinated))+
  geom_line(color = "#00AFBB", size = 2)+
  scale_x_date(labels = date_format("%m-%d-%Y"))


