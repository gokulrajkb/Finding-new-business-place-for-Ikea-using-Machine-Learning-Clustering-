############################################################################
############################################################################

#From,

#      Gokul Kaisaravalli Bhojraj
#      Id: 80789
#      Business Intelligence

############################################################################
############################################################################

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

###############################################################

library(plyr)
library(readr)

unzip("C:/Users/Gokul/Desktop/Bi lab 4/data.zip",unzip = "internal") 

Data_read<-read.delim("C:/Users/Gokul/Desktop/Bi lab 4/data.txt")
dim(Data_read)

Data = na.omit(Data_read)
dim(Data)

Data$Kommun_name<-iconv(Data$Kommun_name,to = "UTF-8",from = "latin-9")

# given city having ikea store
IKEA_stores <-c("Borlänge", "Gävle", "Göteborg", "Haparanda", "Helsingborg" , "Jönköping", "Kalmar", "Karlstad", "Linköping", "Malmö", "Stockholm", "Sundsvall", "Uddevalla", "Umeå", "Uppsala", "Västerås", "Älmhult",  "Örebro")

# create Ikea status variable assign value 1 for city having ikea store and 0 otherwise
Data$IKEA_status<-ifelse(Data$Kommun_name%in% IKEA_stores ,1,0)


library(corrplot)
correlation<-round(cor(Data[4:10]),2)
correlation
corrplot(correlation,type= "upper", order="hclust",t1.col="black",t1.srt=45)

Ikea_present<-subset(Data,Data$IKEA_status==1)
Ikea_present

sort(Ikea_present$Population)
sort(Ikea_present$Revenue)

correlation<-round(cor(Ikea_present[4:10]),2)
correlation
corrplot(correlation,type= "upper", order="hclust",t1.col="black",t1.srt=45)

Data_1<-as.matrix(scale(Data[4:10]))
Data_1
dim(Data_1)

# we keep number of iter.max=15 to ensure the algorithm 
# converges and nstart=50 to #ensure that atleat 50 random
# sets are choosen  

#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- Data_1
wss <- sapply(1:k.max,function(k)
                    {
                     kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss

plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

kmm = kmeans(data,5,nstart = 50,iter.max = 15)
kmm

cluster_no<-kmm$cluster
cluster_no

Data<-cbind(Data,cluster_no)


library(factoextra)
library(cluster)

fviz_cluster(kmm,data=Data_1)

###################################################3

clus_1<-subset(Data,Data$cluster_no==1)
clus_1_mean<-c(mean(clus_1$Revenue),mean(clus_1$Employee),mean(clus_1$Population),
mean(clus_1$Population_University),mean(clus_1$Percent_University),
mean(clus_1$Productivity),mean(clus_1$SalesIndex))
clus_1_mean<-append(clus_1_mean,sum(clus_1_mean))

clus_2<-subset(Data,Data$cluster_no==2)
clus_2_mean<-c(mean(clus_2$Revenue),mean(clus_2$Employee),mean(clus_2$Population),
               mean(clus_2$Population_University),mean(clus_2$Percent_University),
               mean(clus_2$Productivity),mean(clus_2$SalesIndex))
clus_2_mean<-append(clus_2_mean,sum(clus_2_mean))

clus_3<-subset(Data,Data$cluster_no==3)
clus_3_mean<-c(mean(clus_3$Revenue),mean(clus_3$Employee),mean(clus_3$Population),
               mean(clus_3$Population_University),mean(clus_1$Percent_University),
               mean(clus_3$Productivity),mean(clus_3$SalesIndex))
clus_3_mean<-append(clus_3_mean,sum(clus_3_mean))

clus_4<-subset(Data,Data$cluster_no==4)
clus_4_mean<-c(mean(clus_4$Revenue),mean(clus_4$Employee),mean(clus_4$Population),
               mean(clus_4$Population_University),mean(clus_4$Percent_University),
               mean(clus_4$Productivity),mean(clus_4$SalesIndex))
clus_4_mean<-append(clus_4_mean,sum(clus_4_mean))

clus_5<-subset(Data,Data$cluster_no==5)
clus_5_mean<-c(mean(clus_5$Revenue),mean(clus_5$Employee),mean(clus_5$Population),
               mean(clus_5$Population_University),mean(clus_5$Percent_University),
               mean(clus_5$Productivity),mean(clus_5$SalesIndex))
clus_5_mean<-append(clus_5_mean,sum(clus_5_mean))

Attribute<-c("Revenue","Employee","Population","Population_University","Percent_University","Productivity","SalesIndex","sum")
All_cal_all<-data.frame(Attribute,clus_1_mean,clus_2_mean,clus_3_mean,clus_4_mean,clus_5_mean)
All_cal<-All_cal_all[,-5]

highest<-c(max(All_cal[1,-1]),max(All_cal[2,-1]),max(All_cal[3,-1]),
           max(All_cal[4,-1]),max(All_cal[5,-1]),max(All_cal[6,-1]),
           max(All_cal[7,-1]),max(All_cal[8,-1]))
All_cal<-cbind(All_cal,highest)


# selection was selected

library(dplyr)
Cluster_Data <- filter(Data, Data$cluster_no== 5 & Data$IKEA_status==0)
View(Cluster_Data)

library(plotly)

plot_Data <- data.frame(Cluster_Data$Kommun_name,Cluster_Data$Population,Cluster_Data$Revenue)
colnames(plot_Data)<-c("KommunName","Population","Revenue")

plot_ly(plot_Data, x = ~KommunName, y = ~Revenue, type = 'bar', name = 'Revenue') %>% 
  add_trace(y = ~Population/100, name = 'Population') %>% 
  layout(yaxis = list(title = ''), barmode = 'group')

##############################################

# Analysis

Chosen_clus_1<-c( "Falkenberg","Eskilstuna","Kristianstad",
                  	"Skövde","Södertälje","Kungsbacka",
                  	"Karlskrona","Burlöv")
                  
Chosen_clus_2<-c("Solna","Partille","Alingsås")

Chosen_clus_3<-c("Varberg","Haninge","Strömstad","Norrtälje")

Chosen_clus_5<-c("Huddinge","Järfälla","Norrköping","Mölndal",
                "Lund","Borås","Växjö","Täby","Halmstad","Nacka")

chosen_all_clus<-c(Chosen_clus_1,Chosen_clus_2,Chosen_clus_3,
                   Chosen_clus_5)

Chosen_data<-NULL
chosen_row<-NULL
k<-1
for(i in 1:length(Data$Kommun_code))
{
        chosen_row<-subset(Data,Data$Kommun_name==chosen_all_clus[k])
        Chosen_data<-rbind(Chosen_data,chosen_row)
        k<-k+1
}
Chosen_data

Final<-data.frame(Chosen_data[,3])
Final<-cbind(Final,Chosen_data[,6])
Final<-cbind(Final,Chosen_data[,4])
Final<-cbind(Final,Chosen_data[,14])
colnames(Final)<-c("City","Population","Revenue","Cluster")
Final

subset(Final,Final$Population>=75000)

Final_1<-Final[order(Final$Population),]
View(Final_1)