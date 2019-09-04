library(tidyverse)
library(plyr)
library(dplyr)
library(magrittr)

gplay <- read.csv("C:/Users/Shefali Kolge/Desktop/WORK/ML/googleplaystore.csv")
View(gplay)
dim(gplay)

colnames(gplay)
names(gplay) <- make.names(names(gplay), unique=TRUE)
str(gplay)

# Data Cleaning

gplay$Last.Updated[gplay$Last.Updated=="1.0.19 April 1"]= NULL

gplay$Last.Updated <- as.character(gplay$Last.Updated)
gplay$Last.Updated <- as.Date(gplay$Last.Updated,format="%B %d, %Y",tryFormats = "%Y %m %B")

gplay$Rating <- as.numeric(gplay$Rating)
gplay$Reviews <- as.numeric(gplay$Reviews)

# For Installs
gplay$Installs <- gsub(",", "", gsub("\\.", "", gplay$Installs))
gplay$Installs <- as.character(gplay$Installs)
gplay$Installs = substr(gplay$Installs,1,nchar(gplay$Installs)-1)

gplay$Installs <- as.numeric(gplay$Installs)

# For Size
gplay$Size <- gsub(",", "", gsub("\\.", "", gplay$Size))
gplay$Size <- as.character(gplay$Size)
gplay$Size = substr(gplay$Size,1,nchar(gplay$Size)-1)

gplay$Size <- as.numeric(gplay$Size)


gplay$App <- as.character(gplay$App)

gplay$Category = as.character(gplay$Category)
gplay$Category[gplay$Category=="1.9"]<-NA
gplay$Category <- as.factor(gplay$Category)

gplay$Type = as.character(gplay$Type)
gplay$Type[gplay$Type=="0"]<-NA
gplay$Type[gplay$Type=="NaN"] <- NA
gplay$Type <- as.factor(gplay$Type)

gplay$Price <- as.character(gplay$Price)
gplay$Price = as.numeric(gsub("\\$", "", gplay$Price))

gplay$Genres = as.character(gplay$Genres)

gplay$Month <- format(gplay$Last.Updated, "%b")
gplay$Month <- factor(gplay$Month, labels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))

#No. of apps last updated each month
table(gplay$Month)

gplay<-unique(gplay)
str(gplay)

summary(gplay)
colnames(gplay)
gplay<- na.omit(gplay)
sum(is.na(gplay))

summary(gplay$Rating)

##Let us analyze the type of Apps and their average rating
cat = gplay %>% group_by(Category) %>% select(c(App, Category, Rating))
cat = as.data.frame(cat)
str(cat$Rating)
table(cat$Rating)
#Checking for NA values
sum(is.na(cat$Rating))

#list of average rating per category
list<-aggregate(cat[,3], list(cat$Category), mean,na.rm=TRUE,  na.action=na.pass)
View(list)

table(gplay$Type)
#Analyzing the Type of Apps and their Average Rating 
type<- gplay %>% group_by(Type) %>% select(c(Rating,Type, Installs, Category, Price))
type=as.data.frame(type)
View(type)

table(gplay$Type)

#Installations by Type and Categories
ggplot(type, aes(x=type$Type, y=type$Installs, fill=type$Type))+geom_bar(stat="identity")+labs(x="Type",y="Installs",fill="Types",title="Installations by Type of Apps")

ggplot(type, aes(x=type$Category, y=type$Installs, fill=type$Category))+ geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, vjust=0.5))+ labs(title="Installations by Category of Apps",x="Categories",y="Installs",fill="Categories")

#Since Game has more installations lets see the most installed Apps under it
gp1<- subset(gplay, Category=="GAME", select = c(App, Installs, Rating))
gp1<-top_n(gp1, 10)
ggplot(gp1, aes(x=gp1$App, y=gp1$Installs, fill=gp1$Rating))+geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, vjust=0.5))+ labs(title="Most installed Apps under Game Category",subtitle="(All rated above 5)",x="Apps",y="Installs",fill="Rating")


#Most Family Apps installed
gp2<- subset(gplay, Category=="FAMILY", select = c(App, Installs, Rating))
gp2<-top_n(gp2, 10)
ggplot(gp2, aes(x=gp2$App, y=gp2$Installs, fill=gp2$Rating))+geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, vjust=0.5))+ labs(title="Most installed Apps under Family Category",subtitle="(All rated above 5)",x="Apps",y="Installs",fill="Rating")

#Lets see how are the App's Priced?
ggplot(gplay, aes(x=gplay$Price))+geom_density(linetype="dashed", color="Black", fill="blue")+ theme(axis.text.x = element_text(angle = 90, vjust=0.5))+labs(title="Lets see how are the App's Priced?",x="App Prices")

paid_data<- subset(gplay, Type=="Paid", select = c(App, Price, Rating,Installs))
ggplot(paid_data, aes(x=paid_data$Installs))+geom_density(color="black",fill="blue")+labs(title="Installation density of Paid Apps",x="Installs")

#Let's Analyze content rating and Installs
ggplot(gplay, aes(x=gplay$Content.Rating, y=gplay$Installs, fill=gplay$Installs))+geom_bar(stat="identity")+labs(title="Content Rating and Installs",subtitle= "Analysis",x="Content Rating",y="Installs",fill="Installs")


# check what category has the highest rating

ggplot(gplay, aes(x=Rating, y=Category)) +
  geom_segment(aes(yend=Category), xend=0, colour="grey50") +
  geom_point(size=1, aes(colour=Type)) +
  scale_colour_brewer(palette="Set1", limits=c("Free", "Paid"), guide=FALSE) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  facet_grid(Type ~ ., scales="free_y", space="free_y") +
  ggtitle("Checking which Category has the highest Rating between Free and Paid Apps")

#Ratings
ggplot(gplay, aes(x= Category, y= Rating, fill = Type)) +
  geom_bar(position='dodge',stat='identity') +
  coord_flip() +
  ggtitle("Number Of App Ratings Based On Category and Type")

#Reviews
ggplot(gplay, aes(x= Category, y= Reviews, fill = Type)) +
  geom_bar(position='dodge',stat='identity') +
  coord_flip() +
  ggtitle("Number Of App Reviews Based On Category and Type")

#Installs
ggplot(gplay, aes(x= Category, y= Installs, fill = Type)) +
  geom_bar(position='dodge',stat='identity') +
  coord_flip() +
  ggtitle("Number Of App Installs Based On Category and Type")
