 
library(ggplot2)
library(reshape2)
#install.packages("corrplot")
library(GGally)
library(plyr)
library("tidyverse")
library("gridExtra")
library("memisc")
library(dplyr)
library(corrplot)
# Load the Data
df <- read.csv('C:/Users/Shefali Kolge/Desktop/WORK/ML/AppleStore.csv')
df_apdes <- read.csv("C:/Users/Shefali Kolge/Desktop/WORK/ML/appleStore_description.csv")
View(df)
# Introduction about the Data set!
#In this document we are going to explore the top trending apps in iOS app store.
#The dataset has more than 11000 Apple iOS mobile application details. 
#The data was extracted from the iTunes Search API at the Apple Inc website.

# Showing summary about the data structures
dim (df)
str(df)
summary(df)
df$X=NULL

# Adding a new variable to include size by MB.
df$size_mb <- df$size_bytes/1024/1024

tf=df%>% mutate(free=ifelse(price>0,0,1))
#View(df)
##Count plot as per free/not free app
ggplot(tf,aes(x=free,fill=as.factor(free)))+geom_bar()+geom_text(stat='count',aes(label=..count..), vjust=-1)+
  ggtitle("Count of Apps Free/Not Free") +xlab("Free/Not Free") +
  ylab("Count") + scale_fill_discrete(name = "",labels=c( "NotFree","Free"))+scale_color_manual(labels = c("T999", "T888"))
#fig1 
#Showing frequency of apps in our data set sorted by their count.
p <- ggplot(data=df,aes(x=fct_infreq(df$prime_genre),fill=prime_genre))
p+geom_bar()+labs(title="Frequncy of Apps based on the category",x="Categories",y="Count")+theme(axis.text.x=element_text(angle=90,hjust=1))+geom_text(stat='count',aes(label=..count..),vjust=-1)


#Let's check a quick summary about free and paid apps
# Creating a new variable is_game to seperate General apps and Games.
df$is_game <- df$prime_genre == 'Games'
df$is_game <- ifelse(df$is_game == TRUE, "Game", "General App")
# Showing summary about the price, also adding a new variable to seperate paid
# and free apps.
#summary1
summary(df$price==0)
df$is_free <- df$price == 0
df$is_free <- ifelse(df$is_free == TRUE, "Free", "Paid")

# Showing frequency of apps in our data set based on app content category.
ggplot(df,aes(cont_rating,fill=cont_rating))+geom_bar()+xlab("Content Type")+ylab("Count")+ggtitle("Frequncy of Apps based on the content type")


# Showing frequncy of Apps based on the user rating above 0
qplot(x=user_rating, data = subset(df,df$user_rating>0),binwidth = 1,fill=user_rating) + 
labs (title="Frequncy of Apps based on the user rating above 0",
x="User Rating",y="Count") 

#df <- subset(df,df$user_rating>0)
#We excluded 0 rating from our apps to better investigate the dataset. 
#Most of apps are rated 4.5 and very few are rated as average 5.


# Create a new variable in our data set that include copy of the data 
# after seperating Games and non-Games.
dfGames <-subset(df,df$is_game == 'Game')
dfGeneral <-subset(df,df$is_game != 'Game')

# Showing top 10 Games based on user rating and total number of ratings
print("Top 10 Games based on user rating and total number of ratings")
head(dfGames[order(dfGames$user_rating,dfGames$rating_count_tot, decreasing= T),
c("track_name","rating_count_tot")], n = 10,data = dfGames)

# Showing top 10 General apps based on user rating and total number of ratings
print("Top 10 General apps based on user rating and total number of ratings")
head(dfGeneral[order(dfGeneral$user_rating,dfGeneral$rating_count_tot, 
decreasing= T),c("track_name","rating_count_tot",
"prime_genre")], n = 10,data = dfGeneral)
# This results are sorted based on user rating first and total ratings count.

# Showing top 10 apps based on total count of user rating
print("Top 10 apps based on total count of user rating")
head(df[order(df$rating_count_tot, decreasing= T),
c("track_name","rating_count_tot")], n = 10,data = df)
# This results are sorted based on user rating first and total ratings count.
# Univariate Analysis
### Structure of the dataset:

#There are 11000 observation about apps in the US iOS app store in our dataset 
#with 16 features.
#Categorical Variables Are: prime_genre, currency, user_rating, cont_rating, 
#is_game.
#Numerical Variables Are: price, rating_count_tot, size_bytes, size_mb, 
#user_rating_ver, ver, rating_count_ver.

#The primary category for most of the observation are about Games, the rest are 
#for general apps.

#Price can be devided into Free or Paid.

#75% of apps are less than $2 in price and have average rating of 4.5
### Main feature(s) of interest in the dataset?
#The main features of interest in our dataset are the app details like 
#price, and user rating.

### Other features in the dataset I think will help support my \
#investigation into the feature(s) of interest?

#Lanugage Supported and size may be useful in future state to determine the 
#relation between ratings.

#All currencies in our dataset are USD. Which means no need to check any other 
#currency for this oarticular dataset.

### What about creating new variables in the dataset?

#I created app_size by mb to better understand the size of the app and 
#how it relects to the user ratings. 
#I also created a variable of "Is free?" to better understand if the app is paid
#or free will reflect into the user ratings?
#I created a new variable is_game .. to cateorize application 
#based if it's a general app or a game.

#I subset the data to exclude data for 0 number ratings. It's not fair to 
#calculate zero ratings in our investigations.

#> **GGally**: Let's have an overview of all variables together, that should 
#give us a quick look to the relation between each variable and the rest of them.

ggpairs(df[,c('price','rating_count_tot',
              'rating_count_ver','user_rating','user_rating_ver',
              'size_mb','is_game','is_free',
              'lang.num','sup_devices.num')]) + theme(axis.text.x=element_text(angle=90,hjust=1))

#> **ggcorrplot**: A quick look to correlation between our numeric columns.

#```{r echo=FALSE, ggcorrplot, message=FALSE, warning=FALSE}
M <-cor(select_if(df[,5:20], is.numeric))
corrplot(M, method="circle")

# Bivariate Plots Section

#Is there a relation between the price of an app and it's mean user rating? 
#Let's deep onto our data set to answer this question ^^
  
# A plotting point showing the relation between price of an app 
# and it's mean user rating.
ggplot(aes(x=user_rating,y=price,fill=user_rating), data = subset(df,price<50)) +
  geom_point() + xlim(c(1,5)) +
  labs (x="Average User Rating", y="Price",
        title="Not strong relation between price of an app and it's mean user rating.")+
  facet_wrap(~is_game) 

#In the above plot, I included prices less than 50 to exclude some few 
#applications their prices over $100. As we see there are some outer points 
#in general apps for 4-5 ratings, which fairly indicates that 
#user rating/quality for a general app may be the reason for increasing the price. 
#This rarely could be happen (4% acuurancy).


# A plot line showing the relation between number of languages supported 
#by an app and it's quality/user rating.
ggplot(aes(x=user_rating,y=lang.num), data = df) +
  geom_line(stat = 'summary', fun.y = mean) +
  labs (x="Average User Rating", y="Number of language supported",
        title="The relation between number of languages supported 
        by an app and it's quality/user rating")

summary(df$lang.num)
#Yes, number of lanuages supported by an app may affect on it's total rating. 
#We will check whether it affects on the price or not in a further analysis.

#75% of our data set have 8 language supported or less. 
#Most of them have only one language supported.

# Showing the relation between number of devices supported by an app and 
#it's quality/user rating
ggplot(aes(y=user_rating,x=df$sup_devices.num), data = df) +
  scale_x_continuous(limits=c(36,47),breaks = seq(36,47,1)) +
  geom_line(stat = 'summary', fun.y = mean) + 
  labs (y="Average User Rating", x="Number of devices supported",
        title="The relation between number of devices supported by an app and 
        it's quality/user rating")

# Printing summary and coorelation between user rating and devices supported.
summary(df$sup_devices.num)
cor(df$user_rating,df$sup_devices.num)

#The minumum number of supported devices is 9. Median is 37 
#75% of our data set have 39 language supported or less. Most of them have 37 
#devices supported.

#Correlation is negative 4/100.

# a boxlot plot for content rating as a categorical value and price for an app.
ggplot(aes(x=cont_rating,y=price), data =subset(df,df$price>0)) +
  ylim(0,25) +
  geom_boxplot() 

summary.factor(df$cont_rating,stats=TRUE)
#All content based apps are nearly have 3 in user rating. 
#There are some slight diffferences but not that much to decide that the content 
#rating may affect on the user ratings! 
  
#  * 25% of 12+ apps have prices less than $5, 
#* 9+ and 17+ are nearly the same in regarding of the price.
#* 4+ apps which include high number of games are less than $3.
#There are some ouliers for apps high than $7.


#Q: Which category has the most highgest rating?
  
# A histogram showing the relation between user ratings and an app category.
ggplot(aes(x=prime_genre,y=user_rating,fill=prime_genre), data = df) +
  geom_histogram(stat = 'summary', fun.y = mean) + coord_flip() +
  labs (x="Categories", y="Average of user rating",
        title="Does mean user ratings  depend on app category?") + 
  facet_wrap(~is_free) + theme_minimal()
#* Productivity and Music have the highest average rating in free apps.
#* Cataloges and Shopping have the highest average rating in paid apps.

#However,

#We see that books in paid apps have high mean rating, however very less in 
#free apps. The same in Catalogs.


#Q: Which category has the highgest total number of ratings?
  
# A histogram showing total number of user ratings and app category.
ggplot(aes(x=prime_genre,y=rating_count_tot,fill=prime_genre), data = df) +
  geom_bar(stat = 'summary', fun.y = mean) + coord_flip() +
  labs (x="Categories", y="Average of total number of user rating",
        title="Does mean of total number of user ratings depend on app category?") + 
  facet_wrap(~is_free) + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) + theme_minimal()


#WoW! The magic begins to appear!
#  Users don't give feedback or rating to paid apps. However they do in free apps!


#Let's check if price may affect on app category?
  
# A histogram plot answering  question of 
# Which category is more expensive than other apps?
ggplot(aes(x=prime_genre,y=price,fill=prime_genre), data = df) +
  geom_histogram(stat = 'summary', fun.y = mean) + coord_flip() + 
  labs (x="Categories", y="Average Price",
        title="Which category is more expensive than other apps?") + 
  theme_minimal()

#Someting strange, Medical category are the most expensive ones! 
 # Yes the category field can affect on the price of the app!!
  #Shopping apps have very less price than other apps



# line plot showing the relation between current version rating 
# and average user rating for an app.
ggplot(aes(x=user_rating_ver,y=user_rating), data = df) +
  geom_line(stat = 'summary', fun.y = median) + geom_smooth() + 
  labs (x="User rating of recent version", y="Total average user rating",
        title="Does the current version is always have more rating than 
        the total overall rating?")

#Correlation between current version user rating and total 
#overall user rating is 0.7 which leads to a strong positive 
#correlation between them as the above plot shows.


# The relation between app size and price or user_rating
ggplot(aes(x=price,y=size_mb), data = subset(df,df$price<25&&df$price>0)) +
  geom_boxplot(alpha=1/10) + scale_x_continuous(limits = c(0,10),
                                                breaks = seq(0,10,2)) + 
  scale_y_continuous(limits = c(0,1000), breaks = seq(0,1000,100)) + 
  facet_wrap(~is_game) + labs (x="Price", y="Size (MB)",
                               title="The relation between app size and price or user_rating")

#* Most of paid games are less than $5.
#* For Games, price decreases when size of the app getting lower, 
#however in general apps there are no clear relation between them.

#fig3
# A histogram showung the summary of size_mb (MEAN) and user rating.
ggplot(aes(x=user_rating,y=size_mb,fill=user_rating), data = subset(df,df$user_rating>0)) +
  geom_histogram(stat = 'summary', fun.y = mean) +
  scale_x_continuous(breaks = seq(1.5,4.5,.5),limits = c(1,5))




#from fig3
# Multivariate Plots Section


# Showing the relation between total count ratings, 
# price and user rating
ggplot(aes(x=log10(rating_count_tot),y=price), data = subset(df,df$price<20&df$rating_count_tot),bandwidth= 1) +
  geom_boxplot(aes(color = factor(user_rating))) + 
  labs (y="Price", x="Log10 transformed total count of ratings ",title="The relation between total count ratings, price and user rating")

print("Summary of total number of ratings /n")
summary(log10(df$rating_count_tot))


#* We can conclude from the plot above that most of good rated apps are cheap and
#they also have been rated with large number of users.


# Showing two plots for size and total number of ratings and price 
# colored by user rating
q1<- ggplot(aes(x=size_mb,y=price), data = subset(df,df$price<25)) +
  geom_point(aes(color = factor(user_rating))) + coord_flip() + labs (y="Price", x="Size (MB)" )

q2<- ggplot(aes(x=log10(rating_count_tot),y=price), 
            data = subset(df,df$price<25))+geom_point(aes(color = factor(user_rating))) + 
  labs (y="Price", x="Transformed log10 of total count of ratings " ) + coord_flip()

grid.arrange(q1,q2,ncol=2)

#* We see that prices under $10 their sizes vary, 
#their user rating stay good while sizes increase. 
#* Overall, average user rating remain good though the number 
#of total ratings increase.

# Relation between price and different category colored by user rating to define
# which category has more price in comparing of user rating.
ggplot(aes(x=prime_genre,y=price), data = df) +
  geom_line(aes(color = factor(user_rating))) + 
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(y="Price",x="Categories")


#Utilities are expensive apps which have low user ratings.

# The relation and correlation between user rating and total number of ratings.
ggplot(aes(x=prime_genre,y=log10(rating_count_tot)), data = df) +
  geom_line(aes(color = factor(user_rating))) + 
  labs(y="Total number of ratings",x="Categories") + 
  theme(axis.text.x=element_text(angle=90,hjust=1))

print ("Correlation between user rating and total number of ratings")
cor(df$rating_count_tot,df$user_rating)


#* We see from the plot above that total rating count have a small effect on 
#the user rating for different categories.


ggplot(aes(x=rating_count_tot,y=user_rating), data = df) +
  geom_point(aes(color = is_free)) + coord_flip()

# Multivariate Analysis

# Low prices apps are not good enough in general, the user rating increases in  
# most of category when the price increases; specially in Navigation, Education 
# and References apps. 

# * By plotting price and size of the app per MB show that apps have nearly price.
# i.e: $5, $10 or $15.
# * Low prices of Navigation apps are not good. 
# Moderate expensive music apps have good rating though.
# 
# 
# * Users love to give good rating when they notice the app already has a good
# rating. That's why while total number of rating increases, the average of 
# user_rating increases too.
# 
# ------

#here from this fig 5
# Final Plots and Summary

### plot1
# Plotting final summary
# Prices based on categories
ggplot(aes(x=prime_genre,y=price), data = df) + 
geom_histogram(stat = 'summary', fun.y = mean) + coord_flip() + 
labs (x="Category", y="Price",title="Prices based on categories")

# User rating based on categories
ggplot(aes(x=prime_genre,y=user_rating,,fill=prime_genre), data = df) +
geom_histogram(stat = 'summary', fun.y = mean) + coord_flip() +
labs (x="Category", y="User Rating",title="User rating based on categories") +
facet_wrap(~is_free) 

### Description One
# * Medical app category is the most expensive ones, 
# may be because they provide valuable information?
# * The category field can affect on the price of the app!! 
# Shopping and finance apps have less price than other apps. Make sense because 
# they provide services and users already pay to use the service.

### Plot Two

# line plot showing avg User rating vs Rating for recent version
ggplot(aes(x=user_rating_ver,y=user_rating), data = df) + 
geom_line(stat = 'summary', fun.y = median) + geom_smooth() +
labs (x="User Rating for last version", y="Avg User Rating",
title="Avg User rating vs Rating for recent version") 


# ### Description Two
# Newer versions of most of the apps have better rating than the median rating. 
# Developers always try to publish a better app that worth a better rating always. 
# Correlation is 0.7 positive between twose two variables.

### Plot Three

# points plot showing the relation between app price based on size
ggplot(aes(x=size_mb,y=price), data = subset(df,df$user_rating>0)) +
geom_point(aes(color = factor(user_rating))) + coord_flip() + 
ylim(limits = c(0,10)) +
labs (x="Size (MB)", y="Price",title="App price based on size") 


# Creating a linear model to predict app price or user rating for an app.
m1 <- lm(user_rating~price,df)
m2 <- update(m1, ~ . + price)
m3 <- update(m2, ~ . + prime_genre)
m4 <- update(m3, ~ . + cont_rating)
m5 <- update(m4, ~ . + size_mb)
m6 <- update(m5, ~ . + lang.num)
mtable(m1,m2,m3,m4,m5,m6,sdigits = 3)

xx <- data.frame(prime_genre="Social Networking",
                 price=0,cont_rating="4+",size_mb=100,lang.num=2)
yy <- predict (m6, newdata = xx, interval="prediction",level = .90)
exp(yy)

install.packages("tree")
library(tree)
apps.tree <- tree(user_rating~size_bytes+price+sup_devices.num+ipadSc_urls.num+lang.num, data = df) 
apps.tree
summary(apps.tree)
plot(apps.tree)
text(apps.tree, pretty = 0)

df$track_name[df$ipadSc_urls.num > 3.5 & df$lang.num > 3.5]
df$track_name[df$ipadSc_urls.num > 3.5 & df$lang.num > 3.5]

# In order to maximise the average user rating across all versions, need to
# have more than three screenshots and more than three supported languages.
# Average user rating is still good with less than supported languages.
# If you provide three or fewer screenshots, then need to offer more than four
# supported languages.

# If less screenshots and less supported languages, then need to increase price
# to more than 50p to get average user rating of 3. Otherwise, need to increase
# the size in bytes to get user rating of 3. The worse user rating is for free apps
# with low bytesize.

# This suggests that the number of screenshots and number of supported languages
# attract more users, whilst price and size are indicators of functionality.
# If it has poor functionality, it won't rate well.

appsver.tree <- tree(user_rating_ver~size_bytes+price+sup_devices.num+ipadSc_urls.num+lang.num, data = df) 
appsver.tree
summary(appsver.tree)
plot(appsver.tree)
text(appsver.tree, pretty = 0)

# Just taking the current version of the app into account, the maximum average user rating 
# is for more than three screenshots and supporte d languages. But, oddly, if there
# are three or fewer supported languages, then apps with one supported language
# are more likely to have a higher user rating than apps with two or three 
# supported languages.

# If there are three or fewer screenshots, then there needs to be more than five
# supported languages to increase the user rating. 

# What should I price my app at?

price.lm <- lm(price~size_bytes+sup_devices.num+ipadSc_urls.num+lang.num+prime_genre, data = df)
price.lm
summary(price.lm)

cor(df$price, df$size_bytes)
cor(df$price, df$ipadSc_urls.num)
cor(df$price, df$sup_devices.num)
cor(df$price, df$lang.num)

# The attributes that seem to linked to price are number of screenshots, number of 
# supported devices, size and the number of supported languages. 
# Price decreases as the number of supported devices or languages increase. 
# Price increases as the size and number of screenshots increase.
# Howevr, in all four cases, the corellation is weak.

genreprice.glm <- glm(prime_genre~price+size_bytes+sup_devices.num+ipadSc_urls.num+lang.num, data = df, family = binomial)
summary(genreprice.glm)

# There is a relationship between prime genre of app and the number of screenshots or the number of supported languages
########################################################################################################

all_df=merge(x =df, y = df_apdes, by = "id", all.x = TRUE)
#View(all_df)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
colnames(df_apdes)
library(dplyr)
View(all_df)
all_df%>%select("app_desc")
desc_rating<-all_df%>%filter(user_rating>4)
View(desc_rating$app_desc)
docs <- Corpus(VectorSource(desc_rating$app_desc))
inspect(docs)

docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
inspect(docs)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)


dtm <- TermDocumentMatrix(docs)
View(dtm)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
View(d)

#########################

df$revenue=df$rating_count_tot*df$price
View(df)
#rating_count_tot * user_rating can tell user favourites and
rating_count_ver * user_rating_ver
#will tell us the favourites in current version
df$favourites_tot = df$rating_count_tot * df$user_rating
df$favourites_ver = df$rating_count_ver * df$user_rating_ver



################
set.seed(123)
nrow(df)
?sample_n
index=sample(1:nrow(df),0.7*nrow(df))
View(index)
train=df[index,]
test=df[-index,]
View(train)

str(df)
df$ver=as.character(df$ver)
df$track_name=as.character(df$track_name)
df$ver=as.numeric(df$ver)
df$currency=as.character(df$currency)
library(dummies)
df_new= dummy.data.frame(as.data.frame(df$prime_genre), sep = ".")

View(df_new)

app_final=cbind(df_new,df)
View(app_final)
names(students.new)

app_final=app_final%>%
  mutate(size_bin=ifelse(size_mb<=5,1,ifelse(size_mb<=10,2,ifelse(size_mb<=50,3,ifelse(size_mb<=100,4,ifelse(size_mb<500,5,6))))))

library(randomForest)

set.seed(123)
nrow(df)
?sample_n
index=sample(1:nrow(app_final),0.7*nrow(app_final))
View(index)
train=app_final[index,]
test=app_final[-index,]
colnames(train)
parameters=c("prime_genre.Book" ,
             "prime_genre.Business","prime_genre.Catalogs" ,"prime_genre.Education"
             ,
             "prime_genre.Entertainment", "prime_genre.Finance",
             "prime_genre.Food & Drink"  , "prime_genre.Games",
             "prime_genre.Health & Fitness" ,"prime_genre.Lifestyle",
             "prime_genre.Medical" , "prime_genre.Music",
             "prime_genre.Navigation", "prime_genre.News",
             "prime_genre.Photo & Video" , "prime_genre.Productivity",
             "prime_genre.Reference" ,"prime_genre.Shopping",
             "prime_genre.Social Networking" ,"prime_genre.Sports",
             "prime_genre.Travel","prime_genre.Utilities",
             "prime_genre.Weather", "id",
             #track_name,,
             "price",
             #rating_count_tot  ,rating_count_ver,
             "user_rating"    ,"user_rating_ver",
             "vercont_rating","sup_devices.num",
             "ipadSc_urls.numlang.num",
             #vpp_lic ,
             "size_mb"
             #favourites_tot ,favourites_ver
)
str(train)
model1 <- randomForest(user_rating ~
                         .-ver-prime_genre-track_name-currency, data = train, importance =
                         TRUE)
model1

summary(train)
colSums(is.na(train))
View(train$user_rating)
class(train$user_rating)


train=df[index,]
test=df[-index,]
str(train)
train1=train[,-c(2,4,10)]
model1 <- randomForest(user_rating ~., data = train1, importance =
                         TRUE,na.action=na.roughfix)
model1