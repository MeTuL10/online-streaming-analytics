df=read.csv("D:/vit/sem5/fda/jcomp/twitchdata final.csv",header=T)
head(df)
df=df[,-1]
head(df)
nrow(df)
#################################################################################### IGNORE THESE
#adding new attribute
df=mutate(df,Watch.Stream.ratio=Watch.time.Minutes./Stream.time.minutes.)
head(df)
head(arrange(df,-Watch.time.Minutes.))
head(arrange(df,-Watch.Stream.ratio))
head(arrange(df,-Peak.viewers))

#adding new attribute, ratio of followers gained to total
df=mutate(df,followergained.total.ratio=Followers.gained/Followers)
head(arrange(df,-followergained.total.ratio))     
########################################################################################

#pie plots
data=as.numeric(factor(df$Partnered))
data=table(data)
data
pie(data,labels=levels(factor(df$Partnered)),main="partnered streamers")

data=as.numeric(factor(df$Language))
data=table(data)
data
pie(data,labels=levels(factor(df$Language)),main="languages")

#scatter plots
plot(df$Average.viewers,df$Watch.time.Minutes.)
cor(df$Average.viewers,df$Watch.time.Minutes.)

plot(df)
library(e1071)
skewness(df$Followers.gained)
head(df)


###################################################################################

#K MEDOIDS  -- clustering the streamers into 4 tiers(S,A,B,C) based on attributes
library(factoextra)
library(cluster)
#choosing the attributes for finding cluster
df2=select(df,c(Channel,Watch.time.Minutes.,Followers,Followers.gained,Average.viewers,Partnered,Language,Views.gained))
lang=factor(df2$Language)
df2$Language=as.numeric(factor(df2$Language))
partner=factor(df2$Partnered)
df2$Partnered=as.numeric(factor(df2$Partnered))
#normalizing all atributes
dfk=scale(df2[,2:8])
head(dfk)

#finding appropriate number of clusters required
fviz_nbclust(dfk, pam, method = "wss")

#performing kmedoids to cluster streamers into different tiers
kmed=pam(dfk,k=4)
summary(kmed)
#plotting the clusters using first 2 principal components
fviz_cluster(kmed,data=dfk)
channel.Tier=kmed[3]
#adding the channeltier(cluster centers) to the dataframe
dfcluster=data.frame(df2,channel.Tier)
head(dfcluster)
#table(dfcluster$Partnered)
dfcluster=rename(dfcluster,channel.Tier=clustering)
dfcluster=arrange(dfcluster,channel.Tier)

dfcluster$channel.Tier=factor(dfcluster$channel.Tier)
label=c("S","A","B","C")
levels(dfcluster$channel.Tier)=label
dfcluster$Language=factor(dfcluster$Language)
levels(dfcluster$Language)=lang
head(dfcluster)
dfcluster$Partnered[dfcluster$Partnered==1]="NO"
dfcluster$Partnered[dfcluster$Partnered==2]="YES"
head(dfcluster)
tail(dfcluster)
table(dfcluster$Partnered)
tiercount=table(channel.Tier)
tiercount

#plotting number of streamers in each tier
barplot(tiercount,names.arg=label,main="number of channels in each tier")
ggplot(dfcluster, aes(x=factor(channel.Tier)))+ geom_bar(stat="count")+ annotate("text", x = 2.5, y = 550, label = "CHANNEL TIER COUNT")

stier=filter(dfcluster,channel.Tier=="S")
atier=filter(dfcluster,channel.Tier=="A")
btier=filter(dfcluster,channel.Tier=="B")
ctier=filter(dfcluster,channel.Tier=="C")
#dtier=filter(dfcluster,channel.Tier=="D")
#etier=filter(dfcluster,channel.Tier=="E")
summary(stier)
summary(atier)
summary(btier)
summary(ctier)





#####################################################
#regression
install.packages('caTools')
library(caTools)
split = sample.split(df$Views.gained, SplitRatio = 0.8)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)
head(df)
#views gained as output and the other 4 attributes as input
model=lm(Views.gained~Followers+Followers.gained+Average.viewers+Watch.time.Minutes.,training_set)
model

library(car)
avPlots(model)

summary(model)
head(test_set)
predict(model,newdata=test_set)



