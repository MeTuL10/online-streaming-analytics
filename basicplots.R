df=read.csv("D:/vit/sem5/fda/jcomp/twitchdata final.csv",header=T)
head(df)
df=df[,-1]
head(df)
nrow(df)
#adding new attribute
df=mutate(df,Watch.Stream.ratio=Watch.time.Minutes./Stream.time.minutes.)
head(df)
head(arrange(df,-Watch.time.Minutes.))
head(arrange(df,-Watch.Stream.ratio))
head(arrange(df,-Peak.viewers))

#adding new attribute, ratio of followers gained to total
df=mutate(df,followergained.total.ratio=Followers.gained/Followers)
head(arrange(df,-followergained.total.ratio))     

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




