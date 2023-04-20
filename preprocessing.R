df=read.csv("D:/vit/sem5/fda/jcomp/twitchdata r3.csv",header=T)
head(df)
summary(df)
#converting all languages and channel to lower case
levels(factor(df$Language))
df$Language=tolower(df$Language)
df$Channel=tolower(df$Channel)
head(df)

#removing comma from the string in watchtimeminutes
df$Watch.time.Minutes. <- as.numeric(gsub(",","",df$Watch.time.Minutes.))
summary(df)
names(df)


#imputing using mice regression
library(mice)
df$Watch.time.Minutes.=df$Watch.time.Minutes./100000
df$Views.gained=df$Views.gained/100000
imp <- mice(df[,2:8], method = "norm.predict") # Impute data
data_det <- complete(imp)
head(data_det)
df[,2:8]=data_det
df$Watch.time.Minutes.=df$Watch.time.Minutes.*100000
df$Views.gained=df$Views.gained*100000
nrow(df[complete.cases(df),])
df[!complete.cases(df),]
head(df)
plot(df)
cor(df$Watch.time.Minutes.,df$Followers)
cor(df$Views.gained,df$Followers)
cor(df$Views.gained,df$Followers.gained)

#boxplots
boxplot(df$Followers)
df=mutate(df,followergained.total.ratio=Followers.gained/Followers)
boxplot(df$followergained.total.ratio)
df=mutate(df,Watch.Stream.ratio=Watch.time.Minutes./Stream.time.minutes.)
boxplot(df$Watch.Stream.ratio)

#dropping outliers of followers gained to followers ratio
temp=df$followergained.total.ratio
temp <- !temp %in% boxplot.stats(temp)$out
temp
df=df[temp,]
df
head(df)
nrow(df)

write.csv(df,"D:/vit/sem5/fda/jcomp/twitchdata final.csv")
