setwd("C:\\Users\\Andrew\\Documents\\SICSS\\FFChallenge_v5")

na_vals <- c("-9", "-6", "-2", "-1", "-3", "NA")
background <- read.csv("background.csv", header=T, na = na_vals)
train <- read.csv("train.csv", header=T, na = na_vals)
prediction <- read.csv("prediction.csv", header=T, na=na_vals)

#merge
df <- merge(background,train,by='challengeID')
df2 <- merge(background, prediction, by='challengeID')

#clean up df
#remove all missing cols
noinfo<-sapply(df,function(x) sum(!is.na(x))==0)
df<-df[,!noinfo]

#missing data as col means
dfmeans <- df



which(names(dfmeans)=='cf1intmon')
for(i in 1:ncol(dfmeans)){
  #i<-2
  if(i%%100==0)
    print(i)
  tmpclass<-class(dfmeans[,i])
  if(tmpclass!='numeric')  {
    dfmeans[,i] <- as.numeric(dfmeans[,i]) 
  }
  dfmeans[is.na(dfmeans[,i]), i] <- mean(dfmeans[,i], na.rm = TRUE)
}

#print dfmeans
write.csv(dfmeans, file = "dfmeans.csv")

#clean up df2
#remove all missing cols
noinfo2<-sapply(df2,function(x) sum(!is.na(x))==0)
df2<-df2[,!noinfo2]

#missing data as col means
df2means <- df2

which(names(df2means)=='cf1intmon')
for(i in 1:ncol(df2means)){
  #i<-2
  if(i%%100==0)
    print(i)
  tmpclass<-class(df2means[,i])
  if(tmpclass!='numeric')  {
    df2means[,i] <- as.numeric(df2means[,i]) 
  }
  df2means[is.na(df2means[,i]), i] <- mean(df2means[,i], na.rm = TRUE)
}

write.csv(df2means, file = "df2means.csv")
