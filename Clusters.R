library(dplyr)
library(tidyr)
library(readxl)
library(caret)
library(car)
library(mice)
library(rpart)
library(rpart.plot)
library(ggplot2)

source('/Users/aaqibjabbarhj/Desktop/4th Sem/BA field project/BabsonAnalytics.R')
df = read_excel('/Users/aaqibjabbarhj/Desktop/4th Sem/BA field project/df.new.xlsx')

str(df)

df$Week.Start.Date = NULL
df$Str.Id = NULL
df$Dist.Id = NULL
df$Fcst.Sales=NULL 
df$Fcst.Txns=NULL 
df$Fcst.Items=NULL 
df$Fcst.Hrs=NULL 
df$Fcst.Labor.Cost=NULL 
df$Sched.Hrs=NULL 
df$Sched.Labor.Cost=NULL 
df$Executions.Completed.Ontime=NULL 
df$Executions.Completed.Late=NULL 
df$Executions.Forced.Closed=NULL 
df$Executions.Open=NULL 
df$Sched.Labor.Cost= NULL
df$Act.Sales= NULL
df$Act.Txns= NULL
df$Act.Items= NULL
df$Act.Hrs= NULL
df$Act.Labor.Cost= NULL
df$Sched.Shifts.SYS= NULL
df$Sched.Shifts.MGR= NULL
df$Sched.Efficiency.SYS= NULL
df$Sched.Efficiency.MGR= NULL
df$Sched.Edits= NULL
df$Logons= NULL
df$Executions.Planned= NULL
df$Week.No= NULL


str(df)

#removes missing values
df = na.omit(df)
#removes infinite values
df <- df[is.finite(rowSums(df[ ,-1])),]

#without pre-processing
model = kmeans(df, 4)
#plot(df,col=model$cluster) 

model$centers
model$size


#Pre-processing
standardizer = preProcess(df, method=c("center", "scale"))
df = predict(standardizer, df)

model = kmeans(df, 2)
model$centers
model$size

#determine k value
elbowChart(df)

model$cluster
df = cbind(df,model$cluster)

View(df)
str(df)
----------

library(stats)
library(ggfortify)
autoplot(model,df,frame=TRUE)
