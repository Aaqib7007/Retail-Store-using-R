library(dplyr)
library(tidyr)
library(readxl)
library(rpart.plot)
library(readr)
library(ggplot2)
library(caret)
library(car)
library(mice)
source('/Users/aaqibjabbarhj/Desktop/4th Sem/BA field project/BabsonAnalytics.R')

df = read_excel('/Users/aaqibjabbarhj/Desktop/4th Sem/BA field project/df.new.xlsx')
locations <- read_excel ('/Users/aaqibjabbarhj/Desktop/4th Sem/BA field project/All files/Company data/Store Locations.xlsx')
df = df %>% merge(locations, by = c("Str.Id"))
str(df)

df$Overperformance=NULL

df$SalesPerLH = df$Act.Sales/df$Act.Hrs
df$FcstSalesPerLH = df$Fcst.Sales/df$Fcst.Hrs
df$Performance = df$SalesPerLH-df$FcstSalesPerLH
df$Overperform = df$Performance>0
df$Overperform = as.integer(df$Overperform)
df$EditsPerLH =df$Sched.Edits/df$Sched.Shifts.SYS
df$ShiftRatio = df$Sched.Shifts.MGR/df$Sched.Shifts.SYS
df$EfficiencyRatio = df$Sched.Efficiency.MGR/df$Sched.Efficiency.SYS
df$ShiftDifference = df$Sched.Shifts.MGR-df$Sched.Shifts.SYS
df$EfficiencyDifference = df$Sched.Efficiency.MGR-df$Sched.Efficiency.SYS
df$LogonsPerLH = df$Logons.hour
df$OnTimePercentage = df$Executions.Completed.Ontime/(df$Executions.Planned)
#stores forced closed are excluded for reasons aaqib knows

df$Overperform=as.factor(df$Overperform)

#District Trees Plots
x = rpart.plot(pruned)
B=data.frame('Full' = x$coefficients[,1])
errorRates=list()
errorBenchs= list()
PercentageImprovements= list()

for (distId in unique(df$Dist.Id)){
  dfDist = df[df$Dist.Id == distId, ]
  N = nrow(dfDist)
  trainingSize  = round(N*0.6)
  trainingCases = sample(N, trainingSize)
  training  = dfDist[trainingCases,]
  test      = dfDist[-trainingCases,]
  #model = rpart(Overperform ~ EditsPerLH + ShiftRatio + EfficiencyRatio + ShiftDifference + EfficiencyDifference + Logons.per.hour , data=training)
  stoppingRules = rpart.control(minsplit=2, minbucket=1, cp = 0)
  overfit = rpart(Overperform ~ EditsPerLH + ShiftRatio + EfficiencyRatio + ShiftDifference + EfficiencyDifference + Logons.per.hour , data=training, control=stoppingRules)
  pruned = easyPrune(overfit)
  predPruned = predict(pruned, test, type="class")
  errorRate = sum(predPruned != test$Overperform)/nrow(test)
  errorBench = benchmarkErrorRate(training$Overperform, test$Overperform)
  PercentageImprovement = ((errorBench-errorRate)/errorBench)*100
  print(distId)
  #rpart.plot(pruned)
  print(N)
  print(errorRate)
  print(errorBench)
  print(PercentageImprovement)
  errorRates[[length(errorRates)+1]] = errorRate
  PercentageImprovements[[length(PercentageImprovements)+1]] = PercentageImprovement
  errorBenchs[[length(errorBenchs)+1]] = errorBench
  }

errorRates=unlist(errorRates)
errorBenchs= unlist(errorBenchs)
PercentageImprovements = unlist(PercentageImprovements)

hist(PercentageImprovements, breaks=20)


#State Trees Plots
x = rpart.plot(pruned)
B=data.frame('Full' = x$coefficients[,1])
errorRates=list()
errorBenchs= list()
PercentageImprovements= list()
for (stateid in unique(df$State)){
  dfState = df[df$State == stateid, ]
  N = nrow(dfState)
  trainingSize  = round(N*0.6)
  trainingCases = sample(N, trainingSize)
  training  = df[trainingCases,]
  test      = df[-trainingCases,]
  #model = rpart(Overperform ~ EditsPerLH + ShiftRatio + EfficiencyRatio + ShiftDifference + EfficiencyDifference + Logons.per.hour , data=training)
  stoppingRules = rpart.control(minsplit=2, minbucket=1, cp = 0)
  overfit = rpart(Overperform ~ EditsPerLH + ShiftRatio + EfficiencyRatio + ShiftDifference + EfficiencyDifference + Logons.per.hour , data=training, control=stoppingRules)
  pruned = easyPrune(overfit)
  predPruned = predict(pruned, test, type="class")
  errorRate = sum(predPruned != test$Overperform)/nrow(test)
  errorBench = benchmarkErrorRate(training$Overperform, test$Overperform)
  PercentageImprovement = ((errorBench-errorRate)/errorBench)*100
  print(stateid)
  rpart.plot(pruned)
  print(N)
  print(errorRate)
  print(errorBench)
  print(PercentageImprovement)
  errorRates[[length(errorRates)+1]] = errorRate
  PercentageImprovements[[length(PercentageImprovements)+1]] = PercentageImprovement
  errorBenchs[[length(errorBenchs)+1]] = errorBench
}








