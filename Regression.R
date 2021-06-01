library(dplyr)
library(tidyr)
library(ftsa)
library(readxl)

metrics = read.csv('/Users/aaqibjabbarhj/Desktop/4th Sem/BA field project/All files/R studio files/Cleaned data/CSV files/Filtered-Store Business and Scheduling Metrics.csv')
executions = read.csv('/Users/aaqibjabbarhj/Desktop/4th Sem/BA field project/All files/R studio files/Cleaned data/CSV files/Filtered Executions.csv')
df = read.csv('/Users/aaqibjabbarhj/Desktop/4th Sem/BA field project/All files/R studio files/Cleaned data/CSV files/df.csv')
logons = read.csv('/Users/aaqibjabbarhj/Desktop/4th Sem/BA field project/All files/R studio files/Cleaned data/CSV files/Filtered Logons.csv')
str

executions = executions %>%
  group_by(`Str.Id`,`Week.Start.Date`) %>%
  summarise("Executions Planned" = sum(`Executions.Planned`),
            "Executions On-Time" = sum(`Executions.Completed..On.Time`),
            "Executions Forced Closed" = sum(`Executions.Forced.Closed`))


df = merge(df, logons, by=c("Str.Id", "Week.Start.Date"))
df = merge(df, executions, by=c("Str.Id", "Week.Start.Date"))

df$SalesPerLH = df$`Act.Sales` / df$`Act.Hrs`
df$EditsPerLH = df$`Sched.Edits` / df$`Act.Hrs`
df$LCbySales = df$`Act.Labor.Cost` / df$`Act.Sales`

df$ShiftRatio = df$`Sched.Shifts.MGR` / df$`Sched.Shifts.SYS`
df$ShiftDifference = df$`Sched.Shifts.MGR` - df$`Sched.Shifts.SYS`
df$EfficiencyRatio = df$`Sched.Efficiency.MGR` / df$`Sched.Efficiency.SYS`
df$EfficiencyDifference = df$`Sched.Efficiency.MGR` - df$`Sched.Efficiency.SYS`

df$Wage = df$`Act.Labor.Cost` / df$`Act.Hrs`
df$LogonsPerLH = df$Logons / df$`Act.Hrs`
df$OnTimePercentage = df$`Executions On-Time`/df$`Executions Planned`
df$ForcedClosedPercentage = df$`Executions On-Time`/df$`Executions Planned`

summary(df)

df = na.omit(df)
df <- df[is.finite(rowSums(df[ ,-1])),]

mdl = lm(SalesPerLH ~ EditsPerLH, data =df)
summary(mdl)

mdl = lm(SalesPerLH ~ EditsPerLH + 
           ShiftRatio + 
           EfficiencyRatio + 
           Wage + 
           ShiftDifference +
           EfficiencyDifference + 
           LogonsPerLH + 
           OnTimePercentage + 
           ForcedClosedPercentage +
           `Dist Id`, data=df)
summary(mdl)


x = summary(mdl)
B=data.frame('Full' = x$coefficients[,1])
for (distId in unique(df$`Dist Id`)){
  dfDist = df[df$`Dist Id` == distId, ]
  mdl = lm(SalesPerLH ~ EditsPerLH + 
             ShiftRatio + 
             EfficiencyRatio + 
             Wage + 
             ShiftDifference +
             EfficiencyDifference + 
             LogonsPerLH + 
             OnTimePercentage,
           data = dfDist)
  x = summary(mdl)
  B[,c(distId)] = x$coefficients[,1]
}

View(x$coefficients)
