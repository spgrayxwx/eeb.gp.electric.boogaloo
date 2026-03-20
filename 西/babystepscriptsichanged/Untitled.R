#nvm this was so not needed kms
#ermmm m = -0.943852, b = 44.4454 R^2 = 0.9925

#Soooo: median survival time
#21*C = 24.05 days
#24*C 22.2
#27 = 19.4
#30 = 16.2
#32 = 14.25
#34 = 12
install.packages("caTools")
install.packages("ggplot2")
library(caTools)
library(ggplot2)

Data4MortalityMosqModel <- data.frame(
  Temp = c(21, 24, 27, 30, 32, 24),
  Med.Survival.in.Days = c(24.05, 22.2, 19.4, 16.2, 14.25, 12)
)

split = sample.split(Data4MortalityMosqModel$Med.Survival.in.Days, SplitRatio = 0.7)
trainingset = subset(Data4MortalityMosqModel, split == TRUE)
testset = subset(Data4MortalityMosqModel, split == FALSE)

lm_D4M = lm(formula = Med.Survival.in.Days ~ Temp, data = trainingset)
summary(lm_D4M)

#visualize bc why not kms
ggplot() + geom_point(aes(x =
                            trainingset$Temp, y = trainingset$Temp), 
                      color = 'purple') +
  geom_line(aes(x = trainingset$Temp, y = predict (lm_D4M, newdata = trainingset)), 
            color = 'blue') +
  ggtitle('Temperature vs Median Life Span (t set)') +
  xlab('Temp') +
  ylab('MedianDaysAlive')