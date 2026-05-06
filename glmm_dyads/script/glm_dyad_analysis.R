setwd("C:/Users/rohit_negi/Desktop/Max Planck PhD_RN/3. project III - handedness_RN/final_analysis")

source("diagnostic_fcns.r")

install.packages("DHARMa")
library(DHARMa)

xdata = read.table(file="glm_dyad_dataset.csv", header = T, sep=",", stringsAsFactors=T)

xdata$context

hist(xdata$handedness_pair)

full = glm(handedness_pair ~ context + proximity, 
           family = binomial,
           data = xdata)

null = glm(handedness_pair ~ 1, 
           family = binomial,
           data = xdata)

## full vs null comparrison 
round(as.data.frame(anova(null, full, test="Chisq")), 3)

## Assumptions
## Check for overdispersion
overdisp.test(full)

##Calculating scaled residuals using Dharma
testDispersion(full)

##Calculating Randomized Quantile residuals usind Dharma
simulationOutput <- simulateResiduals(fittedModel = full, n = 1000, plot = T)

## Re-level factor, model: stone_tool_use

xdata$context <- relevel(xdata$context, ref="stone_tool_use")

xdata$proximity <- relevel(xdata$proximity, ref="<5m>")

## Summary
summary(full)


