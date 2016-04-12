# Week 1 ###############################################################################################################

## Lectures

library(kernlab)
data(spam)
str(spam[,1:5])

# Perform the subsampling
set.seed(3435)
trainIndicator = rbinom(4601, size = 1, prob = 0.5)
table(trainIndicator)

trainSpam = spam[trainIndicator == 1, ]
testSpam = spam[trainIndicator == 0, ]

names(trainSpam)
head(trainSpam)
table(trainSpam$type)
plot(trainSpam$capitalAve ~ trainSpam$type)
plot(log10(trainSpam$capitalAve + 1) ~ trainSpam$type) # take base-10 log transf because data is skewed; add 1 because log10(0) doesn't work
plot(log10(trainSpam[, 1:4] + 1))

hCluster = hclust(dist(t(trainSpam[, 1:57]))) # hierarchical c-analysis
plot(hCluster)
hClusterUpdated = hclust(dist(t(log10(trainSpam[, 1:55] + 1))))
plot(hClusterUpdated)

...

## Quiz

