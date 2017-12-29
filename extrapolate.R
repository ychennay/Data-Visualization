library(fGarch)
library(ggplot2)

# create a list containing the summary statistics for medicare and overall survey responses
MEAN <- c(5.4304, .6746)
STD <- c(33.97454, 25.1111)
SKEWNESS <- c(-2.6, -3.266)

# use fGarch library to generate a random sample from a skewed Gaussian distribution for medicare and overall
# the 200000 value specifies that a list of 200,000 random samples will be generated
medicare <- rsnorm(200000, MEAN[1], STD[1], SKEWNESS[1])
overall <- rsnorm(200000, MEAN[2], STD[2], SKEWNESS[2])

# print the summary statistics for each of the different survey's random samples
mean(medicare)
stdev(medicare)
median(medicare)
mean(overall)
stdev(overall)
median(overall)

# save the data to a csv file
dat <- data.frame(xx = c(medicare, overall),yy = c(rep("Medicare", 200000), rep("Overall",200000)))
write.csv(dat,'generated_samples.csv')

#plot the histogram distributions using ggplot2
ggplot(dat,aes(x=xx)) + 
  geom_histogram(data=subset(dat,yy == 'Medicare'), aes(y = (..count..)/sum(..count..)), fill = "red", alpha = 0.6, bins = 75) +
  geom_histogram(data=subset(dat,yy == 'Overall'), aes(y = (..count..)/sum(..count..)),col = "black", fill = "green", alpha = 0.2, bins = 75) + 
  xlab("% Margin") +
  xlim(c(-150,75)) +
  ylab('% of Respondents') +
  scale_fill_manual(name="group",values=c("red","darkgray"),labels=c("a","b")) +
  ggtitle("Fitted Distributions of Survey Responses") + 
  scale_y_continuous(labels = scales::percent)

# find the probability that overall margin > medicare margin
mean(overall > medicare)

# define the percentile intervals we are interested in
percentiles <- seq(from = .1, to = .9, by = .05)

# print the percentile values for medicare and overall
quantile(medicare, percentiles) 
quantile(overall,percentiles)
