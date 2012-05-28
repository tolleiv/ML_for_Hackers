# File-Name:       chapter02.R           
# Date:            2011-11-11                                
# Author:          John Myles White
# Email:           jmw@johnmyleswhite.com
# Purpose:         Code for Chapter 2. Showcases tools for exploratory data analysis.
# Data Used:       data/01_heights_weights_genders.csv
# Packages Used:   ggplot2
# Machine:         John Myles White's MacBook

# All source code is copyright (c) 2011, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php

# All images and materials produced by this code are licensed under the Creative Commons 
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/

# All rights reserved.

#
# Snippet 1
#

# Load in the data set from disk.
data.file <- file.path('data', '01_heights_weights_genders.csv')
heights.weights <- read.csv(data.file, header = TRUE, sep = ',')

# Create a numeric vector containing just the heights data.
heights <- with(heights.weights, Height)
summary(heights)

# Expected output.
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#54.26   63.51   66.32   66.37   69.17   79.00 

#
# Snippet 2
#

# Define our own mean and median functions.
my.mean <- function(x)
{
  return(sum(x) / length(x))
}

my.median <- function(x)
{
  sorted.x <- sort(x)
  if (length(x) %% 2 == 0)
  {
    indices <- c(length(x) / 2, length(x) / 2 + 1)
    return(mean(sorted.x[indices]))
  }
  else
  {
    index <- ceiling(length(x) / 2)
    return(sorted.x[index])
  }
}

#
# Snippet 3
#

# Compare means and medians on toy examples.
my.vector <- c(0, 100)

my.vector
# [1]	0 100

mean(my.vector)
#[1] 50

median(my.vector)
#[1] 50

my.vector <- c(0, 0, 100)

mean(my.vector)
#[1] 33.33333

median(my.vector)
#[1] 0

#
# Snippet 4
#

# Confirm that our mean and median functions produce the correct answer.
my.mean(heights)
#[1] 66.36756

my.median(heights)
#[1] 66.31807

mean(heights) - my.mean(heights)
#[1] 0

median(heights) - my.median(heights)
#[1] 0

#
# Snippet 5
#

# Experiment with functions for assessing the range of a data set.
min(heights)
#[1] 54.26313

#
# Snippet 6
#

max(heights)
#[1] 78.99874

#
# Snippet 7
#

c(min(heights), max(heights))
#[1] 54.26313 78.99874

range(heights)
#[1] 54.26313 78.99874

#
# Snippet 8
#

# Try out the quantile function for computing arbitrary quantiles.
quantile(heights)
#      0%      25%      50%      75%     100% 
#54.26313 63.50562 66.31807 69.17426 78.99874 

#
# Snippet 9
#

quantile(heights, probs = seq(0, 1, by = 0.20))
#      0%      20%      40%      60%      80%     100% 
#54.26313 62.85901 65.19422 67.43537 69.81162 78.99874 

#
# Snippet 10
#

seq(0, 1, by = 0.20)
#[1] 0.0 0.2 0.4 0.6 0.8 1.0

#
# Snippet 11
#

# Define a variance function to assess the spread of data.
my.var <- function(x)
{
  m <- mean(x)
  return(sum((x - m) ^ 2) / length(x))
}

#
# Snippet 12
#

# Test our variance function for correctness.
my.var(heights) - var(heights)

#
# Snippet 13
#

# Update the variance function to make it unbiased.
my.var <- function(x)
{
  m <- mean(x)
  return(sum((x - m) ^ 2) / (length(x) - 1))
}

# Test our variance function again for correctness.
my.var(heights) - var(heights)

#
# Snippet 14
#

# Check the range predicted by the variance function.
c(mean(heights) - var(heights), mean(heights) + var(heights))
#[1] 51.56409 81.17103

#
# Snippet 15
#

c(mean(heights) - var(heights), mean(heights) + var(heights))
#[1] 51.56409 81.17103
range(heights)
#[1] 54.26313 78.99874

#
# Snippet 16
#

# Switch to standard deviations instead for thinking about ranges.
my.sd <- function(x)
{
  return(sqrt(my.var(x)))
}

#
# Snippet 17
#

# Test our standard deviation function for correctness.
my.sd(heights) - sd(heights)

#
# Snippet 18
#

c(mean(heights) - sd(heights), mean(heights) + sd(heights))
# [1] 62.52003 70.21509

range(heights)
#[1] 54.26313 78.99874

#
# Snippet 19
#

c(mean(heights) - sd(heights), mean(heights) + sd(heights))
# [1] 62.52003 70.21509

c(quantile(heights, probs = 0.25), quantile(heights, probs = 0.75))
#     25%      75% 
#63.50562 69.17426 

#
# Snippet 20
#

# Start visualizing data using the ggplot2 package.
library('ggplot2')

# Load the data from scratch for purity.
data.file <- file.path('data', '01_heights_weights_genders.csv')
heights.weights <- read.csv(data.file, header = TRUE, sep = ',')

# Experiment with histograms.
snip20.plot <- ggplot(heights.weights, aes(x = Height)) +
  geom_histogram(binwidth = 1)
ggsave(filename = file.path("images", "snip20.pdf"))


#
# Snippet 21
#

snip21.plot <- ggplot(heights.weights, aes(x = Height)) +
  geom_histogram(binwidth = 5)
ggsave(filename = file.path("images", "snip21.pdf"))


#
# Snippet 22
#

snip22.plot <- ggplot(heights.weights, aes(x = Height)) +
  geom_histogram(binwidth = 0.001)
ggsave(filename = file.path("images", "snip22.pdf"))


#
# Snippet 23
#

# Experiment with kernel density estimates.
snip23.plot <- ggplot(heights.weights, aes(x = Height)) +
  geom_density()
ggsave(filename = file.path("images", "snip23.pdf"))


#
# Snippet 24
#

# Separate out heights and weights based on gender.
snip24.plot <- ggplot(heights.weights, aes(x = Height, fill = Gender)) +
  geom_density()
ggsave(filename = file.path("images", "snip24.pdf"))


#
# Snippet 25
#

snip25.plot <- ggplot(heights.weights, aes(x = Weight, fill = Gender)) +
  geom_density()
ggsave(filename = file.path("images", "snip25.pdf"))


#
# Snippet 26
#

# Produce two facets in a single plot to make it easier to see the hidden structure.
ggplot(heights.weights, aes(x = Weight, fill = Gender)) +
  geom_density() +
  facet_grid(Gender ~ .)
ggsave(filename = file.path("images", "snip26.pdf"))


#
# Snippet 27
#

# Experiment with random numbers from the normal distribution.
m <- 0
s <- 1
snip27.plot<-ggplot(data.frame(X = rnorm(100000, m, s)), aes(x = X)) +
  geom_density()
ggsave(filename = file.path("images", "snip27.pdf"))



#
# Snippet 28
#

# Compare the normal distribution with the Cauchy distribution.
set.seed(1)
normal.values <- rnorm(250, 0, 1)
cauchy.values <- rcauchy(250, 0, 1)
range(normal.values)
range(cauchy.values)

#
# Snippet 29
#

ggplot(data.frame(X = normal.values), aes(x = X)) +
  geom_density()
ggplot(data.frame(X = cauchy.values), aes(x = X)) +
  geom_density()

set.seed(1234)
df = rbind(
    data.frame(X=rnorm(250, 0, 1), d="norm"),
    data.frame(X=rcauchy(250, 0, 1), d="cauchy"),
    data.frame(X=rbinom(250, 0, 1), d="binom"),
    data.frame(X=rpois(250, lambda=1), d="pois")
)
snip29.plot <- ggplot(df, aes(x=X)) + geom_density(aes(colour=d))  + facet_grid(d ~ .)
ggsave(filename = file.path("images", "snip29.pdf"))


#
# Snippet 30
#

# Experiment with random numbers from the gamma distribution.
gamma.values <- rgamma(100000, 1, 0.001)
snip30.plot <- ggplot(data.frame(X = gamma.values), aes(x = X)) +
  geom_density()
ggsave(filename = file.path("images", "snip30.pdf"))


#
# Snippet 31
#

# Generate scatterplots of the heights and weights to see their relationship.
snip31.plot <- ggplot(heights.weights, aes(x = Height, y = Weight)) +
  geom_point()
ggsave(filename = file.path("images", "snip31.pdf"))


#
# Snippet 32
#

# Add a smooth shape that relates the two explicitly.
snip32.plot <- ggplot(heights.weights, aes(x = Height, y = Weight)) +
  geom_point() +
  geom_smooth()
ggsave(filename = file.path("images", "snip32.pdf"))


#
# Snippet 33
#

# See how the smooth shape gets better with more data.
ggplot(heights.weights[1:20, ], aes(x = Height, y = Weight)) +
  geom_point() +
  geom_smooth()
ggplot(heights.weights[1:200, ], aes(x = Height, y = Weight)) +
  geom_point() +
  geom_smooth()
ggplot(heights.weights[1:2000, ], aes(x = Height, y = Weight)) +
  geom_point() +
  geom_smooth()

df = rbind(
    data.frame(heights.weights[1:20, ], d="Small"),
    data.frame(heights.weights[1:200, ], d="Medium"),
    data.frame(heights.weights[1:2000, ], d="Large")
)
ggplot(df, aes(x = Height, y = Weight)) +
  geom_point(aes(colour=d)) +
  facet_grid(d ~ .) +
  geom_smooth()
ggsave(filename = file.path("images", "snip33.pdf"))

#
# Snippet 34
#

# Visualize how gender depends on height and weight.
snip34.plot <- ggplot(heights.weights, aes(x = Height, y = Weight)) +
  geom_point(aes(color = Gender, alpha = 0.25)) +
  scale_alpha(guide="none") +
  scale_color_manual(values = c("Male" = "black", "Female" = "gray")) +
  theme_bw()
ggsave(filename = file.path("images", "snip34.pdf"))


# An alternative using bright colors.
snip34a.plot <- ggplot(heights.weights, aes(x = Height, y = Weight, color = Gender)) +
  geom_point()
ggsave(filename = file.path("images", "snip34a.pdf"))

#
# Snippet 35
#

heights.weights <- transform(heights.weights,
                             Male = ifelse(Gender == 'Male', 1, 0))

logit.model <- glm(Male ~ Weight + Height,
                   data = heights.weights,
                   family = binomial(link = 'logit'))

snip35.plot <- ggplot(heights.weights, aes(x = Height, y = Weight)) +
  geom_point(aes(color = Gender, alpha = 0.25)) +
  scale_alpha(guide="none") +
  scale_color_manual(values = c("Male" = "black", "Female" = "gray")) +
  theme_bw() +
  stat_abline(intercept = -coef(logit.model)[1] / coef(logit.model)[2],
              slope = - coef(logit.model)[3] / coef(logit.model)[2],
              geom = 'abline',
              color = 'black')
ggsave(filename = file.path("images", "snip35.pdf"))
