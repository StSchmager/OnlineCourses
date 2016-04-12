## Week 2 ##############################################################################################################

# Question 1 
# What is the variance of the distribution of the average an IID draw of n observations from a population with mean ?? and variance ??2.

# Question 2)
# Suppose that diastolic blood pressures (DBPs) for men aged 35-44
# are normally distributed with
# a mean of 80 (mm Hg) and
# a standard deviation of 10.
# About what is the probability that a random 35-44 year old has a DBP less than 70?
pnorm(q = 70, mean = 80, sd = 10, lower.tail = T)
# Approximately 68%, 95% and 99% of the normal density lies within 1, 2 and 3 standard deviations from the mean, respectively

# Question 3
# Brain volume for adult women is normally distributed with a mean of about 1,100 cc for women with a standard deviation of 75 cc. What brain volume represents the 95th percentile?
qnorm(p = .95, mean = 1100, sd = 75, lower.tail = T)
# By symmetry, 1.28, 1.645, 1.96 and 2.33 are the 90th, 95th, 97.5th and 99th percentiles of the standard normal distribution respectively
1100+1.645*75

# Question 4
# Refer to the previous question.
# Brain volume for adult women is about 1,100 cc for women with a
# standard deviation of 75 cc.
# Consider the sample mean of 100 random adult women from this population.
# What is the 95th percentile of the distribution of that sample mean?
qnorm(0.95, mean = 1100, sd = 75/sqrt(100))
1100+1.645*(75/sqrt(100))
# By symmetry, 1.28, 1.645, 1.96 and 2.33 are the 90th, 95th, 97.5th and 99th percentiles of the standard normal distribution respectively

# Question 5 
# You flip a fair coin 5 times, about what's the probability of getting 4 or 5 heads?
pbinom(q = 3, size = 5, prob = .5, lower.tail = FALSE)

# Question 6 
# The respiratory disturbance index (RDI), a measure of sleep disturbance, for a specific population has a
# mean of 15 (sleep events per hour) and a standard deviation of 10.
# They are not normally distributed. Give your best estimate of the probability that a
# sample mean RDI of 100 people is between 14 and 16 events per hour?
pnorm(16, lower.tail = T, mean = 15, sd = 10/sqrt(100))-pnorm(14, lower.tail = TRUE, mean = 15, sd = 10/sqrt(100))

# Question 7 
# Consider a standard uniform density.
# The mean for this density is .5 and the
# variance is 1 / 12.
# You sample 1,000 observations from this distribution and take the sample mean,
# what value would you expect it to be near?

# Question 8
# The number of people showing up at a bus stop is assumed to be
# Poisson with a mean of 5 people per hour.
# You watch the bus stop for 3 hours.
# About what's the probability of viewing 10 or fewer people?
ppois(q = 10, lambda = 5*3, lower.tail = T)

## Week 3 ##############################################################################################################

# Question 1 
# In a population of interest, a
# sample of 9 men yielded a sample
# average brain volume of 1,100cc and a
# standard deviation of 30cc.
# What is a 95% Student's T confidence interval
# for the mean brain volume in this new population?

round(1100+c(-1, 1)*qt(p = .975, df = 9-1)*30/sqrt(9))

# Question 2 
# A diet pill is given to 9 subjects over six weeks.
# The average difference in weight (follow up - baseline) is -2 pounds.
# What would the standard deviation of the difference in weight have to be
# for the upper endpoint of the 95% T confidence interval to touch 0?

round((2/qt(p = .975, df = 9-1))*sqrt(9), 2)

# Question 3
# In an effort to improve running performance,
# 5 runners were either given a protein supplement or placebo.
# Then, after a suitable washout period, they were given the opposite treatment.
# Their mile times were recorded under both the treatment and placebo,
# yielding 10 measurements with 2 per subject.
# The researchers intend to use a T test and interval to investigate the treatment.
# Should they use a paired or independent group T test and interval?

# You could use either

# It's necessary to use both

# Independent groups, since all subjects were seen under both systems

# [x] A paired interval

# Question 4 
# In a study of emergency room waiting times,
# investigators consider a new and the standard triage systems.
# To test the systems, administrators selected 20 nights and
# randomly assigned the
# new triage system to be used on 10 nights and the
n_std <- 10
# standard system on the remaining 10 nights.
n_new <- 10
# They calculated the nightly median waiting time (MWT) to see a physician.
# The average MWT for the new system was 3 hours with a variance of 0.60 while
m_new <- 3
s2_new <- .6
s_new <- sqrt(s2_new)
# the average MWT for the old system was 5 hours with a variance of 0.68.
m_std <- 5
s2_std <- .68
s_std <- sqrt(s2_std)
# Consider the 95% confidence interval estimate for the differences of the mean MWT associated with the new system.
# Assume a constant variance. What is the interval? Subtract in this order (New System - Old System).
sp <- sqrt( ((n_new-1) * s2_new + (n_std-1) * s2_std) / (n_new + n_std - 2))

(m_new-m_std) + c(-1,1) * qt(p = .975, df = n_new+n_std-2) * sp * sqrt(1/n_new+1/n_std)

# [1.29, 2.70]

 [-2.75, -1.25]

# [-2,70, -1.29]

# [1.25, 2.75]

# Question 5 
# Suppose that you create a 95% T confidence interval.
# You then create a 90% interval using the same data.
# What can be said about the 90% interval with respect to the 95% interval?

qt(p = .95, df = 100)
qt(p = .90, df = 100)

# The interval will be the same width, but shifted.

# It is impossible to tell.

# The interval will be wider

[x] The interval will be narrower.

# Questio 6 
# To further test the hospital triage system,
# administrators selected 200 nights and randomly assigned a
# new triage system to be used on 100 nights and a
n_new <- 100
# standard system on the remaining 100 nights.
n_std <- 100
# They calculated the nightly median waiting time (MWT) to see a physician.
# The average MWT for the new system was 4 hours with a standard deviation of 0.5 hours while
 m_new      <- 4
 s_new      <- .5
s2_new      <- s_new^2
# the average MWT for the old system was 6 hours with a standard deviation of 2 hours.
 m_std      <- 6
 s_std      <- 2
s2_std      <- s_std^2
# Consider the hypothesis of a decrease in the mean MWT associated with the new treatment.
# What does the 95% independent group confidence interval with unequal variances suggest vis a vis this hypothesis?
# (Because there's so many observations per group, just use the Z quantile instead of the T.)

m_std-m_new + c(-1, 1)*qnorm(.975)*sqrt(s2_new/n_new + s2_std/n_std)

# When subtracting (old - new) the interval is entirely above zero. The new system does not appear to be effective.

# When subtracting (old - new) the interval contains 0. The new system appears to be effective.

# When subtracting (old - new) the interval is entirely above zero. The new system appears to be effective.

# [x] When subtracting (old - new) the interval contains 0. There is not evidence suggesting that the new system is effective.

# Question 7 
# Suppose that 18 obese subjects were randomized,
# 9 each, to a new diet pill and a placebo.
n_pill <- 9
n_plac <- 9
# Subjects' body mass indices (BMIs) were measured
# at a baseline and again after having received the treatment or placebo for four weeks.
# The average difference from follow-up to the baseline (followup - baseline) was
# -3 kg/m2 for the treated group and
# 1 kg/m2 for the placebo group.
m_pill <- -3
m_plac <- 1
# The corresponding standard deviations of the differences was
# 1.5 kg/m2 for the treatment group and
# 1.8 kg/m2 for the placebo group.
sd_pill <- 1.5
sd_plac <- 1.8
# Does the change in BMI over the four week period appear to differ between the treated and placebo groups?
# Assuming normality of the underlying data and a common population variance,
# calculate the relevant 90% t confidence interval.
# Subtract in the order of (Treated - Placebo) with the smaller (more negative) number first.
sp <- sqrt( ((8)*(1.5^2) + (8)*(1.8^2)) /
                   (16)
            )
-3 - 1 + c(-1, 1) * qt(p = .95, df = 16) * sp * sqrt(1/9 + 1/9)
-3 - 1 + c(-1, 1) * qt(p = .95, df = 16) * sqrt((1.5^2)/9 + (1.8^2)/9)

# [-5.531, -2.469]

# [2.636, 5.364]

# [-5.364, -2.636]

# [2.469, 5.531]

## Week 4 ##############################################################################################################

# Question 1 
# A pharmaceutical company is interested in
# testing a potential blood pressure lowering medication.
# Their first examination considers only
# subjects that received the medication at baseline
# then two weeks later.
# The data are as follows (SBP in mmHg)

# Subject   Baseline	Week 2
# 1	      140	      132
# 2	      138	      135
# 3	      150	      151
# 4	      148	      146
# 5	      135	      130

# Consider testing the hypothesis that
# there was a mean reduction in blood pressure?
# Give the P-value for the associated two sided T test.
# (Hint, consider that the observations are paired.)

t.test(c(132, 135, 151, 146, 130),
       c(140, 138, 150, 148, 135),
       alternative = "two.sided", paired = T)

[x] 0.087

# 0.05

# 0.10

# 0.043

# Question 2 
# A sample of 9 men yielded
n  = 9
df = n-1
# a sample average brain volume of 1,100cc and
m = 1100
# a standard deviation of 30cc.
s = 30
# What is the complete set of values of mu0
# that a test of H0:mu=mu0 would
# fail to reject the null hypothesis in a
# two sided 5% Students t-test?
alpha = .05
prob  = 1-alpha/2

round(m + c(-1, 1) * qt(prob, df) * s/sqrt(n))

# 1031 to 1169

# 1080 to 1120

[x] 1077 to 1123

# 1081 to 1119

# Question 3 
# Researchers conducted a blind taste test of Coke versus Pepsi.
# Each of 4 people was asked
# which of two blinded drinks given in random order that they preferred.
# The data was such that 3 of the 4 people chose Coke.
# Assuming that this sample is representative,
# report a P-value for a test of the hypothesis
# that Coke is preferred to Pepsi using a one sided exact test.

round(pbinom(q = 2, size = 4, prob = .5, lower.tail = FALSE), 2)

# 0.10

# 0.62

[x] 0.31

# 0.005

# Question 4 
# Infection rates at a hospital
# above 1 infection per 100 person days at risk
# are believed to be too high and are used as a benchmark.

# A hospital that had previously been above the benchmark recently had
# 10 infections over the last 1,787 person days at risk.
# About what is the one sided P-value for the relevant test
# of whether the hospital is *below* the standard?

# H0: benchmark
lambda = 1/100
# Ha: lambda < 1/100
(10/1787)*100

round(ppois(10, lambda*1787, lower.tail = TRUE), 2)

# 0.22

# 0.52

# 0.11

[x] 0.03

# Question 5 
# Suppose that 18 obese subjects were randomized, 9 each,
n1 = 9
n2 = 9
n  = n1 + n2
# to a new diet pill and a placebo.
# Subjects' body mass indices (BMIs) were measured at a baseline and
# again after having received the treatment or placebo for four weeks.
# The average difference from follow-up to the baseline (followup - baseline) was
# -3 kg/m2 for the treated group and
# 1 kg/m2 for the placebo group.
m1 = -3
m2 = 1
# The corresponding standard deviations of the differences was
# 1.5 kg/m2 for the treatment group and
# 1.8 kg/m2 for the placebo group.
s1 = 1.5
s2 = 1.8
# Does the change in BMI appear to differ between the treated and placebo groups?
# Assuming normality of the underlying data and a
# common population variance, give a pvalue for a two sided t test.

sp = sqrt(((n1-1)*s1^2 + (n2-1)*s2^2)/(n-2))

 t = (m1-m2) / (sp * sqrt(1/n1+1/n2))
 
 pt(q = t, df = n-2, lower.tail = TRUE)*2

# Less than 0.05, but larger than 0.01 (wrong)

# Larger than 0.10

# Less than 0.10 but larger than 0.05 (wrong)

# [x] Less than 0.01

# Question 6 
# Brain volumes for 9 men yielded a
# 90% confidence interval of 1,077 cc to 1,123 cc.
# Would you reject in a two sided 5% hypothesis test of
# H0:mu=1,078?
m    = 1078
n    = 9
ll   = 1077
up   = 1123
prob = .9 

m + c(-1, 1) * qt(prob, n - 1) * s/sqrt(n)

# It's impossible to tell.

# [x] No you wouldn't reject.

# Where does Brian come up with these questions?

# Yes you would reject.

# Question 7 
# Researchers would like to conduct a study of 100 healthy adults
n = 100
# to detect a four year mean brain volume loss of .01 mm3.
delta = .01
# Assume that the standard deviation of four year volume loss in this population is .04 mm3.
sd = .04
# About what would be the power of the study for a 5% one sided test versus a null hypothesis of no volume loss?
alpha = 0.05

power.t.test(n = n, delta = delta, sd = sd,
             sig.level = alpha,
             type = "one.sample",  alt = "one.sided")$power

# 0.70

# 0.60

# 0.50

[x] 0.80

# Question 8 
# Researchers would like to conduct a study of n healthy adults to detect a four year mean brain volume loss of .01 mm3.
delta = .01
# Assume that the standard deviation of four year volume loss in this population is .04 mm3.
sd = .04
# About what would be the value of n needed for 90% power of type one error rate of 5% one sided test
power = .9
alpha = 0.05
# versus a null hypothesis of no volume loss?

power.t.test(power = power, delta = delta, sd = sd,
             sig.level = alpha,
             type = "one.sample",  alt = "one.sided")$n

# 180

# 160

[x] 140

# 120

# Question 9 
# As you increase the type one error rate, alpha, what happens to power?

[x] You will get larger power.

# No, for real, where does Brian come up with these problems?

# It's impossible to tell given the information in the problem.

# You will get smaller power.
