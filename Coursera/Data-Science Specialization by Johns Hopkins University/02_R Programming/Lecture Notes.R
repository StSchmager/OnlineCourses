##week 1

print()

class()
as.numeric()
as.logical()
as.character()

matrix()
dim()
attributes()

cbind()
rbind()

#vector vs. list

c()
list()

factor()
table()
unclass()
attr()

is.na()
is.nan()
complete.cases()

#matrix vs. data frame

row.names()
read.table()
read.csv()
data.matrix()
data.frame()
nrow()
ncol()

names()
dimnames(q)

dput()
dget()
dump()
rm()
source()

file()
gzfile()
bzfile()
url()

str()

close()

readlines()

head()

##quiz 1

setwd("~/R/Coursera")
q<-read.csv("hw1_data.csv")
q[1:2,]
str(q)
q[152:153,]
q[47,]
sum(is.na(q[,1]))
mean(q[,1],na.rm=T)

q_s1<-subset(q,Ozone>31&Temp>90)
q_s1
mean(q_s1$Solar.R,na.rm=T)

q_s2<-subset(q,Month==6)
q_s2
mean(q_s2$Temp,na.rm=T)

q_s3<-subset(q,Month==5)
q_s3
max(q_s3$Ozone,na.rm=T)

##week 2

#control structures

if() 
{
        
} else 
{
        
}

for (i in 1:10) {
        print(i)
}

x <- c("a", "b", "c", "d")
for (i in 1:4) {
        print(x[i])
}

x <- c("a", "b", "c", "d")
for (i in seq_along (x))
{print(x[i])}

for (letter in x)
{print(letter)}

count <- 0
while (count < 10)
{
        print(count)
        count <- count +1
}

z <- 5
while (z>=3 && z <= 10)
{
        print(z)
        coin <- rbinom(1, 1, .5)
        
        if(coin == 1)
        {
                z <- z + 1
        } else 
        {
                z <- z -1
        }
}

repeat
        
        next

return

#functions

f <- function(a, b)
{ a^2}

formals(f)
args(paste)

f(2)

f <- function(a, b=43)
{
        print(a)
        print(b)
}

f(45)

args()
paste()
cat()

add2 <- function(x, y)
{
        x + y
}

above10 <- function(x)
{
        use <- x > 10
        x[use]
}

above <- function(x, n = 10)
{
        use <- x > n
        x[use]
}

columnmean <- function(y, removeNA = T)
{
        nc <- ncol(y)
        means <- numeric(nc)
        for(i in 1:nc)
        {
                means[i] <- mean(y[, i], na.rm = removeNA)
        }
        means
}

#coding standard
#vectorized operations

#dates & times

as.date()
weekdays()
month()
quarters()
as.POSIXlt()
as.POSIXct()
strptime()
class()
       
DOB <- as.Date("1989-03-06")
weekdays(DOB)
months(DOB)
quarters(DOB)
unclass(DOB)
class(DOB)
today <- as.Date("2014-09-10")
age <- today-DOB
class(age)
age
DOB <- as.POSIXlt(DOB)
today <- strptime(today, "%Y-%b-%d")
class(DOB)
class(today)
age <- today - DOB
age
names(age)

##week 3

#loop functions

lapply #loop over list of elements and apply function over it (output: list)
sapply # same as obove, but tries to simplify output (vector or matrix)

apply # especially for summaries of matrix rows and columns as well as higher-dimensional arrays
rowsums
rowmeans
colsums
colmeans

tapply # table apply loop over subsets of vector (can be simplified through function parameter); split your data into groups based on the value of some variable, then apply a function to each group
gl #generates factor levels
split # doesn't apply functions, but can be helpful in conjunction with apply functions (splits vector into factor-level groups)
interaction #splitting on more than one level, although that split does it without an interaction, but with a list of factors

vapply # specifies explicitly the format of the result, wheras sapply tries to guess it (whether list or vector or matrix); recommended for coding functions in order to be certain about what is expected

mapply # multivariate version of lapply/sapply (apply function over more than two lists)

#debugging tools
traceback

debug
browser

trace
recover

options(error = recover)

#quiz

library(datasets)
data(iris)
?iris
iris <- as.data.frame(iris)
head(iris)

tapply(iris$Sepal.Length, iris$Species, mean)

apply(iris[, 1:4], 2, mean)

library(datasets)
data(mtcars)
?mtcars
cars <- as.data.frame(mtcars)
head(cars)

tapply(cars$mpg, cars$cyl, mean)

Question4 <- tapply(cars$hp, cars$cyl, mean)
abs(Question4["4"] - Question4["8"])

debug(ls)
ls

##week 4

str()
summary()
gl()

#simulation
rnorm # random number generation
dnorm #density
pnorm #cumulative (probabilities)
qnorm #quantile

rpois
d...
p...
r...

rbinom

sample

set.seed #critical for reproducibility

#R-Profiler
system.time
Rprof
summaryRprof #more readible than above
... $by.total
... $by.self # more useful than above
... $sample.interval
... $sampling.time

#scoping rules
optim
nlm
optimize
ls(environment())

##quiz 4

set.seed(1)
rpois(5, 2)
