library(biomaRt)
library(caret)
library(caTools)
library(microbenchmark)


print(iris$Sepal.Length)
sample.split(iris,SplitRatio = 0.7)->s
subset(iris, s==TRUE)->train_data
subset(iris, s==FALSE)->test_data

lm(train_data$Sepal.Length~., train_data)->model1
summary(model1)


# Create dataframe
set.seed(100)
col1 <- runif(12^4, 0, 2)
col2 <- rnorm(12^4, 0, 2)
col3 <- rpois(12^4, 3)
col4 <- rchisq(12^4, 2)
df <- data.frame(col1, col2, col3, col4)
head(df)  

# Get all package installed
installed.packages()

# String extraction
x <- "The quick brown fox jumps over the lazy dog"
split.string <- strsplit(x, " ")
extract.words <- split.string[[1]]
result <- unique(tolower(extract.words))
print(result)

# Replace all missing value of a vector x with the sum of the vector
replacena = function(x){x[is.na(x)]=sum(x, na.rm=TRUE); x}
replacena (c(1, NA, 3))

# What is the difference between "%%" and "%/%"?
5%%3 # Modulo
5%/%3 # integer division (i.e. quotient without remainder)

# apply(), lapply(), sapply(), tapply() Function in R with Examples
m1 = matrix(C <- (1:10), nrow=5, ncol=9)
print(m1)

add_one <- function(x){
  return (x+1)
}

apply(m1, 1, add_one)  #DF and MX as input
unlist(lapply(m1, add_one)) #Return a list
sapply(m1[1,],add_one)   #List, Vector or DF as Input
tapply(df, df$output, sum)  # Apply function in a group indexed

# What is corrleation
head(iris)
cor(iris[-5], method = 'pearson')
cor(iris[-5], method= 'spearman')
cor(iris[-5], method= 'kendal' )

# Select, Filter and Pipeline.
library(dplyr)

filter(iris, Species=="setosa") # Disard rownames, Much faster in big dataset and  can use SQL
subset(iris, Species=="setosa") # Keep rownames
iris %>% select(Species) %>% filter(Species!='setosa' && !is.na(Species))->a
which(1:10>3)
m <- data.frame(matrix(c(1:15), 4,5))
div.3 = m%%3==0
print(div.3)
mloc = data.frame(which(div.3, arr.ind=TRUE))
for (i in 1:min(dim(m))){
  set(m, i, i, 0)
}

#Filter in a matrix (data frame)
m <- data.frame(matrix(1:15, 3,5))
m[which(m>4, arr.ind = TRUE)]=1
m

# with, within and by
a = warpbreaks
head(a)
with(a, table(wool, tension))
b <- within(a,{name <- breaks+1})
head(b)

by(warpbreaks[,1:2], warpbreaks[,"tension"], summary)
by(warpbreaks, warpbreaks['tension'], function(x){lm(breaks~wool, data=x)})

# Sampling 
sample(1:12, 1000, replace=TRUE)

# 100 Bernoulli trials
sample(c(0,1), 100, replace = TRUE)

# What is the difference between seq(4) and seq_along(4)
seq(4)
seq_along(10)

# Data structure in R
# List, Dataframe, Array, Vector, Matrix

# Aggregate
head(state.x77)
print(state.region)
aggregate(state.x77, list(Region = state.region), mean)

# Merge function for adding the columns
# NOT RUN {
authors <- data.frame(
  ## I(*) : use character columns of names to get sensible sort order
  surname = I(c("Tukey", "Venables", "Tierney", "Ripley", "McNeil")),
  nationality = c("US", "Australia", "US", "UK", "Australia"),
  deceased = c("yes", rep("no", 4)))
authorN <- within(authors, { name <- surname; rm(surname) })

books <- data.frame(
  name = I(c("Tukey", "Venables", "Tierney",
             "Ripley", "Ripley", "McNeil", "R Core")),
  title = c("Exploratory Data Analysis",
            "Modern Applied Statistics ...",
            "LISP-STAT",
            "Spatial Statistics", "Stochastic Simulation",
            "Interactive Data Analysis",
            "An Introduction to R"),
  other.author = c(NA, "Ripley", NA, NA, NA, NA,
                   "Venables & Smith"))

print(authorN)
print(books)
(m0 <- merge(authorN, books)) # Automatics finding the index
(m1 <- merge(authors, books, by.x = "surname", by.y = "name"))

# Export data into different types (csv, xlsx, RDS, SAS, SPSS)
path <- paste(getwd(),"/data.csv",sep = "")
write.csv(mtcars, path)
mtcars_1 = read.csv(path)

# library(xlsx)
# path <- paste(getwd(),"/data.xls",sep = "")
# write.xlsx(mt.cars, path)
# library(haven)
# write_sav(df, "table_car.sav")
# write_sas(df, "table_car.sas7bdat")
# write_sas(df, "table_car.sas7bdat")

# Others export to Google Driver and DropBox can be found here:
# https://www.guru99.com/r-exporting-data.html

# Storing R object into a file
save(df, file='z.Rdata')
load("z.Rdata")

# state.x77
state.x77
state = data.frame(state.x77)
subset(state, Population<400)

sum_value <- function(x){
  ifelse(sum(x)>20,">","<=")
}
apply(m1[,1:6],2,sum_value)

df[which(rowSums(df[,1:4])>4),'output']=">4"
which(rowSums(df[,1:4])>4)

original <- microbenchmark({
  myfunction <- function(x){
    if ((x['col1']+x['col2']+x['col3']+x['col4'])>4){
      ">4"
    }else{
      "<=4"
    }
  }
  output=apply(df[,c(1:4)],1, FUN=myfunction)
  df$output <- output
}, times=10L)

# 87

library(data.table)
need <- rowSums(df[,1:4])>4
faster <- microbenchmark({
  for (i in 1:length(need)){
    if (need[i]){
      set(df, i, 'output',">4")
    }else{
      set(df, i, 'output',"<4")
    }
  }
}, times=10L)

faster # 2.03


fastest <- microbenchmark({
  df$output="<=4"
  df[rowSums(df[,1:4])>4,'output']=">4"
}, times=10L)

fastest # 2.03

