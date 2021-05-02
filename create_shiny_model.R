if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(caret)
library(data.table)
library(knitr)
library(dplyr)


url_file <- 'https://raw.github.com/gisforgringo/harvard/master/column_3C_weka.csv'

df <-read.csv(url_file)
y<-df$class
x<- df[ , 1:6]
classes <- unique(y)
features <- names(x)
classes
features

r <- nrow(x)
c <- ncol(x)

model <- train(class ~ ., method = 'naive_bayes', data = df)


# Save model to RDS file
saveRDS(model, "model.rds")

