setwd("C:/Users/OWNER/OneDrive - Emory University/Spring 2020/Marketing Analytics/Projects/Pernalonga")

library(data.table)
library(dplyr)
library(stringr)
library(tidyverse)
library(e1071)

######################### Calculating PSIM ###############################

# calculate similarity for each customer between each of the other two groups and Colgate-only users
baseline <- fread("baseline_customers_input.csv", header = TRUE)
target <- fread("Target_customers_input.csv", header = TRUE)

baseline_cust <- data.frame(baseline$cust_id)
target_cust <- data.frame(target$cust_id)

baseline <- baseline %>%
  remove_rownames() %>%
  column_to_rownames(var="cust_id")

target <- target %>%
  remove_rownames() %>%
  column_to_rownames(var="cust_id")

# first need to create an average vector to represent baseline customers
# use colmeans
baseline_vector <- colMeans(baseline)
baseline_vector <- data.frame(baseline_vector)
target_transpose <- data.frame(t(target))

# now, calculate how similar each user in the other two segments are with the average baseline user vector
PSIM <- data.frame(array(cor(target_transpose,baseline_vector)))

# change names of column and add cust_id as rownames
colnames(PSIM) <- "PSIM"
PSIM$cust_id <- rownames(target)

fwrite(PSIM, "PSIM.csv")

PSIM <- fread("PSIM.csv", header = TRUE)

# we will use a cutoff of 0.5 for customers going into the Naive Bayes model
# let's get just those customers
PSIM <- data.frame(PSIM)
PSIM_thresh <- PSIM[PSIM$PSIM >= 0.5,]

######################### Naive Bayes Model ###############################

# naive bayes model to get probability of purchase for each customer
# import the data
cust_data <- fread("all_customers_sales.csv", header = TRUE)
cust_data[is.na(cust_data)] <- 0 # make sure NAs are 0 for those that have not purchased anything

# the first column is customer id's but let's remove that and just make it the row names
nb_data <- cust_data %>%
  remove_rownames() %>%
  column_to_rownames(var="cust_id")

X <- nb_data[,c(1:5000)] # get just the X variables which are all the products
y <- nb_data[,c(5001)] # get the y variable which is whether or not the customer has bought Colgate toothpaste

# run the model
nb_model <- naiveBayes(x = X, y = y, data = X)
summary(nb_model)
nb_model

# calculate probabilities
nb_preds <- predict(nb_model, nb_data, type = "raw")

# create dataframe with customer id and the predictions
customer_preds <- data.frame(cust_data$cust_id,nb_preds)
colnames(customer_preds) <- c("cust_id", "NO", "YES")

# write the probability of purchase for each customer to a csv
fwrite(customer_preds, "customer_preds_nb.csv")

# only keep NB predictions for customers in the target group that has PSIM >= 0.5
target_nb <- customer_preds[customer_preds$cust_id %in% PSIM_thresh$cust_id,]
target_nb <- left_join(target_nb, PSIM_thresh, by = "cust_id")

# customers that have both PSIM >= 0.5 and NB pred >= 0.5
target_final <- target_nb[target_nb$YES >= 0.5,]
target_final$cust_type <- "Target"

# get our baseline customers too - we will target them
baseline_final <- customer_preds[customer_preds$cust_id %in% baseline$cust_id,]
baseline_final$PSIM <- NA
baseline_final$cust_type <- "Baseline"

# stack the two groups of customers together
all_customers <- rbind(target_final, baseline_final)
all_customers <- all_customers[,c("cust_id","YES","PSIM","cust_type")]

# write final customer list to csv
fwrite(all_customers, "all_customers.csv")

