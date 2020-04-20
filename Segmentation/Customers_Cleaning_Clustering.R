setwd("C:/Users/OWNER/OneDrive - Emory University/Spring 2020/Marketing Analytics/Projects/Pernalonga")

library(data.table)
library(dplyr)
library(stringr)
library(tidyr)
library(cluster)
library(factoextra)
library(ggplot2)
library(qdapTools)

# Who are the best customers in terms of revenues, profits, transactions/store visits, number of products, etc.?
# Are there interesting groupings of customers, e.g., most valuable (buy everything at any price) or cherry-pickers 
# (buy mostly on promotions), defined by certain categories (buy baby products or never buy milk), etc.?

####################### Reading in Data & Data Cleaning ###############################
# read in the files
products <- fread("product_table.csv", header = TRUE)
transaction <- fread("transaction_table.csv", header = TRUE)

# make transaction date into a date
transaction$tran_dt <- as.Date(transaction$tran_dt)

# get month and day of the week from the date column so we can group by months and days of the week
transaction$day_of_week <- weekdays(transaction$tran_dt)
transaction$month <- month(transaction$tran_dt)

# calculate discount rate for each transaction
transaction$discount_rate <- abs(transaction$tran_prod_discount_amt)/transaction$tran_prod_sale_amt

# Exclude transactions where paid amount is negative
transaction <- transaction[transaction$tran_prod_paid_amt>0,]

# merge the two datasets
all_data <- merge(transaction, products, by = "prod_id", all = TRUE)

# handle data anamolies
length(unique(all_data$tran_id)) # only 753 unique transaction_id's - Need to make a new transaction id, as the one available is not unique, duplicated very very often and thus prone to errors

# if a customer has >1 transactions on the same day in the same store, assume it was the same transaction
all_data$new_transaction_id <- paste(all_data$cust_id, all_data$tran_dt, all_data$store_id)

####################### Feature Engineering ###########################
# revenue by customer
all_data <- all_data %>%
  group_by(cust_id) %>% # group by customer id
  mutate(revenue = sum(tran_prod_paid_amt)) %>% # take the sum of all of the transaction paid amount for each customer
  arrange(desc(revenue)) # sort in descending order so we can see the top revenue producers at the top

# profit by customer
# because we can't calculate profit directly from the data, we need to infer it using 80/20 rule
# we need to get the count of each product sold, and categorize products into profit categories based on their frequency of purchase
all_data <- all_data %>%
  group_by(prod_id) %>% # group by product id
  mutate(num_products = sum(tran_prod_sale_qty)) %>% # take the sume of all product sale quantities
  arrange(desc(num_products)) # sort in descending order so we can see the top frequency products

# get just the unique products and their counts
prod_top_products <- all_data[,c('prod_id','num_products')] 
prod_top_products <- unique(prod_top_products) 

# now we need to assign products to categories based on where they fall in the distribution
prod_top_products <- prod_top_products[order(prod_top_products$num_products, decreasing=T),] # order the data by the num_products column we just created, in descending order
prod_top_products$profit_ratio <- 0 # initialize an empty column

# for loop to assign values to each product
for (i in 1:nrow(prod_top_products)){
  if (i/nrow(prod_top_products)<=0.2){
    prod_top_products$profit_ratio[i]<-'L'
  }
  else if(i/nrow(prod_top_products)<=0.4){
    prod_top_products$profit_ratio[i]<-'ML'
  }
  else if(i/nrow(prod_top_products)<=0.6){
    prod_top_products$profit_ratio[i]<-'M'
  }
  else if(i/nrow(prod_top_products)<=0.8){
    prod_top_products$profit_ratio[i]<-'MH'
  }
  else{prod_top_products$profit_ratio[i]<-'H'}
}

# now let's map these categories to the master data set
# first get just product id and profit ratio category
profit_products <- prod_top_products[,c(1,3)]
all_data <- merge(profit_products, all_data, by = "prod_id")

# let's get a count of high profit and medium/high profit purchases for each customer to act as a proxy for customers that produce the highest profit
all_data <- all_data %>%
  group_by(cust_id, profit_ratio) %>% # group by customer id
  mutate(count_profit_ratio = length(unique(new_transaction_id))) # get a count purchases by customer for each profit ratio category

# now let's just get the counts of the low profit margin purchases and assign it to each customer
# just take the rows that have "L" for profit ratio, with just the customer id and counts of profit ratios
high_profit_customers <- all_data[all_data$profit_ratio == "L", c(3,26)] # customer id column and profit ratio count column
high_profit_customers <- unique(high_profit_customers) # get just unique customers and their low profit margin counts
all_data <- merge(high_profit_customers, all_data, by = "cust_id") # merge back with master data set

# now we have a proxy for profit!

# transactions/store visits by customer
all_data <- all_data %>%
  group_by(cust_id) %>% # group by customer id
  mutate(num_transactions = length(new_transaction_id)) %>% # get the total number of transsactions for each customer
  arrange(desc(num_transactions)) # sort in descending order so we can see the customers with the most transactions at the top

# number of products by customer
all_data <- all_data %>%
  group_by(cust_id) %>% # group by customer id
  mutate(num_products = length(prod_id)) %>% # get the total number of products bought for each customer
  arrange(desc(num_products)) # sort in descending order so we can see the customers with the most products bought at the top

# total dollar amount of discounts used by customer
all_data <- all_data %>%
  group_by(cust_id) %>% # group by customer id
  mutate(total_discounts = sum(tran_prod_discount_amt != 0)) %>% # get the total value of discounts used for each customer
  arrange(total_discounts) # sort in descending order so we can see the customers with the most discounts used at the top

# spending by month for each customer
all_data <- all_data %>%
  group_by(month, cust_id) %>% # group by month and customer id
  mutate(spending_by_month = sum(tran_prod_paid_amt)) # get the sum of spending by month and customer id

# create quarter variable to group by
all_data$quarter <- ifelse(all_data$month %in% c(1,2,3), 1,
                           (ifelse(all_data$month %in% c(4,5,6),2,
                                   (ifelse(all_data$month %in% c(7,8,9),3,4)))))

# spending by quarter for each customer
all_data <- all_data %>%
  group_by(quarter, cust_id) %>% # group by quarter and customer id
  mutate(spending_by_quarter = sum(tran_prod_paid_amt)) # get the sum of spending by quarter and customer id

# spending by day for each customer
all_data <- all_data %>%
  group_by(day_of_week, cust_id) %>% # group by day of week and customer id
  mutate(spending_by_day = sum(tran_prod_paid_amt)) # get the sum of spending by day of week and customer id

# create weekday vs. weekend variable to group by
all_data$is_weekday <- ifelse(all_data$day_of_week %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"),1,0)

# spending by weekday vs. weekend for each customer
all_data <- all_data %>%
  group_by(is_weekday, cust_id) %>% # group by whether or not it is a weekday or weekend for each customer
  mutate(is_weekday_spend = sum(tran_prod_paid_amt)) # get the sum of spending for each customer for weekends and weekdays

# now let's get all the data together and ready for clustering for customers
# first we need to create columns to get spending for months and weekdays
# we will create one for every three months (each quarter) and one for weekends vs. weekdays
quarter_data <- all_data[,c(1,32,33)] # get cust_id, quarter, and spending_by_quarter
quarter_data <- unique(quarter_data) # get just unique
quarter_data <- pivot_wider(quarter_data, id_cols = cust_id, names_from = quarter, values_from = spending_by_quarter) # pivot the data
names(quarter_data) <- c("cust_id","q2","q1","q3","q4")

# now let's do weekdays
weekday_data <- unique(all_data[,c(1,35,36)]) # get cust_id, is_weekday, and is_weekday_spend - get just unique ones
weekday_data <- pivot_wider(weekday_data, id_cols = cust_id, names_from = is_weekday, values_from = is_weekday_spend) # pivot the data
names(weekday_data) <- c("cust_id", "weekday_spend","weekend_spend")

# now we will join these two together to get a unique customers dataframe
customers <- left_join(quarter_data, weekday_data, by = "cust_id")

# then get just the columns we created from all_data with cust_id
cust_data <- all_data[,c(1,2,25,26,28:30)] # cust_id, revenue, count_profit_ratio, num_products, num_transactions, total_discounts, total_spend
cust_data <- unique(cust_data)

# join with customers
customers <- left_join(customers, cust_data, by = "cust_id")

# write to csv so we don't have to run all this again
fwrite(customers, "customers.csv")

####################### Customer Segmentation/K-Means Clustering ###########################
# read in customers data
customers <- fread("customers.csv", header = TRUE)
customers <- na.omit(customers)

# to do k-means, let's first standardize the data
customers_2 <- scale(customers[,2:13])
row.names(customers_2) <- customers$cust_id # add the customer id's as row names
customers_2 <- data.frame(customers_2)

# try k-means on all variables first
# first use elbow method to find optimal k clusters
set.seed(555)
fviz_nbclust(customers_2, kmeans, method = "wss")
# optimal number = 3

# build model and look at clusters
model1 <- kmeans(customers_2, centers = 3, nstart = 25, iter.max = 20)
fviz_cluster(model1, data = customers_2, labelsize = 0)

# within clusters sum of squares divided by total sum of square
sum(model1$withinss)/model1$totss
# 49.85%

# add cluster number to the original data
customers$model1_cluster <- model1$cluster

# split each cluster into new datasets so we can calculate column means
model1_segment1 <- customers[customers$model1_cluster == 1,]
model1_segment2 <- customers[customers$model1_cluster == 2,]
model1_segment3 <- customers[customers$model1_cluster == 3,]

# calculate column means
model1_segment1_colmeans <- data.frame(colMeans(model1_segment1, na.rm = TRUE, dims = 1))
colnames(model1_segment1_colmeans) <- "colmeans"

model1_segment2_colmeans <- data.frame(colMeans(model1_segment2, na.rm = TRUE, dims = 1))
colnames(model1_segment2_colmeans) <- "colmeans"

model1_segment3_colmeans <- data.frame(colMeans(model1_segment3, na.rm = TRUE, dims = 1))
colnames(model1_segment3_colmeans) <- "colmeans"

# join the column means
model1_colmeans <- cbind(model1_segment1_colmeans, model1_segment2_colmeans, model1_segment3_colmeans)
colnames(model1_colmeans) <- c("segment1","segment2","segment3")

# clusters aren't very clear with so many variables
# want to try different combinations of variables for k-means, instead of using all of them at once

# let's first try spending by different time periods and num_products - quarter, weekday vs. weekend, num_products
# get just the columns we want
customers_3 <- customers_2[,c(1:6,9)]
# first use elbow method to find optimal k clusters
set.seed(555)
fviz_nbclust(customers_3, kmeans, method = "wss")
# optimal number = 3

# build model and look at clusters
model2 <- kmeans(customers_3, centers = 3, nstart = 25, iter.max = 20)
fviz_cluster(model2, data = customers_3, labelsize = 0)

# within clusters sum of squares divided by total sum of square
sum(model2$withinss)/model2$totss
# 50.34%

# add cluster number to the original data
customers$model2_cluster <- model2$cluster

# split each cluster into new datasets so we can calculate column means
model2_segment1 <- customers[customers$model2_cluster == 1,]
model2_segment2 <- customers[customers$model2_cluster == 2,]
model2_segment3 <- customers[customers$model2_cluster == 3,]

# calculate column means
model2_segment1_colmeans <- data.frame(colMeans(model2_segment1, na.rm = TRUE, dims = 1))
colnames(model2_segment1_colmeans) <- "colmeans"

model2_segment2_colmeans <- data.frame(colMeans(model2_segment2, na.rm = TRUE, dims = 1))
colnames(model2_segment2_colmeans) <- "colmeans"

model2_segment3_colmeans <- data.frame(colMeans(model2_segment3, na.rm = TRUE, dims = 1))
colnames(model2_segment3_colmeans) <- "colmeans"

# join the column means
model2_colmeans <- cbind(model2_segment1_colmeans, model2_segment2_colmeans, model2_segment3_colmeans)
colnames(model2_colmeans) <- c("segment1","segment2","segment3")

# let's next try our "financial" variables - revenue, count_profit_ratio, total_discounts
# get just the columns we want
customers_4 <- customers_2[,c(7,8,11)]
# first use elbow method to find optimal k clusters
set.seed(555)
fviz_nbclust(customers_4, kmeans, method = "wss")
# optimal number = 4

# build model and look at clusters
model3 <- kmeans(customers_4, centers = 4, nstart = 25, iter.max = 20)
fviz_cluster(model3, data = customers_4, labelsize = 0)

# within clusters sum of squares divided by total sum of squares
sum(model3$withinss)/model3$totss
# 42.62%

# add cluster number to the original data
customers$model3_cluster <- model3$cluster

# split each cluster into new datasets so we can calculate column means
model3_segment1 <- customers[customers$model3_cluster == 1,]
model3_segment2 <- customers[customers$model3_cluster == 2,]
model3_segment3 <- customers[customers$model3_cluster == 3,]
model3_segment4 <- customers[customers$model3_cluster == 4,]

# calculate column means
model3_segment1_colmeans <- data.frame(colMeans(model3_segment1, na.rm = TRUE, dims = 1))
colnames(model3_segment1_colmeans) <- "colmeans"

model3_segment2_colmeans <- data.frame(colMeans(model3_segment2, na.rm = TRUE, dims = 1))
colnames(model2_segment2_colmeans) <- "colmeans"

model3_segment3_colmeans <- data.frame(colMeans(model3_segment3, na.rm = TRUE, dims = 1))
colnames(model3_segment3_colmeans) <- "colmeans"

model3_segment4_colmeans <- data.frame(colMeans(model3_segment4, na.rm = TRUE, dims = 1))
colnames(model3_segment4_colmeans) <- "colmeans"

# join the column means
model3_colmeans <- cbind(model3_segment1_colmeans, model3_segment2_colmeans, model3_segment3_colmeans, model3_segment4_colmeans)
colnames(model3_colmeans) <- c("segment1","segment2","segment3","segment4")

# model3 has lowest between clusters sum of squares
