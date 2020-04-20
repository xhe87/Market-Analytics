rm(list = ls())
setwd('/Users/patrickhe/Desktop/MSBA/OneDrive - Emory University/Marketing Analysis/Projects/Pernalonga')

library(data.table)
library(dplyr)
library(stringr)
library(tidyverse)
library(cluster)
library(factoextra)
library(lubridate)
############################################################################################################
products <- fread('product_table.csv')
transactions <-fread("transaction_table.csv")
merged_data <- merge(transactions, products, by='prod_id', all=TRUE)

#Handle data anamolies
length(unique(merged_data$tran_id)) #Only 753 unique transaction_id's - Need to make a new transaction id, as the one available is not unique, duplicated very very often and thus prone to errors

#Since each transaction may have multiple rows (depending on number of products types in a transaction), not possible to accurately determinew which transactions were made together
#Way around: Combine customer_id's, store_id's, and purchase date to make a single transaction id as most customers would only make 1 purchase a day at a particular store

#Bring transaction date to date format
merged_data$date <- as.Date(merged_data$tran_dt)

#If a customer has >1 transactions on the same day in the same store, assume it was the same transaction
merged_data$new_transaction_id <- paste(merged_data$cust_id,merged_data$tran_dt,merged_data$store_id)

#How many transactions in total?
length(unique(merged_data$new_transaction_id)) #2.83 mil transactions made across all stores in 2016 and 2017

a = merged_data[, c('prod_id', 'tran_dt', 'prod_unit','tran_prod_sale_amt','tran_prod_sale_qty','tran_prod_discount_amt',
                    'tran_prod_offer_cts','tran_prod_paid_amt','prod_unit_price','category_desc_eng',
                    'brand_desc','new_transaction_id')]
write.csv(a,"transaction_data.csv")

#Exclude sales where paid amount is negative
merged_data <- merged_data[merged_data$tran_prod_paid_amt>0,]

############################################################################################################
# Read Table
merged_data = fread("transaction_data.csv")

# Feature Engineering 

# 1.If the transaction is made in weekdays
merged_data$dayofweek= wday(as.Date(merged_data$tran_dt,'%Y-%m-%d'))
merged_data$weekday = ifelse(merged_data$dayofweek < 6, 1, 0)

# 2.After discount sale price
merged_data$mean_price_after_discount = (merged_data$tran_prod_sale_amt + merged_data$tran_prod_discount_amt)/merged_data$tran_prod_sale_qty

# 3.Discount Percentage
merged_data$discount_percentage = merged_data$tran_prod_discount_amt/merged_data$tran_prod_sale_amt

# 4.If the product is on discount (There're multiple offers applied to a product, so we need to transfer the 
# offer number greater than 1 to 1)
merged_data$tran_prod_offer_cts = ifelse(merged_data$tran_prod_offer_cts >=1, 1, 0)
############################################################################################################
# Group by Prod_id

# profit by product
# because we can't calculate profit directly from the data, we need to infer it using 80/20 rule
# we need to get the count of each product sold, and categorize products into profit categories based on their frequency of purchase
merged_data <- merged_data %>%
  group_by(prod_id) %>% # group by product id
  mutate(num_products = sum(tran_prod_sale_qty)) %>% # take the sume of all product sale quantities
  arrange(desc(num_products)) # sort in descending order so we can see the top frequency products

# get just the unique products and their counts
prod_top_products <- merged_data[,c('prod_id','num_products')] 
prod_top_products <- unique(prod_top_products) 

# now we need to assign products to categories based on where they fall in the distribution
prod_top_products <- prod_top_products[order(prod_top_products$num_products, decreasing=T),] # order the data by the num_products column we just created, in descending order
prod_top_products$profit_ratio <- 0 # initialize an empty column

# for loop to assign values to each product
for (i in 1:nrow(prod_top_products)){
  if (i/nrow(prod_top_products)<=0.2){
    prod_top_products$profit_ratio[i]<- 1
  }
  else if(i/nrow(prod_top_products)<=0.4){
    prod_top_products$profit_ratio[i]<- 2
  }
  else if(i/nrow(prod_top_products)<=0.6){
    prod_top_products$profit_ratio[i]<- 3
  }
  else if(i/nrow(prod_top_products)<=0.8){
    prod_top_products$profit_ratio[i]<- 4
  }
  else{prod_top_products$profit_ratio[i]<- 5}
}

# now let's map these categories to the master data set
# first get just product id and profit ratio category
profit_products <- prod_top_products[,c(1,3)]
merged_data <- merge(profit_products, merged_data, by = "prod_id")
# now we have a proxy for profit!
############################################################################################################

# 1. Total product sales in terms of volume (quantity)
merged_data <- merged_data %>%
  group_by(prod_id) %>%
  mutate(product_sale_quantity= sum(tran_prod_sale_qty))

#2. Total product sales in terms of revenue
merged_data <- merged_data %>%
  group_by(prod_id) %>%
  mutate(product_sales= sum(tran_prod_paid_amt))

#3. Percentage of product sales with promotion
merged_data <- merged_data %>%
  group_by(prod_id) %>%
  mutate(perc_discount_transactions= sum(tran_prod_offer_cts)/n())

#4. Average discount percentage of the product
merged_data <- merged_data %>%
  group_by(prod_id) %>%
  mutate(perc_discount= sum(discount_percentage*(-1))/n())

#5. Mean Sale Price of the Product
merged_data <- merged_data %>%
  group_by(prod_id) %>%
  mutate(mean_sale_price= sum(mean_price_after_discount)/n())

#6. Percentage of transactions made in weekdays
merged_data <- merged_data %>%
  group_by(prod_id) %>%
  mutate(week_day= sum(weekday)/n())

#7. Number of transactions made for each product
merged_data <- merged_data %>%
  group_by(prod_id) %>%
  mutate(pro_num_transactions= length(unique(new_transaction_id)))

#Extract relevant columns for product clustering
product_features <- c("prod_id","product_sale_quantity", "product_sales","perc_discount_transactions",     
                    "perc_discount", "mean_sale_price" , "week_day","pro_num_transactions","profit_ratio")

# CT & KG
merged_data_ct = merged_data[merged_data$prod_unit=="CT",]
merged_data_kg = merged_data[merged_data$prod_unit=="KG",]
rm(merged_data)
############################################################################################################

#Keep only these relevant features
product_data_ct <- merged_data_ct[,product_features]

#Keep data granularity at store level. i.e. 1 row for each store with all aggregate measures as calculated above
product_data_ct <- distinct_at(product_data_ct,.vars="prod_id",.keep_all = TRUE)

#Change from grouped_df to data frame to do k-means clustering
product_data_ct <- as.data.frame(product_data_ct)
rm(merged_data_ct)

#Exclude prod_id this isnt an attribute used in clustering
prod_clustering_data <- product_data_ct[,c(2,3,4,5,6,7,8)]

#Standardize columns by subtracting mean and diving by standard deviation. First, save store_id of each store as an index/rowname
row.names(prod_clustering_data) <- product_data_ct$prod_id

prod_clustering_data_scaled <- scale(prod_clustering_data)

#Get same results everytime
set.seed(42)

#Elbow curve to determine optimal number of clusters
fviz_nbclust(prod_clustering_data_scaled, kmeans, method = "wss") #3 clusters

#K-means
product_kmeans <- kmeans(prod_clustering_data_scaled, centers = 3, nstart = 15, iter.max = 10)

#Visualize the clusters
fviz_cluster(product_kmeans, data = prod_clustering_data_scaled,labelsize = 0)

#Variance explained by the clusters
sum(product_kmeans$withinss) / product_kmeans$totss #61.30%

#Save cluster number (1/2/3) as a feature. This will be used to segment data and look at descriptive statistics for each segment. e.g. mean
prod_clustering_data$cluster_number <- product_kmeans$cluster
prod_clustering_data$profit_ratio <- product_data_ct$profit_ratio

#Segment by customer and see cluster means for the store features calculated
cluster_1 <- filter(prod_clustering_data,cluster_number==1)
cluster_2 <- filter(prod_clustering_data,cluster_number==2)
cluster_3 <- filter(prod_clustering_data,cluster_number==3)

#See mean values for each feature within a cluster
round(colMeans(x=cluster_1, na.rm = TRUE),2)
round(colMeans(x=cluster_2, na.rm = TRUE),2)
round(colMeans(x=cluster_3, na.rm = TRUE),2)

############################################################################################################

#Keep only these relevant features
product_data_kg <- merged_data_kg[,product_features]
#Keep data granularity at store level. i.e. 1 row for each store with all aggregate measures as calculated above
product_data_kg <- distinct_at(product_data_kg,.vars="prod_id",.keep_all = TRUE)

#Change from grouped_df to data frame to do k-means clustering
product_data_kg <- as.data.frame(product_data_kg)
rm(merged_data_kg)

#Exclude prod_id this isnt an attribute used in clustering
prod_clustering_data <- product_data_kg[,c(2,3,4,5,6,7,8)]

#Standardize columns by subtracting mean and diving by standard deviation. First, save store_id of each store as an index/rowname
row.names(prod_clustering_data) <- product_data_kg$prod_id

prod_clustering_data_scaled <- scale(prod_clustering_data)

#Get same results everytime
set.seed(42)

#Elbow curve to determine optimal number of clusters
fviz_nbclust(prod_clustering_data_scaled, kmeans, method = "wss") #3 clusters

#K-means
product_kmeans <- kmeans(prod_clustering_data_scaled, centers = 3, nstart = 15, iter.max = 10)

#Visualize the clusters
fviz_cluster(product_kmeans, data = prod_clustering_data_scaled,labelsize = 0)

#Variance explained by the clusters
sum(product_kmeans$withinss) / product_kmeans$totss #56.3%

#Save cluster number (1/2/3) as a feature. This will be used to segment data and look at descriptive statistics for each segment. e.g. mean
prod_clustering_data$cluster_number <- product_kmeans$cluster
prod_clustering_data$profit_ratio <- product_data_kg$profit_ratio

#Segment by customer and see cluster means for the store features calculated
cluster_4 <- filter(prod_clustering_data,cluster_number==1)
cluster_5 <- filter(prod_clustering_data,cluster_number==2)
cluster_6 <- filter(prod_clustering_data,cluster_number==3)

#See mean values for each feature within a cluster
round(colMeans(x=cluster_4, na.rm = TRUE),2)
round(colMeans(x=cluster_5, na.rm = TRUE),2)
round(colMeans(x=cluster_6, na.rm = TRUE),2)





