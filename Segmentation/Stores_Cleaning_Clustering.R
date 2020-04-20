
#First, load all required packages

library(data.table)
library(dplyr)
library(stringr)
library(tidyverse)
library(cluster)
library(factoextra)

#Load the datasets

transactions <- fread("C:/Users/harsh/Desktop/MSBA/Spring/Marketing Analytics/Projects/Project 1/Pernalonga/transaction_table.csv")
products <-fread("C:/Users/harsh/Desktop/MSBA/Spring/Marketing Analytics/Projects/Project 1/Pernalonga/product_table.csv")

##Data Prep for EDA

#Map product id's to other product features by joining the two tables
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
length(unique(merged_data$new_transaction_id)) #2.83 mil transactions made across all stores in 2016 and 2017 - likely underestimated but should reveal directionally accurate insights

#Verify the amount paid is consistent with (unit price * quantity - discount amount)
merged_data$transaction_amount <- (merged_data$prod_unit_price * merged_data$tran_prod_sale_qty) - abs(merged_data$tran_prod_discount_amt)

#About 33% product prices are not consistent (10 mil out of 29.6mil)
merged_data$IS_CONSISTENT_FINAL_AMOUNT <- ifelse(merged_data$tran_prod_paid_amt==merged_data$transaction_amount,1,0)
inconsistent_prices <- filter(merged_data,merged_data$IS_CONSISTENT_FINAL_AMOUNT==0) 

#How much is this inconsistency?
head(sort(merged_data$difference,decreasing=T),5) #Very minor differences (e-08 at most), likely due to rounding


#We noticed some transaction amounts are negative, so we removed these from our analysis

merged_data <- merged_data[merged_data$tran_prod_paid_amt>0,]

#Now, consider Profit. Bcause we can't calculate profit directly from the data, we would need to infer it using 80/20 rule or similar

#Since each row refers to 1 product and may be part of a bigger transaction, classify each product as H/M/L profit based on its overall sales across the data
#It's likely that high volume and penetration products have lower margins, while low volume and penetration products have L/M margins

merged_data <- merged_data %>%
  group_by(prod_id) %>% # group by product id
  mutate(num_products = sum(tran_prod_sale_qty)) %>% # take the sume of all product sale quantities
  arrange(desc(num_products)) # sort in descending order so we can see the top frequency products

# get just the unique products and their overall sale quantities
prod_top_products <- merged_data[,c('prod_id','num_products')] 
prod_top_products <- unique(prod_top_products) 

# now we need to assign products to categories based on how they rank in terms of overall sales
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

merged_data <-merge(merged_data, prod_top_products[,c("prod_id","profit_ratio")], by = "prod_id")

#FEATURE ENGINEERING - Create store level features to better understand which stores are similar to each other vs. different from each other based on factors such as sales, product portfolio, etc.

#Number of transactions made in each store
merged_data <- merged_data %>%
  group_by(store_id) %>%
  mutate(store_num_transactions= length(unique(new_transaction_id)))

#Avg number of items per order (including multiple quantities in the same order)
merged_data <- merged_data %>%
  group_by(store_id) %>%
  mutate(avg_store_transaction_volume= mean(tran_prod_sale_qty))

#Total store sales in terms of revenue
merged_data <- merged_data %>%
  group_by(store_id) %>%
  mutate(store_sales= sum(tran_prod_paid_amt))

#Average transaction amount in each store
merged_data <- merged_data %>%
  group_by(store_id) %>%
  mutate(avg_store_transaction_amount= sum(tran_prod_paid_amt)/length(unique(new_transaction_id)))

#Number of unique products sold by store
merged_data <- merged_data %>%
  group_by(store_id) %>%
  mutate(store_num_products= length(unique(prod_id)))

#Number of products sold with discount by store) %>%
merged_data <- merged_data %>%
  group_by(store_id) %>%
  mutate(discount_transactions= sum(tran_prod_discount_amt<0))

#Avg discount per transaction in each store
merged_data <- merged_data %>%
  group_by(store_id,new_transaction_id) %>%
  mutate(avg_discount_by_transaction= sum(tran_prod_discount_amt<0)/length(unique(new_transaction_id)))

#Percentage of products sold at a discount in that store
merged_data <- merged_data %>%
  group_by(store_id) %>%
  mutate(perc_discount_products= sum(tran_prod_discount_amt<0)/n())

#Percentage discount for each store. i.e. Ratio of actual discount amount and sales if there were no discounts
merged_data <- merged_data %>%
  group_by(store_id) %>%
  mutate(perc_discount_of_total= sum(abs(tran_prod_discount_amt))/sum(tran_prod_sale_amt))

#Number of transactions for each store
merged_data <- merged_data %>%
  group_by(store_id) %>%
  mutate(store_transactions= length(unique(new_transaction_id)))

#Store customer base
merged_data <- merged_data %>%
  group_by(store_id) %>%
  mutate(store_customers= length(unique(cust_id)))

#Exclude store_id = 302 as it has only 1 transaction in the data
merged_data <- merged_data[merged_data$store_id!=302,]
#Keep relevant columns
merged_data <- merged_data[,c(1:22,24:ncol(merged_data))] #Exclude profit ratio as we don't need it anymore

##-- Clustering Analysis for segmentation

#Extract relevant columns for store clustering. Keep store_id as reference to match segments back to individual stores
store_features <- c("store_id", 
                    "avg_store_transaction_amount",
                    "perc_discount_products","perc_discount_of_total")

#Keep only these relevant features to feed into K-Means
store_data <- merged_data[,store_features]

#Keep data granularity at store level. i.e. 1 row for each store with all aggregate measures as calculated above
merged_data_storelevel <- distinct_at(merged_data,.vars="store_id",.keep_all = TRUE) #Keep all columns seperately to see insights on all relevant features
store_data <- distinct_at(store_data,.vars="store_id",.keep_all = TRUE) #For K-means

#Change from grouped_df to data frame to do k-means clustering
store_data <- as.data.frame(store_data)

#Exclude store_id this isnt an attribute used in clustering
store_clustering_data <- store_data[,c(2:ncol(store_data))]

#Standardize columns by subtracting mean and diving by standard deviation. First, save store_id of each store as an index/rowname
row.names(store_clustering_data) <- store_data$store_id
store_clustering_data_scaled <- scale(store_clustering_data)

#Get same results everytime
set.seed(42)

#Elbow curve to determine optimal number of clusters
fviz_nbclust(store_clustering_data_scaled, kmeans, method = "wss") #3 clusters

#K-means
stores_kmeans <- kmeans(store_clustering_data_scaled, centers = 3, nstart = 15, iter.max = 30)

#Visualize the clusters
fviz_cluster(stores_kmeans, data = store_clustering_data_scaled,labelsize = 0)

#Variance explained by the clusters
sum(stores_kmeans$withinss) / stores_kmeans$totss #44%

#Save cluster number (1/2/3) as a feature. This will be used to segment data and look at descriptive statistics for each segment. e.g. mean
store_clustering_data$cluster_number <- stores_kmeans$cluster

#Also save this info in the original data that has all engineered features, not just the ones that were used in clustering
merged_data_storelevel$cluster_number <- stores_kmeans$cluster


#Segment by customer and see cluster means for the store features calculated
cluster_1 <- filter(merged_data_storelevel,cluster_number==1)
cluster_2 <- filter(merged_data_storelevel,cluster_number==2)
cluster_3 <- filter(merged_data_storelevel,cluster_number==3)


#See mean values for each feature within a cluster
segment_1_summary <- colMeans(x=cluster_1[,c(21:ncol(cluster_1))], na.rm = TRUE)
segment_2_summary <-colMeans(x=cluster_2[,c(21:ncol(cluster_2))], na.rm = TRUE)
segment_3_summary <-colMeans(x=cluster_3[,c(21:ncol(cluster_3))], na.rm = TRUE)

segment_summaries <- as.data.frame(cbind(segment_1_summary,segment_2_summary,segment_3_summary)) #View to see mean values for all relevant features for each segment
View(segment_summaries)

#-----------

#For Visualization EDA

# Cumulative store sales - What % of stores have 80% of the overall sales?

stores_cumsales <- merged_data %>%
  group_by(store_id) %>%
  mutate(sales = sum(tran_prod_paid_amt)) %>%
         arrange(desc(sales))

stores_cumsales <- distinct_at(stores_cumsales,.vars="store_id",.keep_all = T)
stores_cumsales$cum_sale_perc <- cumsum(stores_cumsales$sales) / Total_Sales
stores_cumsales$stores_perc <- as.integer(rownames(stores_cumsales))
stores_cumsales$stores_cum_perc <- stores_cumsales$stores_perc/nrow(stores_cumsales)

ggplot(stores_cumsales,aes(x=stores_cum_perc,y=cum_sale_perc)) + geom_line(colour="blue") + theme_classic() + labs(x="% Stores",y="Cumulative Sales")+geom_hline(yintercept = 0.8,linetype="dashed") + geom_vline(xintercept=0.6,linetype="dashed")

# Cumulative product sales - What % of products have 80% of the overall sales?

product_cumsales <- merged_data %>%
  group_by(prod_id) %>%
  mutate(sales = sum(tran_prod_paid_amt)) %>%
  arrange(desc(sales))

product_cumsales <- distinct_at(product_cumsales,.vars="prod_id",.keep_all = T)
product_cumsales$cum_sale_perc <- cumsum(product_cumsales$sales) / Total_Sales
product_cumsales$product_perc <- as.integer(rownames(product_cumsales))
product_cumsales$product_cum_perc <- product_cumsales$product_perc/nrow(product_cumsales)

ggplot(product_cumsales,aes(x=product_cum_perc,y=cum_sale_perc)) + geom_line(colour="blue") + theme_classic() + labs(x="% Products",y="Cumulative Sales")+geom_hline(yintercept = 0.8,linetype="dashed") + geom_vline(xintercept=0.28,linetype="dashed")


# Cumulative customer sales - What % of customers have 80% of the overall sales?

#Sales by customer, sorted by sales descending 
customer_cumsales <- merged_data %>%
  group_by(cust_id) %>%
  mutate(sales = sum(tran_prod_paid_amt)) %>%
  arrange(desc(sales))  

customer_cumsales <- distinct_at(customer_cumsales,.vars="cust_id",.keep_all = T)
customer_cumsales$cum_sale_perc <- cumsum(customer_cumsales$sales) / Total_Sales
customer_cumsales$customer_perc <- as.integer(rownames(customer_cumsales))
customer_cumsales$customer_cum_perc <- customer_cumsales$customer_perc/nrow(customer_cumsales)

ggplot(customer_cumsales,aes(x=customer_cum_perc,y=cum_sale_perc)) + geom_line(colour="blue") + theme_classic() + labs(x="% Customers",y="Cumulative Sales")+geom_hline(yintercept = 0.8,linetype="dashed") + geom_vline(xintercept=0.72,linetype="dashed")


#Sales by day of the week

merged_data$weekday <- weekdays(merged_data$date)

sales_by_day <- merged_data %>%
  group_by(weekday) %>%
  mutate(weekday_sales = sum(tran_prod_paid_amt))


sales_by_day <- sales_by_day[,c("weekday_sales","weekday")]
sales_by_day <- distinct_at(sales_by_day,.vars="weekday",.keep_all = T)
sales_by_day$weekday <- factor(sales_by_day$weekday,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))


ggplot(sales_by_day,aes(x=weekday,y=weekday_sales)) + geom_bar(fill="steelblue",stat="identity") + theme_classic() + labs(x="Day of the week",y="Total Sales")

# Top 10 customers by sales
customer_cumsales$cust_id <- as.character(customer_cumsales$cust_id)

ggplot(customer_cumsales[1:10,c('cust_id','sales')],aes(x=reorder(cust_id,-sales),y=sales)) + geom_bar(fill="steelblue",stat="identity") + theme_classic()+xlab("Customer id")+ ylab("Revenue in USD")

# Top 10 stores by sales
stores_cumsales$cust_id <- as.character(stores_cumsales$cust_id)

ggplot(stores_cumsales[1:10,c('store_id','sales')],aes(x=reorder(store_id,-sales),y=sales)) + geom_bar(fill="steelblue",stat="identity") + theme_classic()+xlab("Store id")+ ylab("Revenue in USD")

# Top 10 products by sales
product_cumsales$prod_id <- as.character(product_cumsales$prod_id)

ggplot(product_cumsales[1:10,c('prod_id','sales')],aes(x=reorder(prod_id,-sales),y=sales)) + geom_bar(fill="steelblue",stat="identity") + theme_classic()+xlab("Product id")+ ylab("Revenue in USD")

