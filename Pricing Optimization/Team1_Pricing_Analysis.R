#First, load all required packages

library(data.table)
library(dplyr)
library(stringr)
library(tidyverse)
library(cluster)
library(factoextra)
library(psych)
library(plyr)
library(arules)

#Load the datasets

transactions <- fread("C:/Users/harsh/Desktop/MSBA/Spring/Marketing Analytics/Projects/Project 1/Pernalonga/transaction_table.csv")
products <-fread("C:/Users/harsh/Desktop/MSBA/Spring/Marketing Analytics/Projects/Project 1/Pernalonga/product_table.csv")

##Data Prep

#Map product id's to other product features by joining the two tables
merged_data <- merge(transactions, products, by='prod_id', all=TRUE)

rm(transactions)
rm(products)

#Since each transaction may have multiple rows (depending on number of products types in a transaction), not possible to accurately determinew which transactions were made together
#Way around: Combine customer_id's, store_id's, and purchase date to make a single transaction id as most customers would only make 1 purchase a day at a particular store

#Bring transaction date to date format
merged_data$date <- as.Date(merged_data$tran_dt)

#If a customer has >1 transactions on the same day in the same store, assume it was the same transaction
merged_data$new_transaction_id <- paste(merged_data$cust_id,merged_data$tran_dt,merged_data$store_id)

#We noticed some transaction amounts are negative, so we removed these from our analysis

merged_data <- merged_data[merged_data$tran_prod_paid_amt>0,]

##New analysis begins here----

#Load fresh product mapping - This is done manually (e.g. Banana, Milk, Apple, etc.)

fresh_prod <- fread("C:/Users/harsh/Desktop/MSBA/Spring/Marketing Analytics/Projects/Project 3/Fresh_product_mapping.csv")

#Identify fresh products and remove them

all_data <- left_join(merged_data,fresh_prod,by="category_id")

all_data$tran_dt <- as.Date(all_data$tran_dt) #Keep as date

all_data <- all_data[all_data$fresh_product==0 & !is.na(all_data$fresh_product),]  #fresh_product flag is 1 if it's a fresh product

#EDA on prices in april 2017---

#Looking at historical data
# This might want to be moved up in our code
# third week of April 2017
thirdwk_april <- as.data.table(all_data[all_data$tran_dt >= "2017-04-13" & all_data$tran_dt <= "2017-04-19",])
# second week of April 2017
secondwk_april <- as.data.table(all_data[all_data$tran_dt >= "2017-04-06" & all_data$tran_dt <= "2017-04-12",])

# number of transactions for third week of April 2017
thirdwk_april <- thirdwk_april %>%
  group_by(new_transaction_id) %>%
  mutate(count_trans = n())
thirdwk_april_trans <- unique(thirdwk_april[,c("new_transaction_id","count_trans")])

# number of transactions for second week of April 2017
secondwk_april <- secondwk_april %>%
  group_by(new_transaction_id) %>%
  mutate(count_trans = n())
secondwk_april_trans <- unique(secondwk_april[,c("new_transaction_id","count_trans")])

# find all product/store combinations for third week of April 2017
prod_store_april_third <- thirdwk_april[,.(mean(prod_unit_price),
                                           sum(tran_prod_sale_qty)), by=.(prod_id, store_id)]
colnames(prod_store_april_third) <- c("prod_id", "store_id", "avg_unit_price_april_third", "qty_sales_april_third")

# find all product/store combinations for second week of April 2017
prod_store_april_second <- secondwk_april[,.(mean(prod_unit_price),
                                             sum(tran_prod_sale_qty)), by=.(prod_id, store_id)]
colnames(prod_store_april_second) <- c("prod_id", "store_id", "avg_unit_price_april_second", "qty_sales_april_second")

# merge both weeks of April 2017 product/store prices together
prod_store_price <- merge(prod_store_april_third, prod_store_april_second, by=c("prod_id", "store_id"), all=TRUE)

# find sales and mean price of each product for third week of April 2017
prod_april_third <- thirdwk_april[,.(mean(prod_unit_price),
                                     sum(tran_prod_sale_qty)), by=.(prod_id)]
colnames(prod_april_third) <- c("prod_id", "april_third_avg_price", "qty_sales_april_third")

# find sales and mean price of each product for second week of April 2017
prod_april_second <- secondwk_april[,.(mean(prod_unit_price),
                                       sum(tran_prod_sale_qty)), by=.(prod_id)]
colnames(prod_april_second) <- c("prod_id", "april_second_avg_price", "qty_sales_april_second")

# merge both weeks of April 2018 product sales and average prices together
prod_price <- merge(prod_april_third, prod_april_second, by=c("prod_id"), all=TRUE)

# find difference between average price in two weeks
prod_price$april_weeks_diff <- prod_price$april_third_avg_price - prod_price$april_second_avg_price

# find percent difference
prod_price$percent_diff <- prod_price$april_weeks_diff / prod_price$april_second_avg_price
summary(prod_price$percent_diff)

# Create histogram of percent changes in prices
hist(prod_price$percent_diff, breaks = 20, xlab = "Percent Price Change", ylab = "Number of Products", 
     main = "Percent Change in Price between Second Week and Third Week in April, 2017")

# Now let's look at differences in quantities sold
# find the difference between avg quantity sold in two weeks
prod_price$april_weeks_qty_diff <- prod_price$qty_sales_april_third - prod_price$qty_sales_april_second

# find percent difference
prod_price$qty_percent_diff <- prod_price$april_weeks_qty_diff / prod_price$qty_sales_april_second
summary(prod_price$qty_percent_diff)

# Create histogram of percent changes in quantity sold/demand
hist(prod_price$qty_percent_diff, breaks = 20, xlab = "Percent Quantity Sold Change", 
     ylab = "Number of Products", main = "Percent Change in Demand between Second Week and Third Week in April, 2017")

# Now let's take a look at how average prices from 2017 compare to current prices
# first get only columns we need - prod_id, store_id, and prod_unit_price
thirdwk_april_sub <- data.table(thirdwk_april[,c("prod_id","store_id","prod_unit_price")])

# calculate average unit price for each product in each store
thirdwk_april_sub_avgprice <- thirdwk_april_sub[, mean(prod_unit_price), by = .(prod_id, store_id)]
colnames(thirdwk_april_sub_avgprice)[colnames(thirdwk_april_sub_avgprice) == "V1"] <- "avgprice_2017"

# merge 2017 average sale prices back with original data
# we want to compare current product/store price with the historical 2017 average
all_data_historical <- merge(all_data, thirdwk_april_sub_avgprice, by = c("prod_id", "store_id"),all.x = TRUE)

# remove any NAs in the data
all_data_historical <- all_data_historical[!is.na(all_data_historical$avgprice_2017)]

# now, find the difference between each price and 2017 average for that product/store
all_data_historical$diff_2017 <- all_data_historical$prod_unit_price - all_data_historical$avgprice_2017


#Creating long tail graph
# first get a sum of all transaction sale amount by category
sale_amt_category <- all_data[, sum(tran_prod_sale_amt), by = category_desc_eng]
colnames(sale_amt_category)[colnames(sale_amt_category) == "V1"] <- "sale_amt_category"

# calculate the percent of each transaction out of the total category sum
sale_amt_category[, percent := sale_amt_category/sum(sale_amt_category)]

# then get a sum of all transaction quantities by category
sale_qty_category <- all_data[, sum(tran_prod_sale_qty), by = category_desc_eng]
colnames(sale_qty_category)[colnames(sale_qty_category) == "V1"] <- "sale_qty_category"

# sort the sales amount by decreasing percent
sale_amt_category <- sale_amt_category[order(-percent),]

# now get the cumulative sum of percentages to plot against number of product categories
cumulative_vals <- as.data.frame(cumsum(sale_amt_category$percent))
cumulative_vals$index <- c(1:length(sale_amt_category$sale_amt_category))
colnames(cumulative_vals) <- c("percent","index")

# plot
ggplot(cumulative_vals, aes(x=index,y=percent)) +
  geom_point(color="red") +
  scale_y_continuous('Percent of Sales',breaks=seq(0,1,.10))+
  scale_x_continuous('Number of Product Categories',breaks=seq(0,450,50)) +
  geom_hline(yintercept = .8, color = 'black') +
  geom_vline(xintercept = 72, color = 'black') +
  theme_bw()

#--- EDA complete

## Find the two focus categories for Promotions ---

#Number of products per category

all_data <- all_data %>%
  group_by(category_id) %>%
  mutate(num_products = uniqueN(prod_id))

#Since our promotion will be for products sold in atleast 10 stores, remove all products that are sold in <=10 stores

all_data <- all_data %>%
  group_by(prod_id) %>%
  mutate(stores_per_product = uniqueN(store_id))

all_data <- all_data[all_data$stores_per_product>9,]

#Stores and products in focus

stores <- unique(all_data$store_id)
products <- unique(all_data$prod_id)

all_data <- as.data.table(all_data)

#Now, calculate product elasticity. Our aim is to find fairly elastic products and shortlist categories that have a high amount of elastic products. To do this:

#1. For each product, make a list of all historic price points
#2. See the volume sold at each price point (sum of quantities/demand)
#3. Calculate elasticity - For this we will need the slope (Beta) at each price point, alpha (Intercept)

elasticity_vec <- list()
alpha_list <-c()


for (i in 1:length(products)){
  temp <- all_data[all_data$prod_id == products[i]]
  price_points <- unique(temp$prod_unit_price)
  #Blank vector to save price and demand at that price
  price_vec <-c()
  demand_vec <- c()
  #Loop over each price point and calculate demand
  for (j in 1:length(price_points)){
    demand <- sum(temp$tran_prod_sale_qty[temp$prod_unit_price == price_points[j]])
    price_vec <-c(price_vec,price_points[j])
    demand_vec <- c(demand_vec,demand)
  }
  #Calculate elasticity using the price and demand as the Slope of an OLS regression multiplied by the mean price/mean demand
  elasticity_df <- as.data.frame(cbind(price_vec, demand_vec))
  slope <- lm(demand_vec ~ price_vec, data = elasticity_df)$coefficients[[2]]
  alpha <- lm(demand_vec ~ price_vec, data = elasticity_df)$coefficients[[1]]
  alpha_list <- c(alpha_list, alpha)
  mean_price <- mean(price_vec)
  mean_demand <- mean(demand_vec)
  elasticity_vec[i] <- mean_price*slope/mean_demand
}

elasticity_values <- data.frame(matrix(unlist(elasticity_vec), nrow=length(elasticity_vec), byrow=T))
colnames(elasticity_values) <- "Elasticity"

#Add product_id to map the elasticity to the product. Then, use the product id to map it to the category

elasticity_values$prod_id <- products

elasticity_values <- left_join(elasticity_values,unique(all_data[,c("prod_id","category_id","category_desc_eng","sub_category_desc")]))

#We only want to promote elastic products
#Need to ensure we are working with 2 categories that have atleast 100 products combined that are elastic

elasticity_only_elastic <- elasticity_values[elasticity_values$Elasticity<-0.7 & !is.na(elasticity_values$Elasticity),]

#Calculate # of elastic products for each category

elastic_prods_per_category <- elasticity_only_elastic %>% 
  group_by(category_desc_eng) %>%
  summarise(Num_Elastic_Products = n())  #SELECT FINE WINE AND FINE WAFERS as our 2 categories

#We have more than 100 elastic products across these two categories, so let's just shortlist the 100 most elastic products

wines_wafers <- all_data[all_data$prod_id %in% elasticity_only_elastic$prod_id,"prod_id"]

#Elasticity of 100th product

num_prods <- 100 #Input can be changed as needed 

cutoff <- sort(wines_wafers$Elasticity)[num_prods] 

Target_Products <- wines_wafers$prod_id[wines_wafers$Elasticity<=cutoff]

wines_wafers <- wines_wafers[wines_wafers$prod_id %in% Target_Products,] #100 products selected

#Now that we have a list of our 100 target products, let's focus on getting their substitutes and complements

# Our target products based on elasticity
selected_products = as.list(wines_wafers$prod_id)

# Also store all products in the wines and wafers category
wines_wafers_products = as.list(unique(all_data[category_desc_eng %in% c("FINE WINES","FINE WAFERS")]$prod_id))

# 2. Filter transaction data to include only those transactions with purchase of at least a product in our target list (For complements)

# Remove Bags from transactions because a large portion of the transactions include
# the purchase of Bags, which may lead to a biased result for substitutes  even though they arent logical subs for wines/wafers
all_data = all_data[!all_data$category_desc_eng=="BAGS"]
all_data_transaction = all_data[, c('prod_id','new_transaction_id')]

# We first found transactions that contain our target products, then we used the unique transaction id (self-defined) to find all products purchased in the transaction.

all_data_transaction = all_data_transaction[all_data_transaction$prod_id %in% selected_products,]
#Transactions that contain the 100 target products
transaction_list = as.list(unique(all_data_transaction$new_transaction_id))

# Now we have a transaction dataset that is only relevant to our target products (contains all products bought with the 100 wines/wafers)
all_data_target = all_data[all_data$new_transaction_id %in% transaction_list,]
all_data_b = copy(all_data_target)

# 3. Find substitutes and complements for our product
# To find the association rule between the purchase of two products, we used the arules package and apriori
# to mine the rules. 

# 3.1 We first converted each of the transaction to a list of products purchased at the same time, seperated by commas in 1 column
transactionData = ddply(all_data_target,c("new_transaction_id"),
                        function(df1)paste(df1$prod_id,
                                           collapse = ","))

#Do not need the id's anymore - just the column with all products in a transaction
transactionData$new_transaction_id = NULL
colnames(transactionData) = c("items")

# Because the apriori only takes transaction data, we have to transform the dataframe to transaction using
# as(x, 'transactions') and seperate the products in a transaction in multiple columns
transactionData = data.frame(lapply(transactionData,as.factor))
items = strsplit(as.character(transactionData$items), ",")
tr = as(items,"transactions")

# In the for loop, the association rules will be mined on the product level. For each product, we found its
# most realted substitue and complement. Then we combine all the product-level data to create tables with substitues and
# complements for all products in our target list.

substitute = list()
complement = list()
for (i in c(1:length(selected_products))){
  # Generate the association rule co-purchases. Supp is set to (2/number of total transactions), which means
  # there should be at least two transactions both products appeared at the same time. Conf is set to 0 because
  # the transactions include the target product takes up only a small part of the total transactions, and we want
  # to catach as many transactions as possible. minlen and maxlen are both set to 2 to limit our output to the 
  # item and substitute/complement.
  rules = apriori(tr, parameter = list(supp=2/length(tr), conf=0,minlen=2, maxlen=2),appearance = list(lhs=selected_products[i], default="rhs"))
  # the output format was changed to dataframe
  rules_a = DATAFRAME(rules, separate = TRUE)
  # remove special symbol {} from the character with gsub
  rules_a$LHS = gsub("[^[:alnum:]]", "", rules_a$LHS)
  rules_a$RHS = gsub("[^[:alnum:]]", "", rules_a$RHS)
  # A lift value greater than 1 means that item Y is likely to be bought if item X is bought, 
  # while a value less than 1 means that item Y is unlikely to be bought if item X is bought.
  # substitute
  sub_list = rules_a[rules_a$lift<1,]
  # we want to aviod those products already in our target list and categories
  sub_list = sub_list[!(sub_list$RHS %in% wines_wafers_products), ]
  # to distinguish which substitute is the most signifiant, we prefer the ones with high confidence (precision) 
  # over ones with high support (recall).
  sub_list = sub_list[which.max(sub_list$confidence),][,c('LHS','RHS')]
  colnames(sub_list) = c("item","substitute")
  substitute[[i]] = sub_list
  # complement
  comp_list = rules_a[rules_a$lift>1,]
  # we want to find complement outside the two categories we selected
  comp_list = comp_list[!(comp_list$RHS %in% wines_wafers_products), ]
  comp_list = comp_list[which.max(comp_list$confidence),][,c('LHS','RHS')]
  colnames(comp_list) = c("item","complement")
  complement[[i]] = comp_list
}

all_substitute = do.call(rbind, substitute)
all_complement = do.call(rbind, complement)

# Final step to check how many substitues/complements fall in the target list/category list - Should be 0
sum(all_substitute$substitute %in% selected_products)
sum(all_complement$complement %in% wines_wafers_products)


# 4. Average weekly price
# Get a list of all products including our target products and their substitutes and complements
substitute_list = as.list(all_substitute$substitute)
complement_list = as.list(all_complement$complement)
affinity_list = combine(substitute_list,complement_list)
all_list = combine(affinity_list, wines_wafers_products)

#For now, only keep stores that sold wines and wafers in our target list in the past - will update this variable later after finding most viable stores for this promotion
Target_Stores = unique(all_data_target$store_id) #418 stores

# FINAL TABLE A would be the table that has weekly level sales info (for model) for our 100 target products across the historical data
# Select transactions made within our target stores and related to the products in all_list

selected_transactions = all_data_b[all_data_b$prod_id %in% all_list & all_data_b$store_id %in% Target_Stores]
selected_transactions_a = selected_transactions[, c('prod_id','date','category_desc_eng','tran_prod_sale_amt','tran_prod_sale_qty','tran_prod_discount_amt',
                                                    'tran_prod_paid_amt', 'prod_unit_price')]
selected_transactions_a = data.table(selected_transactions_a)

#Account for seasonality by adding variables for Month (11 dummy variables as factors) and Holidays in 2016 and 2017

#Load holiday days as dates
holiday_weeks = c('2016-01-01', '2016-03-25', '2016-04-25', '2016-05-26', '2016-06-10', '2016-08-15', '2016-10-05', '2016-11-01', '2016-12-01', '2016-12-08', '2016-12-25', '2017-01-01', '2017-04-14', '2017-04-16', '2017-04-25', '2017-05-01', '2017-06-10', '2017-06-15', '2017-08-15', '2017-10-05', '2017-11-01', '2017-12-01', '2017-12-08', '2017-12-25')
holiday_weeks <- as.data.frame(holiday_weeks)
colnames(holiday_weeks) <- "Date"

selected_transactions_a[,is_holiday_week:= ifelse(date %in% holiday_weeks$Date,1,0)]

# Change date to year, month and week number 

#Use weekly sales since our offer is for 1 whole week, while use month for seasonality

selected_transactions_a[, year:= strftime(date, format = "%Y")]
selected_transactions_a[, week:= strftime(date, format = "%V")]

#Use month to factor because it'll be used in the model as a control for seasonality

selected_transactions_a[, month:= factor(strftime(date, format = "%m"))]

#Often, holiday transactions are not performed on the holiday itself, but a few days before or after. To account for this, mark the whole transaction week as 1 if that week had a holiday
selected_transactions_a[,is_holiday_week:= max(is_holiday_week),by=c("year","week")]

#Check how many products were sold in holiday weeks in the data
sum(selected_transactions_a$is_holiday_week)

#Calculate average price and units sold at weekly level for each product in each week

selected_transactions_a[, week_revenue:= sum(tran_prod_paid_amt), by=c('category_desc_eng','prod_id','year','week')]
selected_transactions_a[, week_unit:= sum(tran_prod_sale_qty), by=c('category_desc_eng','prod_id','year','week')]
selected_transactions_a[, avg_unit_price:= mean(prod_unit_price), by=c('category_desc_eng','prod_id','year','week')] #Goes into model
selected_transactions_a[, week_paid_amount:= sum(tran_prod_paid_amt), by=c('category_desc_eng','prod_id','year','week')]
#Average discounted price per product across stores
selected_transactions_a[, weekly_list_price:= week_revenue/week_unit, by=c('category_desc_eng','prod_id','year','week')] 
selected_transactions_a[, weekly_discounted_price:= week_paid_amount/week_unit, by=c('category_desc_eng','prod_id','year','week')]

selected_transactions_a = unique(selected_transactions_a[, c('category_desc_eng','prod_id','year','month','week', 'week_unit','avg_unit_price','weekly_list_price','weekly_discounted_price','is_holiday_week')])

# Merge tables to include weekly average price and unit sold for product, substitute, and complement
product_list = merge(all_substitute, all_complement)
product_list <- mutate_all(product_list, function(x) as.numeric(as.character(x)))
temp1 = merge(selected_transactions_a, product_list, by.x="prod_id", by.y="item", fill=TRUE)
setnames(temp1, old = c('category_desc_eng','week_unit','weekly_list_price','weekly_discounted_price','avg_unit_price'), new = c('category_product','week_unit_product','weekly_list_price_product','weekly_discounted_price_product','avg_unit_price_product'))
temp2 = copy(selected_transactions_a)
setnames(temp2, old = c('prod_id'), new = c('substitute'))
temp3 = merge(temp1, temp2, by = c('year','week','substitute'), fill=TRUE)
setnames(temp3, old = c('category_desc_eng','week_unit','weekly_list_price','weekly_discounted_price','avg_unit_price'), new = c('category_substitute','week_unit_substitute','weekly_list_price_substitute','weekly_discounted_price_substitute','avg_unit_price_substitute'))
temp4 = copy(selected_transactions_a)
setnames(temp4, old = c('prod_id'), new = c('complement'))
temp5 = merge(temp3, temp4, by = c('year','week','complement'), fill=TRUE)
setnames(temp5, old = c('category_desc_eng','week_unit','weekly_list_price','weekly_discounted_price','avg_unit_price'), new = c('category_complement','week_unit_complement','weekly_list_price_complement','weekly_discounted_price_complement','avg_unit_price_complement'))
final_table_a = temp5


# Use code below to make subsets - Seperate out the 2 categories
wine_data = final_table_a[category_product == "FINE WINES"]
wafer_data = final_table_a[category_product == "FINE WAFERS"]

#Wine data model to predict demand

wine_model <- lm(week_unit_product ~ factor(month)+ is_holiday_week + avg_unit_price_product + avg_unit_price_substitute + 
                   avg_unit_price_complement +weekly_discounted_price_product + weekly_discounted_price_substitute + 
                   weekly_discounted_price_complement, data=wine_data )

wafers_model <- lm(week_unit_product ~ factor(month)+ is_holiday_week + avg_unit_price_product + avg_unit_price_substitute + avg_unit_price_complement +weekly_discounted_price_product + weekly_discounted_price_substitute + weekly_discounted_price_complement, data=wafer_data )

# FINAL TABLE B - Predict demand for various prices in the week of April 13 2020 using 2017's discount structure 
# Do this for each product at each store weekly 
selected_transactions_b = selected_transactions[, c('prod_id','date', 'store_id','category_desc_eng','tran_prod_sale_amt','tran_prod_sale_qty','tran_prod_discount_amt',
                                                    'tran_prod_paid_amt', 'prod_unit_price')]

selected_transactions_b = data.table(selected_transactions_b)
# Change date to year and week number to calculate average price and unit sold on week level

selected_transactions_b[, year:= strftime(date, format = "%Y")]
selected_transactions_b[, week:= strftime(date, format = "%V")]
selected_transactions_b[, month:= factor(strftime(date, format = "%m"))]

selected_transactions_b[, avg_unit_price:= mean(prod_unit_price), by=c('category_desc_eng','store_id','prod_id','year','week')]
selected_transactions_b[, week_unit:= sum(tran_prod_sale_qty), by=c('category_desc_eng','store_id','prod_id','year','week')]
selected_transactions_b[, week_paid_amount:= sum(tran_prod_paid_amt), by=c('category_desc_eng','store_id','prod_id','year','week')]
selected_transactions_b[, weekly_discounted_price:= week_paid_amount/week_unit, by=c('category_desc_eng','store_id','prod_id','year','week')]

#Calculate historical minimum and maximum for that store for that product and test all prices between the two for optimization
selected_transactions_b[, min_list_price_store:= min(prod_unit_price), by=c('prod_id','store_id')]
selected_transactions_b[, max_list_price_store:= max(prod_unit_price), by=c('prod_id','store_id')]

#Keep the relevant columns and keep 1 transaction per week per store per product
selected_transactions_b = unique(selected_transactions_b[, c('category_desc_eng', 'store_id', 'prod_id','year','week','month','avg_unit_price','weekly_discounted_price','week_unit','min_list_price_store','max_list_price_store')])
selected_transactions_c = selected_transactions_b[, c('store_id', 'prod_id','year','week','month','weekly_discounted_price','avg_unit_price','week_unit')]

# Merge tables to include weekly average price and unit sold for product, substitute, and complement
temp1 = merge(selected_transactions_b, product_list, by.x="prod_id", by.y="item", fill=TRUE)
setnames(temp1, old = c('category_desc_eng','weekly_discounted_price','avg_unit_price'), new = c('category_product','weekly_discounted_price_product','avg_unit_price_product'))
temp2 = copy(selected_transactions_c)
setnames(temp2, old = c('prod_id'), new = c('substitute'))
temp3 = merge(temp1, temp2, by = c('year','week', 'store_id', 'substitute'), fill=TRUE)
setnames(temp3, old = c('weekly_discounted_price','avg_unit_price'), new = c('weekly_discounted_price_substitute','avg_unit_price_substitute'))
temp4 = copy(selected_transactions_c)
setnames(temp4, old = c('prod_id'), new = c('complement'))
temp5 = merge(temp3, temp4, by = c('year','week', 'store_id','complement'), fill=TRUE)
setnames(temp5, old = c('weekly_discounted_price','avg_unit_price'), new = c('weekly_discounted_price_complement','avg_unit_price_complement'))
final_table_b = temp5

#Now, keep observations only for week of April 13-19, 2017 (Week)
target_week <- week("2017-04-13")
Year <- 2017

## STORE SELECTION
#How much sales did each store drive for these 100 products?

store_selection_sales <- all_data[all_data$prod_id %in% selected_products,] %>%
  group_by(store_id) %>%
  summarize(prod_sales = sum(tran_prod_sale_amt)) %>%
  arrange(desc(prod_sales))

#How many of these 100 products did each store sell in our target week?

#First get transaction data for the target week

target_week_data <- final_table_b[final_table_b$week==target_week & year==Year,]

store_selection_count <- target_week_data %>%
  group_by(store_id) %>%
  summarize(num_prods = length(unique(prod_id))) %>%
  arrange(desc(num_prods))

#What is the overlap between top 10 of stores based on sales and num_products being sold in target week?
num_stores <- 10
sum(store_selection_sales$store_id[1:num_stores] %in% store_selection_count$store_id[1:num_stores])  #High overlap, 7 of 10 are common and all 10 are present in the top 17

#Select top 10 stores with highest sales of these 10 products - Verified to have highest incremental revenue

target_stores <- store_selection_sales$store_id[1:num_stores]

target_week_data <- target_week_data[target_week_data$store_id %in% target_stores,]

target_week_data$is_holiday_week <- 0  #No holiday this week (was in preceding week as described in project)

target_week_data <- target_week_data %>%
  tidyr::pivot_longer(c("min_list_price_store","max_list_price_store"),names_to="Price_Type",values_to="Price_point")


## Now, Predict on the various price points and see which price point gets highest demand (ensuring overall profitability)
#For each product, predict demands at various price points and optimize the price so that the demand (quantity sold) is highest at each store and product
#Modify the dataframe to add rows at multiple price points between the min and max historical prices (min and max historical price of the product in that store)

target_products <- unique(target_week_data$prod_id)
price_vec <- data.frame()
temp_df <- data.frame()

#First, Get sequence between minimum and maximum product unit prices to optimize price points in increments of 0.05
for(i in seq_along(target_products))
{ temp_df <- target_week_data[target_week_data$prod_id==target_products[i],]
target_stores <- unique(temp_df$store_id) 
for(j in seq_along(target_stores))
{ min_price = min(temp_df[temp_df$store_id == target_stores[j],"Price_point"])
max_price = max(temp_df[temp_df$store_id == target_stores[j],"Price_point"])
price_vec_temp = data.frame(prod_id=target_products[i] ,store_id = target_stores[j],price_points=seq(min_price,max_price,by=0.05))
price_vec = rbind(price_vec_temp,price_vec)
}
}

target_week_data <- target_week_data[,!names(target_week_data) %in% c("Price_Type","Price_point")]

#Join with model data for predicting demand
model_data <- left_join(price_vec,target_week_data)

#Get model data seperately for categories
wine_model_data <- model_data[model_data$category_product=="FINE WINES",]
wafers_model_data <- model_data[model_data$category_product=="FINE WAFERS",]

wine_model_data$predicted_demand <- predict(wine_model,wine_model_data)

#Remove negative demand predictions
wine_model_data <- wine_model_data[wine_model_data$predicted_demand>0,]

#For each product in each store, what is the highest predicted demand?
wine_model_data <- wine_model_data %>%
  group_by(prod_id,store_id) %>%
  mutate(max_demand = max(predicted_demand))

#What price brings this highest demand? In this case, demand=max demand - keep that row and remove the rest
wine_model_optimized <- wine_model_data[wine_model_data$predicted_demand==wine_model_data$max_demand,]

wine_final <- distinct_at(wine_model_optimized,.vars=c("store_id","prod_id"),.keep_all = T)
wine_final <- wine_final[,c("prod_id","store_id","price_points","avg_unit_price_product","max_demand","week_unit","weekly_discounted_price_product")]
wine_final$max_demand <- floor(wine_final$max_demand) #Quantity would be an integer

#Do the same for wafers

wafers_model_data$predicted_demand <- predict(wafers_model,wafers_model_data)

#Remove negative demand predictions
wafers_model_data <- wafers_model_data[wafers_model_data$predicted_demand>0,]

#For each product in each store, what is the highest predicted demand?
wafers_model_data <- wafers_model_data %>%
  group_by(prod_id,store_id) %>%
  mutate(max_demand = max(predicted_demand))

#What price brings this highest demand? In this case, demand=max demand - keep that row and remove the rest
wafers_model_optimized <- wafers_model_data[wafers_model_data$predicted_demand==wafers_model_data$max_demand,]

wafers_final <- distinct_at(wafers_model_optimized,.vars=c("store_id","prod_id"),.keep_all = T)
wafers_final <- wafers_final[,c("prod_id","store_id","price_points","avg_unit_price_product","max_demand","week_unit","weekly_discounted_price_product")]
wafers_final$max_demand <- floor(wafers_final$max_demand) #Quantity would be an integer

#Final Results

promotion_plan <- rbind(wine_final,wafers_final)

#EXPORT FINAL RESULTS

fwrite(promotion_plan,"Product_List_Optimized.csv")

#CSV contains all necessary columns for calculating Incremental Revenue/Profit and are performed in Excel and submitted as Product_List_Optimized.csv

