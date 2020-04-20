library(data.table)
library(dplyr)
library(stringr)
library(tidyr)
library(cluster)
library(factoextra)
library(ggplot2)
library(qdapTools)
library(data.table)
#########################################################################################################
# Read Table
merged_data = fread("/Users/patrickhe/Desktop/MSBA/OneDrive - Emory University/Marketing Analysis/Projects/Pernalonga/transaction_data1.csv")
products = fread("/Users/patrickhe/Desktop/MSBA/OneDrive - Emory University/Marketing Analysis/Projects/Pernalonga/product_table.csv")

# Brands Existing in the Market
brand_name = unique(products[category_desc_eng == "TOOTHPASTE"][,brand_desc])

# Sub-classes for the toothpaste category
sub_class = unique(products[category_desc_eng == "TOOTHPASTE"][,sub_category_desc])

#COLGATE : 34 products 4 sub categories
COLGATE_product_id = unique(products[category_desc_eng == "TOOTHPASTE"& brand_desc == "COLGATE"][,sub_category_desc])
COLGATE_product_id = unique(products[category_desc_eng == "TOOTHPASTE"& brand_desc == "COLGATE"][,prod_id])

#########################################################################################################
# Exploration for the sub-categories under ToothPaste
# Import cleaned dataset from dataprep file
toothpaste = fread("/Users/patrickhe/Desktop/MSBA/OneDrive - Emory University/Marketing Analysis/Projects/Pernalonga/no_private_label.csv")

# Number of customers in the toothpaste market: 7681
length(unique(toothpaste[,cust_id]))

# Create year column
toothpaste[, year := format(as.Date(tran_dt, format="%Y-%m-%d"),"%Y")]
toothpaste[, total_sale_year := sum(tran_prod_paid_amt), by = c('brand_desc','year')]

# Number of customers for each sub-category
toothpaste[, n_customer_sub := length(unique(cust_id)), by = c('subcategory_id', 'brand_desc','year')]
# Number of transactions for each sub-category
toothpaste[, n_transactions_sub := .N, by = c('subcategory_id', 'brand_desc','year')]
# Number of sale quantities for each sub-category
toothpaste[, n_sale_quantities_sub := sum(tran_prod_sale_qty), by = c('subcategory_id', 'brand_desc','year')]
# Average discount rate for each sub-category
toothpaste[, avg_discount_sub := 1 - sum(tran_prod_paid_amt)/sum(tran_prod_sale_amt), by = c('subcategory_id', 'brand_desc','year')]
# Average unit price for each sub-category
toothpaste[, total_sale_sub := sum(tran_prod_sale_amt), by = c('subcategory_id', 'brand_desc','year')]
toothpaste[, total_paid_sub := sum(tran_prod_paid_amt), by = c('subcategory_id', 'brand_desc','year')]
toothpaste[, avg_unit_price_sub := total_paid_sub/n_sale_quantities_sub, by = c('subcategory_id', 'brand_desc','year')]
toothpaste[, max_sale_price := max(prod_unit_price), by = c('subcategory_id', 'brand_desc','year')]
toothpaste[, avg_sale_price_sub := total_sale_sub/n_sale_quantities_sub, by = c('subcategory_id', 'brand_desc','year')]

# Summary of sales statistics for each brand in each sub-category
a = unique(toothpaste[, c('sub_category_desc','subcategory_id', 'n_customer_sub', 'brand_desc',
                          'n_transactions_sub', 'n_sale_quantities_sub', 'avg_discount_sub',
                          'total_sale_sub','total_paid_sub','avg_unit_price_sub','max_sale_price','avg_sale_price_sub','year')])
newdata <- a[order(-subcategory_id,year, -total_sale_sub, year),]

aaaa = a[brand_desc == 'COLGATE'][year == 2017][, c('brand_desc','sub_category_desc','max_sale_price','avg_sale_price_sub','avg_unit_price_sub')]



# Loyal Customers of COLGATE
toothpaste[, a := ifelse(brand_desc == "COLGATE", 1, 0)]
toothpaste[, aa := sum(a), by = cust_id]
toothpaste[, aaa := aa/.N, by = cust_id]
toothpaste[, COLGATE_Loyal := length(unique(toothpaste[aaa>=0.8][,cust_id]))]

#########################################################################################################
# Brand Level stat 
toothpaste[, n_customer_brand := length(unique(cust_id)), by = c('brand_desc','year')]
toothpaste[, n_transactions_brand := .N, by = c('brand_desc','year')]
toothpaste[, n_sale_quantities_brand := sum(tran_prod_sale_qty), by = c('brand_desc','year')]
toothpaste[, avg_discount_brand := 1 - sum(tran_prod_paid_amt)/sum(tran_prod_sale_amt), by = c('brand_desc','year')]

toothpaste[, total_sale_brand := sum(tran_prod_sale_amt), by = c('brand_desc','year')]
toothpaste[, total_paid_brand := sum(tran_prod_paid_amt), by = c('brand_desc','year')]
toothpaste[, avg_unit_price_brand := total_paid_brand/n_sale_quantities_brand, by = c('brand_desc','year')]
toothpaste[, avg_sale_price_brand := total_sale_brand/n_sale_quantities_brand, by = c('brand_desc','year')]

# Brand Market Share
brand_stat = unique(toothpaste[,c('brand_desc','n_customer_brand','n_transactions_brand','n_sale_quantities_brand',
                                    'avg_discount_brand', 'avg_unit_price_brand','avg_sale_price_brand','total_sale_brand','total_paid_brand','year')])
brand_stat <- brand_stat[order(year,-total_sale_brand),]

# Plot
g <- ggplot(brand_stat, aes(reorder(brand_desc, total_sale_brand), total_sale_brand))
g + geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  labs(title="Total Sales $", 
       subtitle="Toothpaste sales by brand") +
  xlab("Brand") +
  ylab("Sales") +
  theme(axis.text.x = element_text(vjust=0.6)) +
  coord_flip()

# Sale growth

sale_by_yr = unique(toothpaste[,c('brand_desc','year','total_sale_year')])
sale_by_yr[, total_sale_all := sum(total_sale_year), by = c('year')]
sale_by_yr[, market_share := total_sale_year/total_sale_all]
sale_by_yr <- sale_by_yr[order(year, -total_sale_year),][,c('brand_desc','year','total_sale_year','market_share')]
sale_by_yr$market_share = round(sale_by_yr$market_share, 4)


# Customer behavior changes in 2017
toothpaste[, n_transactions_cust := .N, by = c('cust_id','year')]
toothpaste[, n_sale_quantities_cust := sum(tran_prod_sale_qty), by = c('cust_id','year')]
toothpaste[, avg_discount_cust := 1 - sum(tran_prod_paid_amt)/sum(tran_prod_sale_amt), by = c('cust_id','year')]

toothpaste[, total_sale_cust := sum(tran_prod_sale_amt), by = c('cust_id','year')]
toothpaste[, total_paid_cust := sum(tran_prod_paid_amt), by = c('cust_id','year')]
toothpaste[, avg_unit_price_cust := total_paid_cust/n_sale_quantities_cust, by = c('cust_id','year')]
toothpaste[, avg_sale_price_cust := total_sale_cust/n_sale_quantities_cust, by = c('cust_id','year')]

cust_stat = unique(toothpaste[,c('cust_id','n_transactions_cust','n_sale_quantities_cust',
        'avg_discount_cust', 'avg_unit_price_cust','avg_sale_price_cust','total_sale_cust','total_paid_cust','year')])
cust_stat <- cust_stat[order(cust_id, year, n_sale_quantities_cust),]

# There's 700 incremental customers need to be removed from the calcualation of price elasticity on individual level
cust_stat[, n_yr := .N, by = 'cust_id']
count(cust_stat[n_yr == 1])
count(cust_stat[n_yr == 2])
cust_stat = cust_stat[n_yr == 2]

cust_stat_2016 = cust_stat[year == 2016]
cust_stat_2017 = cust_stat[year == 2017]
cust_stat_merge = merge(cust_stat_2016,cust_stat_2017, by='cust_id')

cust_stat_merge[, per_change_quantity := (n_sale_quantities_cust.y - n_sale_quantities_cust.x)/n_sale_quantities_cust.x  , by = 'cust_id']
cust_stat_merge[, per_change_price := (avg_unit_price_cust.y - avg_unit_price_cust.x)/avg_unit_price_cust.x  , by = 'cust_id']
cust_stat_merge[, price_elasticity :=  per_change_quantity/per_change_price , by = 'cust_id']
l = cust_stat_merge[, c('cust_id', "n_sale_quantities_cust.x", "n_sale_quantities_cust.y", "avg_unit_price_cust.x", "avg_unit_price_cust.y","price_elasticity" )]
ll = l[n_sale_quantities_cust.y > n_sale_quantities_cust.x & price_elasticity > 1 |  price_elasticity < 1  ]

aaa = a[,c('sub_category_desc', 'total_paid_sub', 'brand_desc', 'year')][year ==2017]
aaa <- aaa[order(sub_category_desc, -total_paid_sub),]

