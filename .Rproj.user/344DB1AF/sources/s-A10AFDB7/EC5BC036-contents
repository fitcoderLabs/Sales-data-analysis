library(tidyverse)
library(data.table)
library(ggplot2)
library(dplyr)

sales_rdf <- fread("data/sales_data.csv", 
                   colClasses = c("double","character","character","Date","Date","Date","Date","double"))

sales_rdf %>%
  group_by(prod_code, region, sale_month) %>% 
  mutate(n_units_month = sum(n_units)) %>% 
  ungroup() %>% 
  arrange(sale_month) %>%
  group_by(prod_code, region) %>% 
  mutate(percent_profit = (n_units_month-lag(n_units_month))/n_units_month*100,
         percent_profit = replace_na(percent_profit,0)) %>% 
  ungroup()-> sales_df

# column analysis--------------------------
colnames(sales_df)

#summary stats
summary(sales_df)

#distribution of columns-----------------

#prod code
n_distinct(sales_rdf$prod_code)
unique(sales_rdf$prod_code)

#region
n_distinct(sales_rdf$region)
unique(sales_rdf$region)

# number of units over time
sales_df %>% 
  group_by(sale_month) %>% 
  summarise(n_units = sum(n_units)) %>% 
  ungroup() %>% 
  ggplot(aes(sale_month,n_units))+
  geom_col(fill = "steelblue")+
  labs(x = "Sales Month",
       y = "# Units Sold",
       title = "Distribution of sales over time (Binned by month)")+
  theme_bw()

#profit over time
sales_df %>% 
  ggplot(aes(sale_month,percent_profit))+
  #facet_grid(region ~ prod_code)+
  geom_col(fill = "steelblue")+
  labs(x = "Sales Month",
       y = "Profit %",
       title = "Distribution of profit over time (Binned by month)")+
  theme_bw()

