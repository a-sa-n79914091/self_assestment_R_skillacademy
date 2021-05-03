library(dplyr)
library(ggplot2)


# Menambil data lewat csv
superstore_data <- read.csv('Assessment R & Python_ Dataset_superstore_simple.csv', stringsAsFactors = FALSE)

# Mencari data customer_id berdasar sales terbesar
customer_id_by_sales <- superstore_data %>%
  filter(sales == max(sales))%>%
  select(customer_id,sales)

# Mencari sub-category pada category office supplies dan jumlah total profitnya
total_profit_by_subcategory_office_supplies <- superstore_data %>% 
  group_by(category,sub_category) %>%
  filter(category == 'Office Supplies') %>%
  summarise(total_profit = sum(profit))

# Mencari jumlah order dengan profit negatif
order_with_negatif_profit <- superstore_data %>%
  filter(profit < 0)

total_count_order_with_negatif_profit <- count(order_with_negatif_profit)

# Mencari 3 customer dengan total sales terbanyak
customer_with_3_highest_sales <- superstore_data %>%
  group_by(customer_id) %>%
  filter(customer_id =='JE-16165' | customer_id == 'KH-16510' | customer_id == 'AD-10180') %>%
  summarise(total_sales = sum(sales)) %>%
  arrange(desc(total_sales))

# Membuat data frame baru berisi total profit tiap tahun, total sales , dan jumlah customer
superstore_data$order_date <- as.Date(paste(superstore_data$order_date,sep=""))

yearly_sales <- superstore_data %>%
  mutate(year = format(order_date, format="%Y")) %>%
  group_by(year) %>%
  summarise(total_customer = n_distinct(customer_id), total_sales = sum(sales))

# Membuat scatter plot antara sales dan profit pada tahun 2014 dan 2015
sales_profit_scatter_data <- superstore_data %>% 
  mutate(year = format(order_date, format="%Y")) %>%
  filter(year == 2014 | year == 2015) %>%
  select(year,sales,profit)

sales_profit_scatterplot <- ggplot(sales_profit_scatter_data, aes(x = sales, y = profit)) +
  geom_point(aes(color = year)) +
  geom_smooth(aes(color = year), method = lm, 
              se = FALSE, fullrange = TRUE) +
  labs(title = 'Sales vs Profit 2014-2015',
       subtitle = 'Based on Dataset Superstore',
       caption = 'R language tutorial',
       fill = 'Year') +
  xlab('Profit' ) +
  ylab('Sales') +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 16, face = 'bold'),
        axis.title.x = element_text(margin = margin(t = 10),size = 12),
        axis.title.y = element_text(margin = margin(r = 10),size = 12),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.title = element_text(size = 12),
        legend.title.align = 0.5,
        legend.text = element_text(size = 8))

# Membuat barchart untuk 10 customer profit dengan total sales tertinggi tahun 2015
higest10_2015_bar_data <- superstore_data %>%
  mutate(year = format(order_date, format="%Y")) %>%
  filter(year == 2015) %>%
  group_by (year,customer_id) %>%
  summarise(total_sales = sum(sales), total_profit = sum(profit)) %>%
  arrange(desc(total_sales)) %>%
  head(n = 10L) %>%
  arrange(desc(total_profit))

customer_profit_barchart <- ggplot(higest10_2015_bar_data, aes(x = reorder(customer_id, total_sales), y = total_profit, fill = total_sales)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = 'Profit Customer Terhadap 10 Penjualan Tertinggi',
       subtitle = 'Based on Dataset Superstore',
       caption = 'R language tutorial',
       fill = 'Sales') +
  xlab('Customer ID' ) +
  ylab('Profit') +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "grey"),
        plot.title = element_text(size = 16, face = 'bold'),
        axis.title.x = element_text(margin = margin(t = 10),size = 12),
        axis.title.y = element_text(margin = margin(r = 10),size = 12),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.title = element_text(size = 12),
        legend.title.align = 0.5,
        legend.text = element_text(size = 8)) +
  scale_fill_gradientn(colours = rainbow(5))
