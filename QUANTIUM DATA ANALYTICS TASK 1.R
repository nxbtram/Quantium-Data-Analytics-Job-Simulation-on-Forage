# ===========================================
# Quantium Task 1 - Chips Analysis Project
# ===========================================


# ==============================
# 1. Load Required Libraries ----
# ==============================

install.packages("dplyr")
install.packages("ggplot2")
install.packages("readxl")
install.packages("lubridate")
install.packages("scales")
install.packages("tidyr")
install.packages("stringr")
install.packages("RColorBrewer")
install.packages("ggmosaic")
install.packages("gridExtra")

library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(tidyr)
library(scales)
library(stringr)
library(RColorBrewer)
library(ggmosaic)
library(gridExtra)


# ==============================
# 2. Load and Prepare Data ----
# ==============================

setwd("/Users/baogon.39/Desktop/FORAGE/QUANTIUM DATA ANALYTICS")

transaction <- read_xlsx("QVI_transaction_data.xlsx")
purchase_beha <- read_xlsx("QVI_purchase_behaviour.xlsx")

# Merge datasets by loyalty card number
DT <- left_join(purchase_beha, transaction, by = "LYLTY_CARD_NBR")

# convert Excel-style numeric dates into R Date format
DT$DATE <- as.Date(DT$DATE, origin = "1899-12-30") 


# ==============================
# 3. Clean Product Data ----
# ==============================

# Clear non chips products
unique(DT$PROD_NAME)
DT <- DT %>%
  filter(!grepl("salsa", tolower(PROD_NAME))) 

# Clear commercial customer 
View(DT) # Customer ID 226000 have order quantity 200 units => commercial customer 
DT <- DT %>% filter(LYLTY_CARD_NBR != 226000)
DT %>% count(LYLTY_CARD_NBR == 226000)


# ==============================
# 4. Check for Missing Dates ----
# ==============================

all_dates <- seq(as.Date("2018-07-01"), as.Date("2019-06-30"), by = "day")
data_dates <- unique(DT$DATE)
missing_dates <- setdiff(all_dates, data_dates)
as.Date(missing_dates, origin = "1970-01-01") # Expected: "2018-12-25"


# ==============================
# 5. Transactions Over Time ----
# ==============================

# Number of transactions per day
transactions_by_day <- DT %>%
  group_by(DATE) %>%
  summarise(transaction_count = n()) %>%
  arrange(DATE)

p1<-ggplot(transactions_by_day, aes(x = DATE, y = transaction_count)) +
  geom_line(color = "#1B9E77", size = 0.3) +
  scale_x_date(
    date_breaks = "1 month", 
    date_labels = "%b\n%Y"
  ) +
  labs(
    title = "  Number of Transactions on daily",
    subtitle = "From July 2018 to June 2019",
    x = "",
    y = "Transactions"
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#2E4053", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "#566573", hjust = 0.5),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) 

# Zoom-in: December 2018

# December zoom-in
# December data set
december_data <- transactions_by_day %>%
  filter(DATE >= as.Date("2018-12-01") & DATE <= as.Date("2018-12-31"))
# Line chart
p2<-ggplot(december_data, aes(x = DATE, y = transaction_count)) +
  geom_line(color = "#1B9E77") +
  labs(
    title = "Transactions in December 2018",
    x = "", y = "Transactions") +
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#2E4053", hjust = 0.5),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

grid.arrange(p1, p2)

# ==============================
# 6. Feature Engineering ----
# ==============================

# Extract pack size (in grams) from product name
DT <- DT %>% mutate(PACK_SIZE = parse_number(PROD_NAME))  #pick the first number on product name to have gram for PACK SIZE

# Extract brand name (first word)
DT <- DT %>% mutate(BRAND = word(PROD_NAME, 1))#pick the first word on product name to have gram for BRAND NAME

#Recoding brand names
DT <- DT %>%
  mutate(BRAND = case_when(
    BRAND == "RRD" ~ "Red",
    BRAND == "Snbts"~"Sunbites" ,
    BRAND == "GrnWves"~"Grain" ,
    BRAND == "Smith" ~ "Smiths",
    BRAND == "Dorito" ~ "Doritos",
    BRAND == "Infzns"~ "Infuzions" ,
    BRAND == "NCC"~ "Natural",
    BRAND == "WW"~ "Woolworths",
    TRUE ~ BRAND
  ))
unique(DT$BRAND)


# ==============================
# 7. Top Brands & Pack Sizes ----
# ==============================

# BRAND data 
brand <- DT %>%count(BRAND, sort = TRUE)

# Top Brands 
p3 <- ggplot(brand, aes(x = reorder(BRAND, -n), y = n)) +
  geom_bar(stat = "identity", fill = "#66C2A5", width = 0.7) +  
  labs(
    title = "Top Brands ",
    subtitle = "Based on Number of Purchases",
    x = "Brand",
    y = "Number of Purchases"
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#2E4053"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#566573"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Preferred Pack Sizes
packsize <- DT %>% count(PACK_SIZE, sort = TRUE)
# PACK SIZE data Mainstream Single/couple group
p4 <-ggplot(packsize, aes(x = factor(PACK_SIZE), y = n)) +
  geom_bar(stat = "identity", fill = "#66C2A5", width = 0.7) +
  labs(
    title = "Preferred Pack Sizes",
    subtitle = "Based on Purchase Frequency",
    x = "Pack Size (g)",
    y = "Number of Purchases"
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#2E4053"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#566573"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
grid.arrange(p3,p4)


# ==============================
# 8. Total Sales by Segment ----
# ==============================

# Data Sales by Segment
sale_by_segment <- DT %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(total_sales = sum(TOT_SALES), .groups = "drop")

# Bar chart total Sales by Customer 

pa <- ggplot(sale_by_segment) +
  geom_mosaic(aes(weight = total_sales, x = product(PREMIUM_CUSTOMER, LIFESTAGE),fill = PREMIUM_CUSTOMER)) +
  labs(x = "Lifestage", y = "Premium customer flag", title = "Proportion of sales") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(
    title = "Total Sales by Customer Segment",
    subtitle = "Segmented by Lifestage and Premium Status",
    x = "Customer Lifestage",
    y = "Total Sales ($)",
    fill = ""
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#2E4053", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "#566573", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 5, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 5),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  scale_fill_brewer(palette = "Set2")


#### Plot and label with proportion of sales
p5 <- pa + geom_text(data = ggplot_build(pa)$data[[1]], aes(x = (xmin + xmax)/2 , y = (ymin + ymax)/2, label = as.character(paste(round(.wt/sum(.wt),3)*100,'%'))))


# ==============================
# 9. Number of Customers by Segment ----
# ==============================

customer_count_by_group <- purchase_beha %>% count(LIFESTAGE, PREMIUM_CUSTOMER)

#Plot Number of Customers by Segment 
pb <- ggplot(customer_count_by_group) +
  geom_mosaic(aes(weight = n , x = product(PREMIUM_CUSTOMER, LIFESTAGE),fill = PREMIUM_CUSTOMER)) +
  labs(
    title = "Customer Count by Segment",
    subtitle = "Grouped by Lifestage and Premium Status",
    x = "Customer Lifestage",
    y = "Number of Customers",
    fill = ""
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#2E4053", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "#566573", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 5, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 5),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank() 
  ) +
  scale_fill_brewer(palette = "Set2")
#### Plot and label with  of sales
p6 <- pb + geom_text(data = ggplot_build(pb)$data[[1]], aes(x = (xmin + xmax)/2 , y = (ymin + ymax)/2, label = as.character(paste(round(.wt/sum(.wt),3)*100,'%'))))

grid.arrange(p5, p6)


# ==============================
# 10. Average Packs per Customer ----
# ==============================

# Total quantity of customer
total_qty_per_customer <- DT %>%
  group_by(LYLTY_CARD_NBR, LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(total_qty = sum(PROD_QTY), .groups = "drop")

# Total quantity of customer
avg_qty_per_customer <- total_qty_per_customer %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(avg_qty = mean(total_qty), .groups = "drop")
average_pack <- DT %>%
  summarise(meanpack = sum(PROD_QTY) / n_distinct(LYLTY_CARD_NBR))

skim(DT)
# Bar chart Average Packs per Customer
p7<-ggplot(avg_qty_per_customer, aes(x = LIFESTAGE, y = avg_qty, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = round(avg_qty, 1)),
            position = position_dodge(width = 0.7),
            vjust = -0.5,
            size = 2) +
  scale_fill_brewer(palette = "Set2") +
  coord_cartesian(ylim = c(2.5, 12)) +
  labs(
    title = "Average Packs per Customer",
    subtitle = "Segmented by Lifestage and Premium Status",
    x = "Customer Lifestage",
    y = "Average Quantity",
    fill = ""
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#2E4053", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "#566573", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 5, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 5),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# ==============================
# 11. Average Price per Unit ----
# ==============================
# Unit price per transactions
DT_with_unit_price <- DT %>%
  mutate(unit_price = TOT_SALES / PROD_QTY)

# Unit price per segmet
avg_price_per_unit <- DT_with_unit_price %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(avg_price = mean(unit_price), .groups = "drop")

average_price <- DT %>%
  summarise(meanpack = sum(TOT_SALES) / sum(PROD_QTY))

#Average Price per Unit by Segment
p8 <-ggplot(avg_price_per_unit, aes(x = LIFESTAGE, y = avg_price, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_brewer(palette = "Set2") +
  geom_text(aes(label = round(avg_price, 1)),
            position = position_dodge(width = 0.7),
            vjust = -0.5,
            size = 2) +
  labs(
    title = "Average Price per unit ",
    subtitle = "By Lifestage and Premium Status",
    x = "Customer Lifestage",
    y = "Average Price ($)",
    fill = ""
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#2E4053", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "#566573", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 5, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 5),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  coord_cartesian(ylim = c(3.5, 4.25))

grid.arrange(p7, p8)


# ==============================
# 12. T-test: Mainstream vs Others in Young Singles/Couples ----
# ==============================

# Data Single/couple group by  premium customer
filtered_data <- DT %>%
  filter(LIFESTAGE == "YOUNG SINGLES/COUPLES") %>%
  mutate(unit_price = TOT_SALES / PROD_QTY,GROUP = if_else(PREMIUM_CUSTOMER == "Mainstream", "Mainstream", "Others"))

t_test_result <- t.test(unit_price ~ GROUP, data = filtered_data)
print(t_test_result)
# Mainstream customers in Single/couple group pay on average $4.06 per pack, compared to $3,66 for others
# They spend ~$0.38–$0.42 more per pack (95% CI)
# p-value < 2.2e-16 → significant difference in mean unit price


# ==============================
# 13. Deep Insight on Mainstream Young Singles/Couples ----
# ==============================

DT <- DT %>% mutate(GROUP = if_else(LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream","target", "other"))
# BRAND ANALYSIS
# total quantity by group and brand table
table_brand <- DT %>%
  group_by(BRAND, GROUP) %>%
  summarise(total = sum(PROD_QTY), .groups = "drop") %>%
  pivot_wider(names_from = GROUP, values_from = total, values_fill = 0)

sum(table_brand$other) #434174 
sum(table_brand$target)#36225 

# Propotion table
prop_brand <- table_brand %>% 
  mutate(other = other/434174,target = target/36225)

#  affinity
affinity_brand <- prop_brand %>% 
  mutate(affinity= target/other) %>%
  arrange(-affinity)
affinity_brand <- affinity_brand %>% mutate (difference= affinity-1)
view (affinity_brand)

# Mainstream young singles/couples are 22,8% more likely to purchase Tyrrells chips compare to other group

# Mainstream young singles/couples are 55,6% less likely to purchase Burger chips compare to other group



# PACK SIZE ANALYSIS
# total quantity by group and brand table
table_pack<- DT %>%
  group_by(PACK_SIZE, GROUP) %>%
  summarise(total = sum(PROD_QTY), .groups = "drop") %>%
  pivot_wider(names_from = GROUP, values_from = total, values_fill = 0)

sum(table_pack$other) #434174 
sum(table_pack$target)#36225 

# Propotion table
prop_pack <- table_pack %>% 
  mutate(other = other/434174,target = target/36225)

#  affinity
affinity_pack <- prop_pack %>% 
  mutate(affinity= target/other) %>%
  arrange(-affinity)
affinity_pack <- affinity_pack %>% mutate (difference= affinity-1)
view (affinity_pack)

brand_270g <- DT %>%
  filter(PACK_SIZE == "270") %>%
  distinct(PROD_NAME)
view (brand_270g)
brand_380g <- DT %>%
  filter(PACK_SIZE == "380") %>%
  distinct(PROD_NAME)
view (brand_380g)


# Mainstream young singles/couples are 26,7% more likely to purchase 270gram chips of Twisties Cheese and Twisties Chicken compare to other group
# ===============================

# Final combined plot
grid.arrange(p1, p2, p3, p4)
grid.arrange(p5, p6)
grid.arrange(p7, p8)
