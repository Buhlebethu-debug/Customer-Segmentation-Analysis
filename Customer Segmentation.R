
library(readxl)
install.packages("dplyr")   # only first time
library(dplyr)
install.packages("stringr")   # only once
library(stringr)
install.packages("ggplot2")   # only once
library(ggplot2)
install.packages("tidyverse")
install.packages("readxl")
install.packages("stringr")



transaction_data <- read_excel("QVI_transaction_data.xlsx")
purchase_behaviour <- read.csv("QVI_purchase_behaviour.csv")

str(transaction_data)
summary(transaction_data)

str(purchase_behaviour)
summary(purchase_behaviour)

head(transaction_data)
head(purchase_behaviour)

transaction_data$DATE <- as.Date(transaction_data$DATE, origin = "1899-12-30")
summary(transaction_data$DATE)

transaction_data <- transaction_data %>%
  filter(!grepl("salsa", PROD_NAME, ignore.case = TRUE))
  
transaction_data <- transaction_data %>%
  mutate(PACK_SIZE = as.numeric(str_extract(PROD_NAME, "[0-9]+")))
  
summary(transaction_data$PACK_SIZE)
table(transaction_data$PACK_SIZE)

transaction_data <- transaction_data %>%
  mutate(BRAND = word(PROD_NAME, 1))

transaction_data$BRAND <- toupper(transaction_data$BRAND)

transaction_data$BRAND[transaction_data$BRAND == "RED"] <- "RRD"
transaction_data$BRAND[transaction_data$BRAND == "SNBTS"] <- "SUNBITES"
transaction_data$BRAND[transaction_data$BRAND == "INFZNS"] <- "INFUZIONS"
transaction_data$BRAND[transaction_data$BRAND == "WW"] <- "WOOLWORTHS"
transaction_data$BRAND[transaction_data$BRAND == "SMITH"] <- "SMITHS"
transaction_data$BRAND[transaction_data$BRAND == "NCC"] <- "NATURAL"
transaction_data$BRAND[transaction_data$BRAND == "DORITOS"] <- "DORITO"
transaction_data$BRAND[transaction_data$BRAND == "GRAIN"] <- "GRNWVES"
  
table(transaction_data$BRAND)

transaction_data %>%
  filter(PROD_QTY > 5) %>%
  arrange(desc(PROD_QTY))

transaction_data %>%
  filter(PROD_QTY > 5) %>%
  arrange(desc(PROD_QTY))
  
data <- transaction_data %>%
  inner_join(purchase_behaviour, by = "LYLTY_CARD_NBR")

str(data)
summary(data)

data <- data %>%
  mutate(PRICE_PER_UNIT = TOT_SALES / PROD_QTY)

segment_summary <- data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(
    TOTAL_SALES = sum(TOT_SALES),
    TOTAL_QTY = sum(PROD_QTY),
    TRANSACTIONS = n_distinct(TXN_ID),
    CUSTOMERS = n_distinct(LYLTY_CARD_NBR),
    SALES_PER_CUSTOMER = TOTAL_SALES / CUSTOMERS,
    CHIPS_PER_CUSTOMER = TOTAL_QTY / CUSTOMERS,
    AVG_PRICE_PER_UNIT = TOTAL_SALES / TOTAL_QTY,
    PACKS_PER_TRANSACTION = TOTAL_QTY / TRANSACTIONS
  ) %>%
  arrange(desc(TOTAL_SALES))

segment_summary

# PLOTS
# ==============================

# 10. Total Sales by Segment
ggplot(
  segment_summary,
  aes(
    x = reorder(paste(LIFESTAGE, PREMIUM_CUSTOMER, sep = " - "), TOTAL_SALES),
    y = TOTAL_SALES
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Total Sales by Customer Segment",
    x = "Customer Segment",
    y = "Total Sales"
  ) +
  theme_minimal()

# Insight:
# Family segments, especially Older Families - Budget, contribute the highest total sales.

# 11. Average Price per Unit by Segment
ggplot(
  segment_summary,
  aes(
    x = reorder(paste(LIFESTAGE, PREMIUM_CUSTOMER, sep = " - "), AVG_PRICE_PER_UNIT),
    y = AVG_PRICE_PER_UNIT
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Average Price per Unit by Customer Segment",
    x = "Customer Segment",
    y = "Average Price per Unit"
  ) +
  theme_minimal()

# Insight:
# Young Singles/Couples - Mainstream pay the highest average price per unit.

# 12. Total Quantity by Segment
ggplot(
  segment_summary,
  aes(
    x = reorder(paste(LIFESTAGE, PREMIUM_CUSTOMER, sep = " - "), TOTAL_QTY),
    y = TOTAL_QTY
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Total Quantity Purchased by Customer Segment",
    x = "Customer Segment",
    y = "Total Quantity"
  ) +
  theme_minimal()

# Insight:
# Family segments drive category sales through higher purchase volumes.

# 13. Sales per Customer by Segment
ggplot(
  segment_summary,
  aes(
    x = reorder(paste(LIFESTAGE, PREMIUM_CUSTOMER, sep = " - "), SALES_PER_CUSTOMER),
    y = SALES_PER_CUSTOMER
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Sales per Customer by Segment",
    x = "Customer Segment",
    y = "Sales per Customer"
  ) +
  theme_minimal()

# Insight:
# This shows which customer groups spend more on average per customer.

# 14. Packs per Transaction by Segment
ggplot(
  segment_summary,
  aes(
    x = reorder(paste(LIFESTAGE, PREMIUM_CUSTOMER, sep = " - "), PACKS_PER_TRANSACTION),
    y = PACKS_PER_TRANSACTION
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Packs per Transaction by Segment",
    x = "Customer Segment",
    y = "Packs per Transaction"
  ) +
  theme_minimal()

# Insight:
# This helps identify which segments tend to buy more packs each time they shop.

# 15. Average Pack Size by Segment
ggplot(
  pack_summary,
  aes(
    x = reorder(paste(LIFESTAGE, PREMIUM_CUSTOMER, sep = " - "), AVG_PACK_SIZE),
    y = AVG_PACK_SIZE
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Average Pack Size by Customer Segment",
    x = "Customer Segment",
    y = "Average Pack Size (g)"
  ) +
  theme_minimal()

# Insight:
# Pack size differences exist, but they are smaller than differences in sales and pricing.

# 16. Distribution of Price per Unit (Boxplot)
ggplot(
  data,
  aes(
    x = LIFESTAGE,
    y = PRICE_PER_UNIT,
    fill = PREMIUM_CUSTOMER
  )
) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Price Distribution by Lifestage and Customer Type",
    x = "Lifestage",
    y = "Price per Unit"
  ) +
  theme_minimal()

# Insight:
# This shows how price paid varies within and across groups, not just the average.

# 17. Relationship Between Quantity and Sales (Scatter Plot)
ggplot(
  data,
  aes(
    x = PROD_QTY,
    y = TOT_SALES
  )
) +
  geom_point(alpha = 0.3) +
  labs(
    title = "Relationship Between Quantity Purchased and Total Sales",
    x = "Quantity Purchased",
    y = "Total Sales"
  ) +
  theme_minimal()

# Insight:
# There is a clear positive relationship between quantity purchased and total sales.

# 18. Distribution of Pack Sizes (Histogram)
ggplot(
  data,
  aes(x = PACK_SIZE)
) +
  geom_histogram(bins = 20) +
  labs(
    title = "Distribution of Pack Sizes",
    x = "Pack Size (g)",
    y = "Frequency"
  ) +
  theme_minimal()

# Insight:
# Purchases cluster around a few standard pack sizes.

# 19. Quantity Distribution by Lifestage (Faceted Histogram)
ggplot(
  data,
  aes(x = PROD_QTY)
) +
  geom_histogram(bins = 15) +
  facet_wrap(~ LIFESTAGE) +
  labs(
    title = "Quantity Distribution Across Lifestages",
    x = "Quantity Purchased",
    y = "Frequency"
  ) +
  theme_minimal()





















