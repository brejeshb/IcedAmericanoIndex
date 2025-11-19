install.packages(c("tidyverse", "jsonlite", "leaflet", "corrplot"))

library(tidyverse)  # For data manipulation and ggplot2
library(jsonlite)   # For parsing JSON-like strings
library(leaflet)    # For interactive maps
library(corrplot)  # For correlation plots

data <- read_csv(file.choose(), col_types = cols(.default = "c"))

data <- data %>%
  mutate(
    rank = as.numeric(rank),
    x = as.numeric(x),
    y = as.numeric(y),
    distance = as.numeric(distance),
    reviewCount = as.numeric(reviewCount),
    averagePrice = as.numeric(averagePrice),
    americano = as.logical(americano),
    city = factor(city)  # e.g., "강릉" or "창원"
  ) %>%
  filter(success == "true")  # Filter successful scrapes

extract_americano_price <- function(menu_str) {
  if (is.na(menu_str) || menu_str == "") return(NA)
  # Clean the string: Remove outer quotes if needed, fix escaped quotes
  menu_str <- gsub('^"|"$', '', menu_str)  # Remove leading/trailing quotes
  menu_str <- gsub('\\"', '"', menu_str)   # Unescape inner quotes
  tryCatch({
    menu_list <- fromJSON(menu_str)
    americano_item <- menu_list %>% filter(name == "아메리카노" | str_detect(name, "Americano|아메리카노"))
    if (nrow(americano_item) > 0) {
      price <- as.numeric(gsub("[^0-9]", "", americano_item$price[1]))  # Extract numeric price, remove commas
      return(price)
    } else {
      return(NA)
    }
  }, error = function(e) { return(NA) })
}

data <- data %>%
  mutate(americano_price = sapply(menu_items, extract_americano_price))
summary_by_city <- data %>%
  group_by(city, categoryName) %>%
  summarise(
    count = n(),
    avg_reviews = mean(reviewCount, na.rm = TRUE),
    avg_price = mean(averagePrice, na.rm = TRUE),
    avg_americano_price = mean(americano_price, na.rm = TRUE)
  )
print(summary_by_city)


ggplot(data, aes(x = averagePrice)) +
  geom_histogram(bins = 20, fill = "blue") +
  labs(title = "Distribution of Average Prices", x = "Average Price (KRW)", y = "Count") +
  theme_minimal()


data %>%
  filter(!is.na(americano_price)) %>%
  ggplot(aes(x = city, y = americano_price, fill = city)) +
  geom_boxplot() +
  labs(title = "Americano Prices by City", y = "Price (KRW)") +
  theme_minimal()


numeric_data <- data %>%
  select(rank, distance, reviewCount, averagePrice, americano_price) %>%
  na.omit()

cor_matrix <- cor(numeric_data)
corrplot(cor_matrix, method = "circle", type = "upper", tl.cex = 0.8)

summary_by_city <- data %>%
  group_by(city, categoryName) %>%
  summarise(
    count = n(),
    avg_reviews = mean(reviewCount, na.rm = TRUE),
    avg_price = mean(averagePrice, na.rm = TRUE),
    avg_americano_price = mean(americano_price, na.rm = TRUE)
  )
print(summary_by_city)

