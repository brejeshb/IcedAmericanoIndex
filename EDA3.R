rm(list = ls()); graphics.off(); cat("\014")
# install.packages("extrafont")

library(readr)
library(dplyr)
library(sf)
library(geodata)
# terra is an underlying package for geodata, mine was autoloaded
library(ggplot2)
library(viridis)
library(jsonlite)
library(purrr)
library(stringr)
library(cluster)

'''
Error: freetype-config not found.
Please install FreeType with freetype-config script.
If you have not installed FreeType, you can
download the source code from http://freetype.org/

In Debian/Ubuntu-like systems, you can use
  sudo apt-get install libfreetype6-dev
to install FreeType
'''

install.packages(c("sysfonts", "showtextdb", "showtext"))
library(showtext)
showtext_auto()

library(extrafont)

font_import(pattern = "Nanum", prompt = FALSE)  # This may take a few minutes

loadfonts()

# Check available fonts

fonts()[grep("Nanum", fonts())]

font_add("nanumgothic", regular = "NanumGothic.ttf")

raw_scraped_marked <- read_csv("R/icedamericano/data/combined_scraped_output.csv")
# 경기 is missing fyi, it failed in the scrape. Hence it disappeared from the df.


coffee_summary <- raw_scraped_marked %>%
  group_by(city) %>%
  summarise(
    total_scraped  = n(),
    success_count  = sum(success == "true", na.rm = TRUE),
    americano_count = sum(americano, na.rm = TRUE)
  )

americano_prices <- raw_scraped_marked %>%
  filter(americano == TRUE, !is.na(menu_items)) %>%
  mutate(
    items_list = map(menu_items, function(x) {
      tryCatch(fromJSON(x), error = function(e) NULL)
    }),
    
    americano_price = map_chr(items_list, function(df) {
      if (is.null(df)) return(NA_character_)
      
      idx <- grepl("아메리카노", df$name)
      if (!any(idx)) return(NA_character_)
      
      prices <- df$price[idx]
      prices <- prices[prices != "" & !is.na(prices)]
      if (length(prices) == 0) return(NA_character_)
      
      prices_num <- suppressWarnings(as.numeric(str_replace_all(prices, ",", "")))
      return(as.character(min(prices_num, na.rm = TRUE)))
    }),
    
    americano_price_num = as.numeric(americano_price),
    # americano_price_num = ifelse(americano_price_num > 20000, NA, americano_price_num)
  ) %>%
  semi_join(coffee_summary %>% filter(americano_count > 30), by = "city")


americano_city_stats <- americano_prices %>%
  filter(!is.na(americano_price_num)) %>%
  group_by(city) %>%
  summarise(
    n = sum(americano, na.rm = TRUE),
    mean_price = mean(americano_price_num),
    median_price = median(americano_price_num),
    min_price = min(americano_price_num),
    max_price = max(americano_price_num)
  )




grdp_df <- read_csv("~/R/icedamericano/data/GDP/gdrp_2022_ko.csv") %>%
  rename(city = City, grdp = value)


# The mapping dataframe maps our city names in our scraped data to province names on kor1_sf

mapping_df <- data.frame(
  city = c("강릉", "광주", "천안", "서울", "청주", "춘천", "포항", 
           "강원", "경북", "전남", "전북", "충남", "충북", 
           "대구", "대전", "세종", "부산", "울산", "인천", 
           "전주", "제주", "창원"),
  province_match = c("Gangwon-do", "Gwangju", "Chungcheongnam-do", "Seoul", 
                     "Chungcheongbuk-do", "Gangwon-do", "Gyeongsangbuk-do",
                     "Gangwon-do", "Gyeongsangbuk-do", "Jeollanam-do", 
                     "Jeollabuk-do", "Chungcheongnam-do", "Chungcheongbuk-do",
                     "Daegu", "Daejeon", "Sejong", "Busan", "Ulsan", "Incheon",
                     "Jeollabuk-do", "Jeju", "Gyeongsangnam-do"),
  stringsAsFactors = FALSE
)

# Video reference for code: https://www.youtube.com/watch?v=JO7N-4P2r4M
gadm_path <- "/home/brejeshb2023/R/icedamericano/maps"
kor1 <- gadm(country="KOR", level=1, path=gadm_path)
kor2 <- gadm(country="KOR", level=2, path=gadm_path)
kor0 <- gadm(country="KOR", level=0, path=gadm_path)

kor0_sf <- st_as_sf(kor0)
kor1_sf <- st_as_sf(kor1)
kor2_sf <- st_as_sf(kor2)


combined_data <- coffee_summary %>%
  left_join(americano_city_stats, by="city")


city_to_province <- combined_data %>%
  left_join(mapping_df, by = "city") %>%
  filter(!is.na(province_match))


provincial_coffee_data <- city_to_province %>%
  group_by(province_match) %>%
  summarise(
    total_scraped = sum(total_scraped, na.rm = TRUE),
    success_count = sum(success_count, na.rm = TRUE),
    americano_count = sum(americano_count, na.rm = TRUE),
    # Weighted mean price by number of observations
    mean_price = weighted.mean(mean_price, n, na.rm = TRUE),
    median_price = median(median_price, na.rm = TRUE),
    min_price = min(min_price, na.rm = TRUE),
    max_price = max(max_price, na.rm = TRUE),
    n_total = sum(n, na.rm = TRUE)
  )


grdp_provincial <- grdp_df %>%
  left_join(mapping_df %>% select(city, province_match), by = "city") %>%
  filter(!is.na(province_match)) %>%
  group_by(province_match) %>%
  summarise(grdp = first(grdp))


individual_income <- read_csv("~/R/icedamericano/data/Individual_Income.csv")

individual_income_clean <- individual_income %>%
  rename(
    province_name = `By province`,
    individual_income = `Individual Income per Capita`
  ) %>%
  filter(province_name != "Whole country") %>%
  mutate(individual_income = as.numeric(individual_income))

# Create mapping for income data
income_mapping <- data.frame(
  province_name = c("Seoul", "Busan", "Daegu", "Incheon", "Gwangju", "Daejeon", 
                    "Ulsan", "Sejong City", "Gyeonggi-do", "Gangwon State", 
                    "Chungcheongbuk-do", "Chungcheongnam-do", "Jeonbuk State", 
                    "Jeollanam-do", "Gyeongsangbuk-do", "Gyeongsangnam-do", "Jeju"),
  province_match = c("Seoul", "Busan", "Daegu", "Incheon", "Gwangju", "Daejeon", 
                     "Ulsan", "Sejong", "Gyeonggi-do", "Gangwon-do", 
                     "Chungcheongbuk-do", "Chungcheongnam-do", "Jeollabuk-do", 
                     "Jeollanam-do", "Gyeongsangbuk-do", "Gyeongsangnam-do", "Jeju"),
  stringsAsFactors = FALSE
)

individual_income_mapped <- individual_income_clean %>%
  left_join(income_mapping, by = "province_name") %>%
  select(province_match, individual_income)


provincial_data_full <- provincial_coffee_data %>%
  left_join(grdp_provincial, by = "province_match") %>%
  left_join(individual_income_mapped, by = "province_match")


kor1_with_data <- kor1_sf %>%
  left_join(provincial_data_full, by = c("NAME_1" = "province_match"))



# Map 1: Cafe Counts by Province
ggplot() +
  geom_sf(data = kor0_sf, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = kor1_with_data, aes(fill = americano_count), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "C", name = "Cafe Count", na.value="grey90") +
  labs(
    title = "Cafe Counts by Province",
    caption = "Source: Naver & GADM"
  ) +
  theme_minimal()

# Map 2: Mean Americano Price by Province
ggplot() +
  geom_sf(data = kor0_sf, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = kor1_with_data, aes(fill = mean_price), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "plasma", name = "Mean Americano Price (KRW)", na.value="grey90") +
  labs(
    title = "Mean Americano Price by Korean Province",
    caption = "Source: Naver Café Scrape"
  ) +
  theme_minimal()

# Histogram: Price Distribution (using original city-level data for granularity)
americano_normal <- americano_prices %>%
  filter(
    #americano_price_num > 100 &
      americano_price_num <= 10000)

ggplot(americano_normal, aes(x = americano_price_num)) +
  geom_histogram(binwidth = 200, fill = "skyblue", color = "black") +
  labs(
    title = "Americano Price Distribution",
    x = "Price (KRW)",
    y = "Count"
  ) +
  theme_minimal()

# Boxplot by City
city_order <- americano_city_stats %>%
  arrange(median_price) %>%
  pull(city)

par(family = "NanumBarunGothic")

ggplot(americano_normal, aes(x = factor(city, levels = city_order),
                             y = americano_price_num)) +
  geom_boxplot(fill = "skyblue", outlier.alpha = 0.2) +
  labs(
    title = "Americano Price Distribution by City (Boxplot)",
    x = "City",
    y = "Price (KRW)"
  ) +
  theme_minimal(base_family = "nanumgothic") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


affordability_provincial <- provincial_data_full %>%
  mutate(
    americano_affordability_grdp = (grdp * 1000000 / mean_price) / 365,
    americano_affordability_income = (individual_income * 1000 / mean_price) / 365
  )


calculate_gini <- function(affordability_values) {
  affordability_values <- affordability_values[!is.na(affordability_values)]
  n <- length(affordability_values)
  total_sum <- 0
  
  for (i in 1:n) {
    for (j in 1:n) {
      total_sum <- total_sum + abs(affordability_values[i] - affordability_values[j])
    }
  }
  
  mean_n <- mean(affordability_values)
  gini <- total_sum / (2 * n^2 * mean_n)
  
  return(gini)
}

gini_coefficient_grdp <- calculate_gini(affordability_provincial$americano_affordability_grdp)
gini_coefficient_income <- calculate_gini(affordability_provincial$americano_affordability_income)

cat(round(gini_coefficient_grdp, 4))
cat(round(gini_coefficient_income, 4))



library(scales)

plot_data <- affordability_provincial %>%
  filter(!is.na(individual_income) & !is.na(grdp)) %>%
  arrange(grdp)

scale_factor <- max(plot_data$grdp, na.rm = TRUE) / max(plot_data$individual_income, na.rm = TRUE)

ggplot(plot_data, aes(x = reorder(province_match, grdp))) +
  geom_col(aes(y = grdp, fill = "GRDP"), alpha = 0.7) +
  geom_line(aes(y = individual_income * scale_factor, group = 1, color = "Individual Income"), 
            size = 1.2) +
  geom_point(aes(y = individual_income * scale_factor, color = "Individual Income"), 
             size = 3) +
  scale_y_continuous(
    name = "GRDP (Million KRW)",
    sec.axis = sec_axis(~./scale_factor, name = "Individual Income (Thousand KRW)")
  ) +
  scale_fill_manual(values = c("GRDP" = "steelblue")) +
  scale_color_manual(values = c("Individual Income" = "coral")) +
  labs(
    title = "GRDP vs Individual Income by Province",
    x = "Province",
    caption = "Source: Korean Statistical Data"
  ) +
  theme_minimal(base_family = "nanumgothic") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    legend.title = element_blank()
  )


# MAPS 

kor1_with_full_data <- kor1_sf %>%
  left_join(affordability_provincial, by = c("NAME_1" = "province_match"))

# Individual Income Map
ggplot() +
  geom_sf(data = kor0_sf, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = kor1_with_full_data, aes(fill = individual_income), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "viridis", name = "Individual Income\n(Thousand KRW)", 
                       na.value = "grey90") +
  labs(
    title = "Individual Income by Korean Province",
    caption = "Source: Korean Statistical Data & GADM"
  ) +
  theme_minimal()

# GRDP Map
ggplot() +
  geom_sf(data = kor0_sf, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = kor1_with_full_data, aes(fill = grdp), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "magma", name = "GRDP\n(Million KRW)", 
                       na.value = "grey90") +
  labs(
    title = "GRDP by Korean Province",
    caption = "Source: Korean Statistical Data & GADM"
  ) +
  theme_minimal()

# Affordability Map (based on Individual Income)
ggplot() +
  geom_sf(data = kor0_sf, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = kor1_with_full_data, aes(fill = americano_affordability_income), 
          color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "viridis", 
                       name = "Daily Americanos\nAffordable", 
                       na.value = "grey90") +
  labs(
    title = "Americano Affordability by Individual Income",
    subtitle = "Number of Americanos affordable per day based on annual income",
    caption = "Source: Calculated from Individual Income & Mean Americano Price"
  ) +
  theme_minimal()

# Affordability Map (based on GRDP)
ggplot() +
  geom_sf(data = kor0_sf, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = kor1_with_full_data, aes(fill = americano_affordability_grdp), 
          color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "plasma", 
                       name = "Daily Americanos\nAffordable", 
                       na.value = "grey90") +
  labs(
    title = "Americano Affordability by GRDP",
    subtitle = "Number of Americanos affordable per day based on GRDP",
    caption = "Source: Calculated from GRDP & Mean Americano Price"
  ) +
  theme_minimal()



cor_data <- affordability_provincial %>%
  filter(!is.na(mean_price) & !is.na(grdp) & !is.na(individual_income)) %>%
  select(province_match, mean_price, median_price, grdp, individual_income, 
         americano_affordability_grdp, americano_affordability_income)

cat("\n=== CORRELATION ANALYSIS ===\n\n")

cor_mean_grdp <- cor(cor_data$mean_price, cor_data$grdp, use = "complete.obs")
cat("Mean Price vs GRDP: ", round(cor_mean_grdp, 4), "\n")

cor_mean_income <- cor(cor_data$mean_price, cor_data$individual_income, use = "complete.obs")
cat("Mean Price vs Individual Income: ", round(cor_mean_income, 4), "\n")

cor_median_grdp <- cor(cor_data$median_price, cor_data$grdp, use = "complete.obs")
cat("Median Price vs GRDP: ", round(cor_median_grdp, 4), "\n")

cor_median_income <- cor(cor_data$median_price, cor_data$individual_income, use = "complete.obs")
cat("Median Price vs Individual Income: ", round(cor_median_income, 4), "\n")

cor_grdp_income <- cor(cor_data$grdp, cor_data$individual_income, use = "complete.obs")
cat("GRDP vs Individual Income: ", round(cor_grdp_income, 4), "\n\n")

# Create correlation matrix
# cor_matrix <- cor(cor_data %>% select(-province_match), use = "complete.obs")
# print(round(cor_matrix, 3))

library(reshape2)
cor_matrix_melt <- melt(cor_matrix)

# # Plot heatmap
# ggplot(cor_matrix_melt, aes(Var1, Var2, fill = value)) +
#   geom_tile() +
#   scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0, limit = c(-1, 1)) +
#   geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         axis.text.y = element_text(angle = 45, hjust = 1)) +
#   labs(
#     title = "Correlation Heatmap",
#     fill = "Correlation\nCoefficient",
#     x = NULL,
#     y = NULL
#   )

# Scatter plots
p1 <- ggplot(cor_data, aes(x = grdp, y = mean_price)) +
  geom_point(size = 3, alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkblue") +
  labs(
    title = "Mean Americano Price vs GRDP",
    x = "GRDP (Million KRW)",
    y = "Mean Price (KRW)",
    subtitle = paste0("Correlation: r = ", round(cor_mean_grdp, 3))
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

p2 <- ggplot(cor_data, aes(x = individual_income, y = mean_price)) +
  geom_point(size = 3, alpha = 0.6, color = "coral") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(
    title = "Mean Americano Price vs Individual Income",
    x = "Individual Income (Thousand KRW)",
    y = "Mean Price (KRW)",
    subtitle = paste0("Correlation: r = ", round(cor_mean_income, 3))
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

p3 <- ggplot(cor_data, aes(x = grdp, y = median_price)) +
  geom_point(size = 3, alpha = 0.6, color = "seagreen") +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  labs(
    title = "Median Americano Price vs GRDP",
    x = "GRDP (Million KRW)",
    y = "Median Price (KRW)",
    subtitle = paste0("Correlation: r = ", round(cor_median_grdp, 3))
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

p4 <- ggplot(cor_data, aes(x = individual_income, y = median_price)) +
  geom_point(size = 3, alpha = 0.6, color = "purple") +
  geom_smooth(method = "lm", se = FALSE, color = "darkviolet") +
  labs(
    title = "Median Americano Price vs Individual Income",
    x = "Individual Income (Thousand KRW)",
    y = "Median Price (KRW)",
    subtitle = paste0("Correlation: r = ", round(cor_median_income, 4))
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

library(gridExtra)
grid.arrange(p1, p2, p3, p4, ncol = 2)

# Test the statistical significance of the correlation

cor_test_grdp <- cor.test(cor_data$mean_price, cor_data$grdp)
cor_test_income <- cor.test(cor_data$mean_price, cor_data$individual_income)
cor_test_median_grdp <- cor.test(cor_data$median_price, cor_data$grdp)
cor_test_median_income <- cor.test(cor_data$median_price, cor_data$individual_income)

cat("\n=== SIGNIFICANCE TESTS ===\n")
cat("Mean Price vs GRDP: r =", round(cor_test_grdp$estimate, 4), 
    ", p-value =", round(cor_test_grdp$p.value, 4), "\n")
cat("Mean Price vs Individual Income: r =", round(cor_test_income$estimate, 4), 
    ", p-value =", round(cor_test_income$p.value, 4), "\n")
cat("Median Price vs GRDP: r =", round(cor_test_median_grdp$estimate, 4), 
    ", p-value =", round(cor_test_median_grdp$p.value, 4), "\n")
cat("Median Price vs Individual Income: r =", round(cor_test_median_income$estimate, 4), 
    ", p-value =", round(cor_test_median_income$p.value, 4), "\n")






# Create list to store plots
bar_plots <- list()

# Define variables to plot
vars <- c("mean_price", "grdp", "individual_income", 
          "americano_affordability_grdp", "americano_affordability_income")

titles <- c("Mean Americano Price by Province",
            "GRDP by Province",
            "Individual Income by Province",
            "Americano Affordability (GRDP-based)",
            "Americano Affordability (Income-based)")

# Loop through variables
for(i in 1:length(vars)) {
  
  var_name <- vars[i]
  

  plot_data <- affordability_provincial %>%
    filter(!is.na(.data[[var_name]])) %>%
    arrange(.data[[var_name]])

  p <- ggplot(plot_data, aes(x = .data[[var_name]], 
                             y = reorder(province_match, .data[[var_name]]))) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    labs(
      title = titles[i],
      x = NULL,
      y = NULL
    ) +
    geom_text(aes(label = round(.data[[var_name]], 1)), 
              hjust = -0.1, size = 3) +
    theme_minimal(base_family = "nanumgothic") +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      axis.text = element_text(size = 10)
    )
  
  # Store in list with dynamic naming
  assign(paste0("bar", i), p)
  bar_plots[[i]] <- p
}

# Display individual plots
bar1  # Mean price
bar2  # GRDP
bar3  # Individual income
bar4  # Affordability GRDP
bar5  # Affordability Income

