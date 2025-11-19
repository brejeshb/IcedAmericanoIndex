
rm(list = ls()); graphics.off(); cat("\014")

library(readr)
library(dplyr)
library(sf)
library(geodata)
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


install.packages("extrafont")

library(extrafont)

font_import(pattern = "Nanum", prompt = FALSE)  # This may take a few minutes

loadfonts()

# Check available fonts

fonts()[grep("Nanum", fonts())]

font_add("nanumgothic", regular = "NanumGothic.ttf")

raw_scraped_marked <- read_csv("R/icedamericano/data/combined_scraped_output.csv")


coffee_summary1 <- raw_scraped_marked %>%
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
  semi_join(coffee_summary1 %>% filter(americano_count > 30), by = "city")


americano_city_stats <- americano_prices %>%
  filter(!is.na(americano_price_num)) %>%
  group_by(city) %>%
  summarise(
    n = n(),
    mean_price = mean(americano_price_num),
    median_price = median(americano_price_num),
    min_price = min(americano_price_num),
    max_price = max(americano_price_num)
  )




grdp_df <- read_csv("~/R/icedamericano/data/GDP/gdrp_2022_ko.csv") %>%
  rename(city = City, grdp = value)

coffee_summary <- raw_scraped_marked %>%
  group_by(city) %>%
  summarise(americano_count = n())

combined_data <- coffee_summary %>%
  left_join(grdp_df, by="city") %>%
  left_join(americano_city_stats, by="city")

gadm_path <- "/home/brejeshb2023/R/icedamericano/maps"
kor1 <- gadm(country="KOR", level=1, path=gadm_path)
kor2 <- gadm(country="KOR", level=2, path=gadm_path)

kor1_sf <- st_as_sf(kor1)
kor2_sf <- st_as_sf(kor2)


mapping_df <- data.frame(
  city = c("강릉", "광주", "천안", "서울", "청주", "춘천", "포항", "강원", "경북", "전남", "전북", "충남", "충북", "대구", "대전", "세종", "부산", "울산", "인천", "전주", "제주", "창원"),
  boundary_level = c(2, 1, 2, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2),
  match_name = c("Gangneung", "Gwangju", "Cheonan", "Seoul", "Cheongju", "Chuncheon", "Pohang", "Gangwon-do", "Gyeongsangbuk-do", "Jeollanam-do", "Jeollabuk-do", "Chungcheongnam-do", "Chungcheongbuk-do", "Daegu", "Daejeon", "Sejong", "Busan", "Ulsan", "Incheon", "Jeonju", "Jeju", "Changwon")
)

data_for_map <- left_join(combined_data, mapping_df, by="city")

data_level1 <- data_for_map %>% filter(boundary_level == 1)
data_level2 <- data_for_map %>% filter(boundary_level == 2)

kor1_with_data <- kor1_sf %>%
  left_join(data_level1, by = c("NAME_1" = "match_name")) %>%
  filter(!is.na(city))

kor2_with_data <- kor2_sf %>%
  left_join(data_level2, by = c("NAME_2" = "match_name")) %>%
  filter(!is.na(city))

common_columns <- c("city", "americano_count", "mean_price", "median_price",
                    "min_price", "max_price", "grdp", "geometry")

kor1_final <- select(kor1_with_data, all_of(common_columns))
kor2_final <- select(kor2_with_data, all_of(common_columns))

final_map_data <- rbind(kor1_final, kor2_final)

ggplot(final_map_data) +
  geom_sf(aes(fill = americano_count), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "C", name = "Cafe Count", na.value="grey90") +
  labs(
    title = "Cafe Counts by Administrative Boundary",
    caption = "Source: Naver & GADM"
  ) +
  theme_minimal()

ggplot(final_map_data) +
  geom_sf(aes(fill = mean_price), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "plasma", name = "Mean Americano Price", na.value="grey90") +
  labs(
    title = "Mean Americano Price by Korean City",
    caption = "Source: Naver Café Scrape"
  ) +
  theme_minimal()
americano_normal <- americano_prices %>%
  filter(
    #americano_price_num >= 1000 &
      americano_price_num <= 10000)



ggplot(americano_normal, aes(x = americano_price_num)) +
  geom_histogram(binwidth = 200, fill = "skyblue", color = "black") +
  labs(
    title = "Americano Price Distribution",
    x = "Price (KRW)",
    y = "Count"
  ) +
  theme_minimal()

city_order <- americano_city_stats %>%
  arrange(mean_price) %>%
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

