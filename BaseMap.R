




rm(list = ls()); graphics.off(); cat("\014")
# install.packages(c("readr", "dplyr", "sf", "ggplot2", "viridis"))
library(readr)
library(dplyr)
library(sf)
library(geodata) # Raster is deprecated, gotta use geodata instead
library(ggplot2)
library(viridis)

# source of data is https://gadm.org/download_country_v3.html
# I think need terra, mine was autoloaded

raw_scraped_marked <- read_csv("R/icedamericano/data/combined_scraped_output.csv")


grdp_df <- read_csv("~/R/icedamericano/data/GDP/gdrp_2022_ko.csv") %>%
  rename(city = City, grdp = value) # Rename to 'city' and 'grdp' for consistency


coffee_summary <- raw_scraped_marked %>%
  group_by(city) %>%
  summarise(americano_count = n()) # n() counts the number of rows/cafes for each city

combined_data <- left_join(coffee_summary, grdp_df, by = "city")

print(combined_data)


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

data_for_map <- left_join(combined_data, mapping_df, by = "city")


data_level1 <- data_for_map %>% filter(boundary_level == 1)
data_level2 <- data_for_map %>% filter(boundary_level == 2)


kor1_with_data <- kor1_sf %>%
  left_join(data_level1, by = c("NAME_1" = "match_name"))%>%
  filter(!is.na(city)) # IMPORTANT: Keep only the provinces we have data for

kor2_with_data <- kor2_sf %>%
  left_join(data_level2, by = c("NAME_2" = "match_name")) %>%
  filter(!is.na(city)) # IMPORTANT: Keep only the cities we have data for


# To use rbind need same columns
common_columns <- c("city", "americano_count", "grdp", "geometry")

kor1_final <- select(kor1_with_data, all_of(common_columns))
kor2_final <- select(kor2_with_data, all_of(common_columns))


final_map_data <- rbind(kor1_final, kor2_final)

print(final_map_data)


ggplot(data = final_map_data) +
  geom_sf(aes(fill = americano_count), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "C", name = "Cafe Count" ,na.value="grey90") +
  labs(
    title = "Cafe Counts by Administrative Boundary in South Korea",
    caption = "Source: Naver & GADM"
  ) +
  theme_minimal()


ggplot(data = final_map_data) +
  geom_sf(aes(fill = grdp), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "viridis", name = "GRDP (in millions KRW)", na.value="grey90") +
  labs(
    title = "GRDP per Capita by Administrative Boundary in South Korea (2022)",
    caption = "Data Source: Statistics Korea & GADM"
  ) +
  theme_minimal()
