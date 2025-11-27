library(jsonlite)
library(dplyr)
library(ggplot2)
library(sf)
library(viridis)

# Not really sure how to use fully specify relative directory here
# u can use getwd() to check your current directory and manually set it with setwd()
# However, just right click More in the file directory and set icedamericano as wd()
# alternative: setwd("~/R/icedamericano") pls set it accordingly
# data_dir <- "/home/brejeshb2023/R/icedamericano/data/raw_api_responses/"
data_dir <- "./data/raw_api_responses/"
# list.files(path = data_dir, pattern = NULL, all.files = FALSE, full.names = FALSE)

json_files <- list.files(data_dir, pattern = "\\.json$", full.names = TRUE)
json_files <- json_files[!grepl("total", json_files)]


records <- data.frame(file = character(), city = character(), count = numeric(), totalCount = numeric(), file_percentage = numeric(), stringsAsFactors = FALSE)


for (file in json_files) {
  try({
    data <- fromJSON(file, flatten = TRUE)

    meta <- data$result$meta
    count <- meta$count
    total_count <- meta$totalCount
    city <- gsub("_box.*", "", basename(file))
    file_percentage <- ifelse(total_count > 0, (count / total_count) * 100, 0)
    records <- records %>%
      add_row(file = basename(file), city = city, count = count, totalCount = total_count, file_percentage = round(file_percentage, 2))
    
  }, silent = TRUE)
}

city_summary <- records %>%
  group_by(city) %>%
  summarise(count = sum(count, na.rm = TRUE), totalCount = sum(totalCount, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(city_percentage = round((count / totalCount) * 100, 2))


overall_count <- sum(records$count, na.rm = TRUE)
overall_total_count <- sum(records$totalCount, na.rm = TRUE)
overall_percentage <- ifelse(overall_total_count > 0, round((overall_count / overall_total_count) * 100, 2), 0)

print(head(records))


print(city_summary)


cat("\nOverall totals:\n")
cat("Total Count =", overall_count, "\n")
cat("Total TotalCount =", overall_total_count, "\n")
cat("Overall Percentage =", overall_percentage, "%\n")

# from EDA3.R, we map city name to a province in kor1_sf
mapping_df <- data.frame(
  city = c("강릉", "광주", "천안", "서울", "청주", "춘천", "포항", 
           "강원", "경북", "전남", "전북", "충남", "충북", 
           "대구", "대전", "세종", "부산", "울산", "인천", 
           "전주", "제주", "창원", "거제", "진주", "김해",
           "고양", "성남", "수원", "용인"),
  province_match = c("Gangwon-do", "Gwangju", "Chungcheongnam-do", "Seoul", 
                     "Chungcheongbuk-do", "Gangwon-do", "Gyeongsangbuk-do",
                     "Gangwon-do", "Gyeongsangbuk-do", "Jeollanam-do", 
                     "Jeollabuk-do", "Chungcheongnam-do", "Chungcheongbuk-do",
                     "Daegu", "Daejeon", "Sejong", "Busan", "Ulsan", "Incheon",
                     "Jeollabuk-do", "Jeju", "Gyeongsangnam-do", "Gyeongsangnam-do", 
                     "Gyeongsangnam-do", "Gyeongsangnam-do", "Gyeonggi-do", 
                     "Gyeonggi-do", "Gyeonggi-do", "Gyeonggi-do"),
  stringsAsFactors = FALSE
)


city_to_province <- left_join(city_summary, mapping_df, by = "city")



# Video reference for code: https://www.youtube.com/watch?v=JO7N-4P2r4M
gadm_path <- "/home/brejeshb2023/R/icedamericano/maps"

kor1 <- gadm(country="KOR", level=1, path=gadm_path)
kor2 <- gadm(country="KOR", level=2, path=gadm_path)
kor0 <- gadm(country="KOR", level=0, path=gadm_path)

kor0_sf <- st_as_sf(kor0)
kor1_sf <- st_as_sf(kor1)
kor2_sf <- st_as_sf(kor2)

# unique(kor1_sf$NAME_1) to see the names of the shapes in korea 1 layer (province)


provincial_cafe_data <- city_to_province %>%
  group_by(province_match) %>%
  summarise(
    total_cafes = sum(totalCount, na.rm = TRUE)
  )


kor1_with_data <- kor1_sf %>%
  left_join(provincial_cafe_data, by = c("NAME_1" = "province_match"))

ggplot() +
  geom_sf(data = kor0_sf, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = kor1_with_data, aes(fill = total_cafes), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "C", name = "Cafe Count", na.value = "grey90") +
  labs(
    title = "Cafe Counts by Province",
    subtitle = "",
    caption = "Source: Naver & GADM"
  ) +
  theme_minimal()
