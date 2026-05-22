#################################
#### Lab FDI and Trade Data -revise
#### May 22, 2026 
#### Hyoung-geun Cho
#### Version 1.2 
 

# clear environment
rm(list = ls())


#install.packages("censusapi")
library(censusapi)
library(tidyverse)

# Get annual U.S. export data by country
exports_cty_yr<-getCensus(
  name = "timeseries/intltrade/exports/naics",
  vars = c("ALL_VAL_YR", "YEAR", "CTY_CODE", "CTY_NAME"),
  time = "from 2015",
  MONTH = "12",
  show_call = TRUE
)
head(exports_cty_yr)

# Filter out region and aggregation codes
exports_cty_yr_clean <- exports_cty_yr %>%
  filter(!(substr(CTY_CODE, 1, 1) == "0" | substr(CTY_CODE, 2, 2) == "X" | substr(CTY_CODE,1,1)=="-"))

# Convert export values to billions of dollars
exports_cty_yr_clean <- exports_cty_yr_clean %>%
  mutate(ALL_VAL_YR = as.numeric(ALL_VAL_YR)/1000000000,
         YEAR = as.numeric(YEAR))

# Check for possible introduction of NAs due to conversion errors
sum(is.na(exports_cty_yr_clean$ALL_VAL_YR))

# Get top 10 export destinations by year
top10_data <- exports_cty_yr_clean %>%
  group_by(YEAR) %>%
  slice_max(order_by = ALL_VAL_YR, n = 10, with_ties = FALSE) %>%
  arrange(YEAR, desc(ALL_VAL_YR)) 
print(top10_data)

# Assign rankings within each year
top10_data<-top10_data%>%
  group_by(YEAR) %>%
  arrange(-ALL_VAL_YR, CTY_NAME) %>%
  mutate(rank = row_number()) %>%
  ungroup()

# Keep only 2015 and 2025
top10_2015_2025 <- top10_data %>%
  filter(YEAR %in% c(2015, 2025))

# Make graph for 2015 and 2025
export_graph <- ggplot(top10_2015_2025,aes(group = CTY_NAME, y = rank)) +
  geom_tile(aes(x = ALL_VAL_YR/2, width=ALL_VAL_YR, height=.5, color = CTY_NAME, fill = CTY_NAME),show.legend = FALSE) +
  geom_text(aes(x = ALL_VAL_YR, y = rank, label = CTY_NAME), nudge_x=100, size = 3.5, show.legend = FALSE) +
  scale_y_reverse(breaks = 1:10, minor_breaks = NULL)+
  facet_wrap(~ YEAR) +
  labs(x = "Export Value (billions USD)", y = "Ranking by Exports", title = "Top 10 Destinations for U.S. Exports: 2015 and 2025") +
  xlim(0, 500) +
  theme_minimal()
  
export_graph

# Save graph
ggsave(
  filename = "top10_us_exports_2015_2025.png",
  plot = export_graph,
  width = 10,
  height = 6
)
