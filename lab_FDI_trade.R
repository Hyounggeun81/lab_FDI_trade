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

#a different way to sort top 10 than we did for BEA
#here we use slice_max
top10_data <- imports_cty_yr_clean %>%
  group_by(YEAR) %>%
  slice_max(order_by = GEN_VAL_YR, n = 10, with_ties = FALSE) %>%
  arrange(YEAR, desc(GEN_VAL_YR)) 
# View the top 10 data
print(top10_data)

#this assigns every country to a ranking
#the ranking can change over time so it will depend on the year
#how a graph ultimately looks.
top10_data<-top10_data%>%
  group_by(YEAR) %>%
  arrange(-GEN_VAL_YR, CTY_NAME) %>%
  mutate(rank = row_number()) %>%
  ungroup()

#set the year
yrplot<-2023
ggplot(top10_data%>%filter(YEAR==yrplot),aes(group = CTY_NAME, y = rank)) +
  geom_tile(aes(x = GEN_VAL_YR/2, width=GEN_VAL_YR, height=.5, color = CTY_NAME, fill = CTY_NAME),show.legend = FALSE) +
  geom_text(aes(x = GEN_VAL_YR, y = rank, label = CTY_NAME), nudge_x=50, show.legend = FALSE) +
  scale_y_reverse(breaks = 1:10, minor_breaks = NULL)+
  labs(x = "Import Value (billions USD)", y = "Ranking by Imports", title = paste("Top 10 Countries for",yrplot)) +
  theme_minimal()
