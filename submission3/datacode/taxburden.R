if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)



# Import the Data 
cig.data <- read_csv("data/input/The_Tax_Burden_on_Tobacco__1970-2019.csv", col_names = TRUE)
cpi.data <- read_xlsx("data/input/historical-cpi-u-202501.xlsx", skip=3)

colnames(cig.data)


# Clean Tobacco Data 
cig.data <- cig.data %>%
  mutate(measure = case_when(
    SubMeasureDesc == "Average Cost per pack" ~ "cost_per_pack",
    SubMeasureDesc == "Cigarette Consumption (Pack Sales Per Capita)" ~ "sales_per_capita",
    SubMeasureDesc == "Federal and State tax as a Percentage of Retail Price" ~ "tax_percent",
    SubMeasureDesc == "Federal and State Tax per pack" ~ "tax_dollar",
    SubMeasureDesc == "Gross Cigarette Tax Revenue" ~ "tax_revenue",
    SubMeasureDesc == "State Tax per pack" ~ "tax_state"
  )) %>%
  select(state_abb = LocationAbbr, 
         state = LocationDesc, 
         Year, 
         value=Data_Value, 
         measure)
         
final.cig.data <- pivot_wider(cig.data, 
                         id_cols = c("state","Year"), 
                         names_from = "measure",
                         values_from = "value") %>%
  arrange(state, Year)
print(final.cig.data)


# Clean CPI Data 
cpi.data <- cpi.data %>%
  mutate(across(c("Jan.", "Feb.", "Mar.", "Apr.", "May", "Jun.", 
                  "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec."), 
                as.numeric))
cpi.data <- pivot_longer(cpi.data, 
                         cols=c("Jan.","Feb.","Mar.","Apr.","May","Jun.","Jul.","Aug.","Sep.","Oct.","Nov.","Dec."),
                         names_to="month",
                         values_to="index")
cpi.data <- cpi.data %>%
  group_by(Year) %>%
  summarize(index=mean(index, na.rm=TRUE))


# Form Final Dataset 
### adjust to 2010 
final.cig.data <- final.cig.data %>%
  mutate(Year = as.integer(Year))

cpi.data <- cpi.data %>%
  mutate(Year = as.integer(Year))

final.data <- final.cig.data %>%
  left_join(cpi.data, by="Year") %>%
  mutate(price_cpi=cost_per_pack*(218/index))

write_rds(final.data,"data/output/taxburden.rds")