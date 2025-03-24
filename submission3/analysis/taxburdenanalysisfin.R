if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)



# 1. Present a bar graph showing the proportion of states with a change in their cigarette tax in each year from 1970 to 1985. 
### compare by year to identify tax changes for each state 
q1final.data <- final.data %>%
  filter(Year >= 1970, Year <= 1985) %>%
  arrange(state, Year) %>%
  group_by(state) %>%
  mutate(tax_change = tax_state != lag(tax_state, default = first(tax_state))) %>%
  ungroup()

### calculate proportion of states with tax change 
taxchange.proportion <- q1final.data %>%
  group_by(Year) %>%
  summarize(proportion_change = mean(tax_change, na.rm = TRUE))

### plot the bar graph
taxchange.plot <- ggplot(taxchange.proportion, aes(x = Year, y = proportion_change)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Proportion of States with Cigarette Tax Changes",
       x = "Year",
       y = "Proportion of States") +
  theme_minimal()
print(taxchange.plot)


# 2. Plot on a single graph the average tax (in 2012 dollars) on cigarettes and the average price of a pack of cigarettes from 1970 to 2018.
### adjust taxes to 2012 values 
final.data <- final.data %>%
  mutate(price_real = cost_per_pack * (230/index),
         tax_real = tax_dollar * (230/index))

### plot the data
tax.plot <- final.data %>%
  filter(Year >= 1970 & Year <= 2018) %>%
  group_by(Year) %>%
  summarize(avg_tax = mean(tax_real, na.rm = TRUE),
            avg_price = mean(price_real, na.rm = TRUE))

taxprice.plot <- ggplot(tax.plot, aes(x = Year)) +
  geom_line(aes(y = avg_tax, color = "Average Tax"), linewidth = 1.2) +
  geom_line(aes(y = avg_price, color = "Average Price"), linewidth = 1.2) +
  labs(title = "Average Tax and Price of Cigarettes (in 2012 dollars)",
       x = "Year",
       y = "Price",
       color = "Legend") +
  theme_minimal()

print(taxprice.plot)



# 3
final.data <- final.data %>%
  mutate(Year = as.integer(Year))

### calculate price difference 
q3final.data <- final.data %>% 
  filter(Year == 1970) %>% select(state, price_1970=price_real) %>% #### price_real vs price_cpi!!!
  left_join(final.data %>%  filter(Year == 2018) %>% select(state, price_2018=price_real), by=c("state")) %>% 
  mutate(price_change=price_2018-price_1970)

highest.change <- q3final.data %>% slice_max(price_change, n=5) %>% mutate (change_group = "high")
lowest.change <- q3final.data %>% slice_min(price_change, n=5) %>% mutate (change_group = "low")
bothchange.group <- rbind(highest.change, lowest.change)

top.bottom.price <- final.data %>% ungroup() %>% 
inner_join(bothchange.group %>% select(state, change_group),
            by=c("state"))


## Plot the sales per capita for these states 
top5.plot <- top.bottom.price %>% filter(change_group=="high") %>% 
  ggplot(aes(x = Year, y = sales_per_capita, color = state)) +
  stat_summary(fun="mean", geom="line") +
  labs(x = "Year",
       y = "Packs Sold Per Capita",
       color = "State") +
  theme_minimal()

print(top5.plot)


# 4. Identify the 5 states with the lowest increases in cigarette prices over the time period. Plot the average number of packs sold per capita for those states from 1970 to 2018.
bottom5.plot <- top.bottom.price %>% filter(change_group=="low") %>% 
  ggplot(aes(x = Year, y = sales_per_capita, color = state)) +
  stat_summary(fun="mean", geom="line") +
  labs(x = "Year",
       y = "Packs Sold Per Capita",
       color = "State") +
  theme_minimal()

print(bottom5.plot)



# 5. Compare the trends in sales from the 5 states with the highest price increases to those with the lowest price increases.
comparison.plot <- top.bottom.price %>% 
  ggplot(aes(x = Year, y = sales_per_capita, color = change_group)) + 
  stat_summary(fun = "mean", geom = "line") + 
  labs(x = "Year",
       y = "Average Packs Sold Per Capita",
       color = "Level of Price Increase"
  ) + 
  theme_minimal()

print(comparison.plot)

#### 1970-1990 


# 6.
final.data <- final.data %>%
  mutate(log_sales = log(sales_per_capita),
         log_price = log(price_real),
         log_total_tax = log(tax_real))

### regression
library(fixest)
ols.1 <- feols(log_sales ~ log_price, data = final.data %>% filter(Year >= 1970 & Year <= 1990))
summary(ols.1)


# 7.
### run regression 
ivs.1 <- feols(log_sales ~ 1 | log_price ~ log_total_tax, data = final.data %>% filter(Year >= 1970 & Year <= 1990))
summary(ivs.1)



# 8.
### first stage 
first.stage.a <- feols(log_price ~ log_total_tax, data = final.data %>% filter(Year >= 1970 & Year <= 1990))
summary(first.stage.a) 


#first.stage.a <- lm(log_price ~ log_total_tax, data = finaldata.70.90)
### reduced form 
reduced.form.a <- feols(log_sales ~ log_total_tax, data = final.data %>% filter(Year >= 1970 & Year <= 1990))
summary(reduced.form.a)

## trying to make both into a nice table didnt work
# Load necessary libraries
#install.packages("broom")
#library(broom)



# Summarize both s and extract coefficients and statistics
#first.stage.a.summary <- summary(first.stage.a)$coefficients
#reduced.form.a.summary <- summary(reduced.form.a)$coefficients

# Convert the summaries to data frames for easier manipulation
#first.stage.a.df <- as.data.frame(first.stage.a.summary)
#reduced.form.a.df <- as.data.frame(reduced.form.a.summary)



# Add a column to differentiate between the s
#first.stage.a.df$ <- "First Stage"
#reduced.form.a.df$ <- "Reduced Form"

# Combine the data frames into one
#results.df <- rbind(first.stage.a.df, reduced.form.a.df)

# Print the results as a table
#print(results.df)



## Same questions but looking at year range 1991-2015
#### 1991-2015 

# 6.2
ols.2 <- feols(log_sales ~ log_price, data = final.data %>% filter(Year >= 1991 & Year <= 2015))
summary(ols.2)



# 7.2 
ivs.2 <- feols(log_sales ~ 1 | log_price ~ log_total_tax, data = final.data %>% filter(Year >= 1991 & Year <= 2015))
summary(ivs.2)
#final.data.91.15 <- final.data.91.15 %>%
 # mutate(log_sales = log(sales_per_capita),
  #       log_price = log(price_cpi),
   #      log_total_tax = log(tax_dollar))




# 8.2
### first stage 
first.stage.b <- feols(log_price ~ log_total_tax, data = final.data %>% filter(Year >= 1991 & Year <= 2015))
summary(first.stage.b)

### reduced form 
reduced.form.b <- feols(log_sales ~ log_total_tax, data = final.data %>% filter(Year >= 1991 & Year <= 2015))
summary(reduced.form.b)


# 10 Comparison 
coef.1 <- coef(ivs.1)
coef.2 <- coef(ivs.2)  

comparison.table <- data.frame(
  "1970-1990" = coef.1["fit_log_price"],
  "1991-2015" = coef.2["fit_log_price"])


library(knitr)
rownames(comparison.table) <- "Slope Elasticity"
kable(comparison.table, col.names = c("1970-1990", "1991-2015"), 
      caption = "Elasticity Comparison for 1970-1990 and 1991-2015", 
      format = "markdown", align = "c")



rm(list = setdiff(ls(), c("taxchange.plot", "taxprice.plot", "top5.plot", "bottom5.plot", "comparison.plot", "ols.1", "ivs.1", "first.stage.a", "reduced.form.a", "ols.2", "ivs.2", "first.stage.b", "reduced.form.b", "comparison.table")))
save.image("submission3/results/homework3sub3_workspace.RData")