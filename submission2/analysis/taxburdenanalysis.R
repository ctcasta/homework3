if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)



# 1. Present a bar graph showing the proportion of states with a change in their cigarette tax in each year from 1970 to 1985. 
### compare by year to identify tax changes for each state 
final.data.q1 <- final.data %>%
  filter(Year >= 1970, Year <= 1985) %>%
  arrange(state, Year) %>%
  group_by(state) %>%
  mutate(tax_change = tax_state != lag(tax_state, default = first(tax_state))) %>%
  ungroup()

### calculate proportion of states with tax change 
tax.change.proportion <- final.data.q1 %>%
  group_by(Year) %>%
  summarize(proportion_change = mean(tax_change, na.rm = TRUE))

### plot the bar graph
tax.change.plot <- ggplot(tax.change.proportion, aes(x = Year, y = proportion_change)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Proportion of States with Cigarette Tax Changes",
       x = "Year",
       y = "Proportion of States") +
  theme_minimal()
print(tax.change.plot)


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

tax.price.plot <- ggplot(tax.plot, aes(x = Year)) +
  geom_line(aes(y = avg_tax, color = "Average Tax"), linewidth = 1.2) +
  geom_line(aes(y = avg_price, color = "Average Price"), linewidth = 1.2) +
  labs(title = "Average Tax and Price of Cigarettes (in 2012 dollars)",
       x = "Year",
       y = "Price",
       color = "Legend") +
  theme_minimal()

print(tax.price.plot)





# 3
final.data <- final.data %>%
  mutate(Year = as.integer(Year))

### calculate price difference 
final.data.q3 <- final.data %>% 
  filter(Year == 1970) %>% select(state, price_1970=price_real) %>% #### price_real vs price_cpi!!!
  left_join(final.data %>%  filter(Year == 2018) %>% select(state, price_2018=price_real), by=c("state")) %>% 
  mutate(price_change=price_2018-price_1970)

high.change <- final.data.q3 %>% slice_max(price_change, n=5) %>% mutate (change_group = "high")
low.change <- final.data.q3 %>% slice_min(price_change, n=5) %>% mutate (change_group = "low")
change.group <- rbind(high.change, low.change)

top.bottom.price <- final.data %>% ungroup() %>% 
inner_join(change.group %>% select(state, change_group),
            by=c("state"))


## Plot the sales per capita for these states 
top.5.plot <- top.bottom.price %>% filter(change_group=="high") %>% 
  ggplot(aes(x = Year, y = sales_per_capita, color = state)) +
  stat_summary(fun="mean", geom="line") +
  labs(title = "Cigarette Sales Per Capita",
       subtitle = "For the 5 States with the Highest Increase in Cigarette Prices",
       x = "Year",
       y = "Packs Sold Per Capita",
       color = "State") +
  theme_minimal()

print(top.5.plot)



# 4

### bottom 5
bottom5.data <- final.data %>%
  filter(state %in% bottom5.states) %>%
  group_by(Year, state) %>%
  summarize(avg_packs_per_capita = mean(sales_per_capita, na.rm = TRUE)) %>%
  ungroup()

### plot 
bottom5.plot <- ggplot(bottom5.data, aes(x = Year, y = avg_packs_per_capita, color = state)) +
  geom_line(linewidth = 1) +
  geom_point(size = 0.8, shape = 16, alpha = 0.5) +
  labs(
    title = "Average Packs Sold Per Capita (Top 5 States with Lowest Price Increase)",
    x = "Year",
    y = "Average Packs Sold Per Capita",
    color = "State"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
print(bottom5.plot)

# 5
top5.avg <- final.data %>%
  inner_join(top5.states %>% tibble(state = .), by = "state") %>%  # Efficient filtering
  group_by(Year) %>%
  summarize(avg_packs_top5 = mean(sales_per_capita, na.rm = TRUE)) %>%
  ungroup()

bottom5.avg <- final.data %>%
  inner_join(bottom5.states %>% tibble(state = .), by = "state") %>%  
  group_by(Year) %>%
  summarize(avg_packs_bot5 = mean(sales_per_capita, na.rm = TRUE)) %>%
  ungroup()

### merge 
final.data.q4 <- left_join(top5.avg, bottom5.avg, by = "Year")

### plot 
comparison.plot <- ggplot(final.data.q4, aes(x = Year)) + 
  geom_line(aes(y = avg_packs_top5, color = "Highest Increase"), linewidth = 1.2) +
  geom_line(aes(y = avg_packs_bot5, color = "Lowest Increase"), linewidth = 1.2) +
  labs(
    title = "Comparison of Cigarette Sales in States with High vs. Low Price Increases",
    x = "Year",
    y = "Average Packs Sold Per Capita",
    color = "Group"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
print(comparison.plot)



#### 1970-1990 
finaldata.70.90 <- final.data %>%
  filter(Year >= 1970 & Year <= 1990)


# 6.
final.data <- final.data %>%
  mutate(log_sales = log(sales_per_capita),
         log_price = log(price_real),
         log_total_tax = log(tax_real))

### regression
model.a <- lm(log_sales ~ log_price, data = finaldata.70.90)
summary(model.a)



# 7.


### run regression 
ivs.a <- feols(log_sales ~ 1 | log_price ~ log_total_tax, data = final.data %>% filter(Year >= 1970 & Year <= 1990))
summary(ivs.a)



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
install.packages("broom")
library(broom)



# Summarize both models and extract coefficients and statistics
#first.stage.a.summary <- summary(first.stage.a)$coefficients
#reduced.form.a.summary <- summary(reduced.form.a)$coefficients

# Convert the summaries to data frames for easier manipulation
#first.stage.a.df <- as.data.frame(first.stage.a.summary)
#reduced.form.a.df <- as.data.frame(reduced.form.a.summary)



# Add a column to differentiate between the models
#first.stage.a.df$model <- "First Stage"
#reduced.form.a.df$model <- "Reduced Form"

# Combine the data frames into one
#results.df <- rbind(first.stage.a.df, reduced.form.a.df)

# Print the results as a table
#print(results.df)



## Same questions but looking at year range 1991-2015
#### 1991-2015 
final.data.91.15 <- final.data %>%
  filter(Year >= 1991 & Year <= 2015)


# 6.2
ols.b <- feols(log_sales ~ log_price, data = final.data %>% filter(Year >= 1991 & Year <= 2015))
summary(ols.b)





# 7.2 
final.data.91.15 <- final.data.91.15 %>%
  mutate(log_sales = log(sales_per_capita),
         log_price = log(price_cpi),
         log_total_tax = log(tax_dollar))

### regression
library(fixest)
ivs.b <- feols(log_sales ~ 1 | log_price ~ log_total_tax, data = final.data.91.15)
summary(ivs.b)



# 8.2
### first stage 
first.stage.b <- lm(log_price ~ log_total_tax, data = final.data.91.15)
summary(first.stage.b) 

### reduced form 
reduced.form.b <- lm(log_sales ~ log_total_tax, data = final.data.91.15)
summary(reduced.form.b) 



# 10 Comparison 
coef.a <- coef(ivs.a)
coef.b <- coef(ivs.b)  

comparison.table <- data.frame(
  "1970-1990" = coef.a["fit_log_price"],
  "1991-2015" = coef.b["fit_log_price"])


library(knitr)
rownames(comparison.table) <- "Slope Elasticity"
kable(comparison.table, col.names = c("1970-1990", "1991-2015"), 
      caption = "Elasticity Comparison for 1970-1990 and 1991-2015", 
      format = "markdown", align = "c")



rm(list = setdiff(ls(), c("tax.change.plot", "tax.price.plot", "top.5.plot", "bot.5.plot", "comparison.plot", "model.a", "ivs.a", "first.stage.a", "reduced.form.a", "model.b", "ivs.b", "first.stage.b", "reduced.form.b", "comparison.table")))
save.image("submission1/results/homework3_workspace.RData")