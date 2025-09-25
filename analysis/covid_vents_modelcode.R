library(dplyr)
library(lubridate)
library(ggplot2)
library(broom)
library(flextable)

#cleaning data
hos_data <- COVID.19_hospitalizations_by_date_20250917
hos_data <- hos_data %>% 
  dplyr::filter(!is.na(covid_new)) %>%
  mutate(
    hos_Date = as.Date(Date)
)


#build up the model of covid cases and vents
covid_vents_mode <- lm(vents_pts~covid_new, data = hos_data)
covid_vents_mode
summary(covid_vents_mode)

#use ggplot to make the figure of the model
ggplot(covid_vents_mode, aes(x = covid_new,y = vents_pts) )+
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 0.5) +
  labs(
    x = "	Number of new COVID cases",
    y = "	Number of patients on ventilators",
    title = "Number of patients on ventilators vs. Number of new COVID cases")+
  theme_minimal() 

#use ggplot() make the Quantile-quantile plot
mod_residuals <- residuals(covid_vents_mode)
mod_fitted <- fitted(covid_vents_mode)
ggplot(data.frame(residual = mod_residuals), aes(sample = residual)) +
  geom_qq() +
  geom_qq_line() +
  xlab("Theoretical quantile (Normal distribution)") +
  ylab("Sample quantile") + 
  ggtitle("Quantile-quantile plot of residuals")+
  theme_minimal() 

#summary the data of two variables
summary_stat <- hos_data %>%
  dplyr::summarise(
    covid_new_mean = mean(covid_new),
    vents_pts_mean = mean(vents_pts),
    covid_new_std = sd(covid_new),
    vents_pts_std = sd(vents_pts),
    covid_new_max = max(covid_new),
    vents_pts_max = max(vents_pts),
    covid_new_min = min(covid_new),
    vents_pts_min = min(vents_pts),
    
  )

#set up the column name and make a table by using flextable() function
colnames(summary_stat) <- c(
  "Mean \n (covid_new)",
  "Mean \n(vents_pts)",
  "SD \n (covid_new)",
  "SD \n(vents_pts)",
  "Max \n (covid_new)",
  "Max \n(vents_pts)",
  "Min \n (covid_new)",
  "Min \n(vents_pts)"
)


data_summary_table <- flextable(summary_stat) %>%
  add_header_lines(values = "Data Summary of Covid-19 New Cases and Number of patients on ventilators")%>%
  set_table_properties(width = 1, layout = "autofit") %>%
  autofit() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 7, part = "all") %>%
  bold(part = "header")

data_summary_table   

#use ggplot() to make the trend plot
ggplot(hos_data,aes(x = Date))+
  geom_line(aes(y = covid_new, group = 1, color = "covid_new"), linewidth = 0.5)+
  geom_line(aes(y = vents_pts, group = 2, color = "vents_pts"), linewidth = 0.5)+
  scale_color_manual(
    values = c("covid_new" = "red", "vents_pts" = "green")) +
  labs(
    title = "Trends Over Time",
    subtitle = "2020/3/27-2021/5/23",
    x = "Date",
    y = "count"
   ) +
  theme_minimal() 


#make the model summary plot
model_glance <- glance(covid_vents_mode)
model_glance
model_tidy <- tidy(covid_vents_mode) %>%
  mutate(
    p.value = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  )
model_tidy

model_summary_table <- flextable(model_tidy) %>%
  add_header_lines(values = "Summary of Simple Linear Regression Model")%>%
  set_header_labels(
    term = "Variable",
    estimate = "Estimate",
    std.error = "Standard error",
    statistic = "T-value",
    p.value = "P-value"
  ) %>%
  colformat_num(j = c("estimate", "std.error", "statistic", "p.value"), decimals = 3) %>%
  fontsize(size = 10, part = "all") %>%
  autofit() %>%
  add_footer_lines(values = paste(
    "Residual Standard Error:", round(model_glance$sigma, 1),
    "|R-squared:", round(model_glance$r.squared, 3),
    "| Adjusted R-squared:", round(model_glance$adj.r.squared, 3),
    "| F-statistic:", round(model_glance$statistic, 2)
  ))
model_summary_table
