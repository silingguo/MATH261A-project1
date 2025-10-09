library(dplyr)
library(lubridate)
library(ggplot2)
library(broom)
library(flextable)
library(scales)

#cleaning data
hos_data <- COVID.19_hospitalizations_by_date_20250917
hos_data <- hos_data %>% 
  mutate(
    covid_pct_numeric = as.numeric(gsub("%", "", covid_pct_7davg)) / 100,
    covid_new_numeric = as.numeric(covid_new)#standardize the scale of two variables
  ) %>%
  dplyr::filter(!is.na(covid_pct_numeric) & !is.na(covid_new_numeric)) %>%
  mutate(
    hos_Date = as.Date(Date),
    covid_new_standardize = as.numeric(scale(covid_new_numeric,center = TRUE, scale = TRUE)),
    covid_pct_standardize = as.numeric(scale(covid_pct_numeric,center = TRUE, scale = TRUE)),
    composite = 0.8 * covid_pct_standardize + 0.2 * covid_new_standardize,#weighting method
    covid_score = rescale(composite, to = c(0, 100))#rescale the composite data 
  )



#build up the model of covid cases and vents
covid_vents_mode <- lm(vents_pts~covid_score, data = hos_data)
covid_vents_mode
summary(covid_vents_mode)




#use ggplot to make the figure of the model
ggplot(covid_vents_mode, aes(x = covid_score,y = vents_pts) )+
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 0.5) +
  labs(
    x = "Number of coposite covid case",
    y = "Number of patients on ventilators",
    title = "Number of patients on ventilators vs. COVID-19 Composite Severity Score")+
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
    covidscore_mean = mean(covid_score,na.rm = T ),
    covidscore_sd = sd(covid_score,na.rm = T),
    covidscore_max = max(covid_score,na.rm = T),
    covidscore_min = min(covid_score,na.rm = T),
    ventspts_mean = mean(vents_pts,na.rm = T),
    ventspts_sd = sd(vents_pts,na.rm = T),
    ventspts_max = max(vents_pts,na.rm = T),
    ventspts_min = min(vents_pts,na.rm = T)
)%>%
  tidyr::pivot_longer(cols = everything(),
                      names_to = c("variable", "stat"),
                      names_sep = "_") %>%
  tidyr::pivot_wider(names_from = variable, values_from = value) %>%
  dplyr::mutate(stat = factor(stat, levels = c("mean", "sd", "max", "min")))


#set up the column name and make a table by using flextable() function
colnames(summary_stat) <- c("Statistic", "COVID-19 Composite Severity Score", "Patients on Ventilators")

data_summary_table <- flextable(summary_stat) %>%
  add_header_lines(values = "Data Summary of composite Covid-19 cases and Number of patients on ventilators")%>%
  set_table_properties(width = 1, layout = "autofit") %>%
  autofit() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 7, part = "all") %>%
  bold(part = "header")

data_summary_table   






#use ggplot() to make the trend plot

ggplot(hos_data, aes(x = Date)) +
  geom_line(aes(y = covid_score,group = 1, color = "COVID-19 Composite Severity Score"), linewidth = 0.8) +
  geom_line(aes(y = vents_pts, group = 2,color = "Ventilator Patients"), linewidth = 0.8) +
  scale_color_manual(
    values = c("COVID-19 Composite Severity Score" = "red", "Ventilator Patients" = "blue")
  ) +
  labs(
    title = "Trends Over Time",
    subtitle = "2020/3/27-2021/5/23",
    x = "Date",
    y = "Value"
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





#make the redisual&predictor plot
ggplot(hos_data, aes(x = covid_score, y = residuals(covid_vents_mode))) +
  geom_point(alpha = 0.6, size = 2) +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  labs(title = "residual vs. predictor",
       x = "covid_score",
       y = "residual") +
  theme_minimal()





#residual histogram
ggplot(data = NULL, aes(x = residuals(covid_vents_mode))) +
  geom_histogram(bins = 30, fill = "lightyellow", color = "black", alpha = 0.7) +
  labs(title = "residual histogram",
       x = "residual",
       y = "frequency") +
  theme_minimal()

