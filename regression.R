library(haven)
library(tidyverse)
library(ineq)
library(openxlsx)
library(stringi)
library(readxl)
library(psych)
library(scales)
library(sandwich)
library(lmtest)

setwd("/Users/ciciliu/Documents/2024 Fall/ECON 490/thesis/Cleaned Data")

#====================== EDA ======================
panel_data <- read.csv("panel.csv")
str(panel_data)
colnames(panel_data)
eda_variables <- c("company", "robot", "cable_per_area", "income", "non_cognitive_score",
                   "cognitive_score", "comprehensive_score", "education", 
                   "gender", "age", "health", "city_tech", "city_edu",
                   "city_industry")
                   #"ln_income", "ln_company"
summary_table <- describe(panel_data[,eda_variables]) |> 
  select(n, mean, min, max, range, sd) |>
  mutate_if(is.numeric, ~round(., 2))
summary_table

table_flex <- flextable(summary_table)
doc <- read_docx() %>%
  body_add_flextable(table_flex) %>%
  body_add_par("")
# print(doc, target = "table_output.docx")

ai <- read.csv("ai.csv")
# Growth rate 2014-2016 education
ai_year <- ai |> group_by(year) |> 
  summarize(company = sum(company)) |>
  mutate(year = as.numeric(year),
         growth_rate = (company / lag(company) - 1) * 100) |>
  ungroup() |> 
  filter(year >= 2005 & year <=2022)

growth_rate_ai1416 <- ((ai_year[ai_year$year==2016,"company"]/ai_year[ai_year$year==2014,"company"])^0.5-1)*100
growth_rate_income_edu <- merge(education_income14, education_income16, by="I2_1") |>
  mutate(income_r = ((mean_income_16/mean_income_14)^0.5-1)*100)

# Growth rate 2014-2016 industry
growth_rate_income_ind <- panel_data |> group_by(industry_type, year) |>
  summarize(number_of_people = n(),
            mean_income = mean(income))

growth_rate_income_ind_summary <- growth_rate_income_ind %>%
  group_by(industry_type) %>%
  summarize(
    mean_income_2014 = mean_income[year == 2014],
    mean_income_2016 = mean_income[year == 2016]
  ) %>%
  mutate(growth_rate = ((mean_income_2016/mean_income_2014)^0.5-1) * 100)
growth_rate_income_ind_summary
ggplot(panel_data, aes(x=company, y=income)) +
  geom_point()

ai |> filter(year==2017) |> group_by(city_cn) |> summarize(no.companies=sum(company)) |>
  filter(no.companies < 2)
head(ai)

panel_data$education_bin <- case_when(panel_data$education %in% c(0:6) ~ "low",
                                      panel_data$education %in% c(7:12) ~ "medium",
                                      panel_data$education > 12 ~ "high")


data_for_pie <- panel_data |> 
  filter(year == 2016) |> 
  mutate(industry_type = case_when(
    industry_type == 1 ~ "Primary Sector",
    industry_type == 2 ~ "Secondary Sector",
    industry_type == 3 ~ "Tertiary Sector"),
  education_bin = factor(education_bin, levels = c("low", "medium", "high"),
                         labels = c("Low-Skilled", "Medium-Skilled", "High-Skilled"))) |>
  group_by(education_bin, industry_type) |> 
  summarize(count=n()) |>
  mutate(proportion = count / sum(count)) |>
  ungroup()

pie_chart <- ggplot(data_for_pie, aes(x = "", y = proportion, fill = industry_type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  facet_wrap(~ education_bin) +
  labs(
    title = NULL,
    x = NULL, y = NULL, fill = "Industry Type"
  ) +
  geom_text(aes(
    label = sprintf("%.1f%%\n(%d)", proportion * 100, count)
  ), position = position_stack(vjust = 0.5), size = 2) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 10, face = "bold")
  )
pie_chart
#====================== Regression: Company ======================
# Main====================
# library(plm)
# panel_data_p <- pdata.frame(panel_data, index=c("IID", "year"))
# panel_data_p$city <- as.factor(panel_data_p$city)
# summary(plm(ln_income ~ ln_company + I(ln_company^2) + year + industry + city,
#     data = panel_data_p, model = "within"))

# panel_data <- filter(panel_data, province!=540000 & province!=650000)
  
summary(reg00 <- lm(ln_income ~ ln_company + year + industry_type, panel_data))

summary(reg0 <- lm(ln_income ~ ln_company + I(ln_company^2) + year + industry_type, panel_data))

summary(reg1 <- lm(ln_income ~ ln_company + I(ln_company^2) + year + gender + education + #industry_type +
                   + age + I(age^2) + health + cognitive_score + non_cognitive_score + #ln_gdppc+
                      + city_industry + city_edu + city_tech + log(infra) +
                     cognitive_score:ln_company + non_cognitive_score:ln_company, panel_data))
cluster_se_main <- vcovCL(reg1, cluster = ~city)
coeftest(reg1, vcov = cluster_se_main)
# summary(reg1 <- lm(ln_income ~ ln_company + I(ln_company^2) + year + gender + education + industry_type +
#                      + age + I(age^2) + health + cognitive_score + non_cognitive_score +
#                      ln_gdppc + city_industry + city_edu + city_tech + log(infra), panel_data))

# IV: optical cable=====================
reg2sls1 <- ivreg(ln_income ~ I(ln_company) + I(ln_company^2) + year + gender + education + #industry_type
                   + age + I(age^2) + health + cognitive_score + non_cognitive_score
                  + city_industry + city_edu + city_tech + log(infra) + #ln_gdppc +
                 + cognitive_score:ln_company + non_cognitive_score:ln_company
                 | log(lag_company) + I(log(lag_company)^2) + year + gender + education + #industry_type
                   + age + I(age^2) + health + cognitive_score + non_cognitive_score
                 + city_industry + city_edu + city_tech + log(infra) + #ln_gdppc +
                 + cognitive_score:log(cable_per_area) + non_cognitive_score:log(cable_per_area), 
                 data = panel_data)
summary(reg2sls1, diagnostics = TRUE)

reg2sls <- ivreg(ln_income ~ I(ln_company) + I(ln_company^2) + gender + education + #industry_type  + year
                 + age + I(age^2) + health + cognitive_score + non_cognitive_score
                  + city_industry + city_edu + city_tech + log(infra)
                + cognitive_score:ln_company + non_cognitive_score:ln_company
                 | log(cable_per_area) + I(log(cable_per_area)^2) + gender + education + #industry_type + year
                 + age + I(age^2) + health + cognitive_score + non_cognitive_score
                  + city_industry + city_edu + city_tech + log(infra)
                 + cognitive_score:log(cable_per_area) + non_cognitive_score:log(cable_per_area), 
                 data = panel_data)
summary(reg2sls, diagnostics = TRUE)
cluster_se_main <- vcovCL(reg2sls, cluster = ~city)
coeftest(reg2sls, vcov = cluster_se_main)

library(clubSandwich)
cluster_se <- coef_test(reg2sls, vcov = "CR2", cluster = panel_data$city)
print(cluster_se)

# Comprehensive score====================
mean <- mean(panel_data$comprehensive_score)
low_skill_com <- filter(panel_data, panel_data$cognitive_score < mean)
high_skill_com <- filter(panel_data, panel_data$cognitive_score >= mean)

summary(com1 <- lm(ln_income ~ ln_company + I(ln_company^2) + year + gender + education + # industry_type +
                     + age + I(age^2) + health + cognitive_score + non_cognitive_score + log(infra) +
                     ln_gdppc + city_industry + city_edu + city_tech +
                     cognitive_score:ln_company + non_cognitive_score:ln_company, low_skill_com))

summary(com2 <- lm(ln_income ~ ln_company + I(ln_company^2) + year + gender + education + # industry_type +
                     + age + I(age^2) + health + cognitive_score + non_cognitive_score + log(infra) +
                     ln_gdppc + city_industry + city_edu + city_tech+
                     cognitive_score:ln_company + non_cognitive_score:ln_company, high_skill_com))

reg2sls_low_com <- ivreg(ln_income ~ I(ln_company) + I(ln_company^2) + gender + education + year  
                         + age + I(age^2) + health
                         + city_industry + city_edu + city_tech
                         | log(cable_per_area) + I(log(cable_per_area)^2) + gender + education + year 
                         + age + I(age^2) + health
                         + city_industry + city_edu + city_tech,  
                         data = low_skill_com)
summary(reg2sls_low_com, diagnostics = TRUE)
clustered_se <- vcovCL(reg2sls_low_com, cluster = ~ province)
coeftest(reg2sls_low_com, vcov. = clustered_se)

reg2sls_high_com <- ivreg(ln_income ~ I(ln_company) + I(ln_company^2) + gender + education + year  
                          + age + I(age^2) + health
                          + city_industry + city_edu + city_tech
                          | log(cable_per_area) + I(log(cable_per_area)^2) + gender + education + year 
                          + age + I(age^2) + health
                          + city_industry + city_edu + city_tech, 
                         data = high_skill_com)
summary(reg2sls_high_com, diagnostics = TRUE)
clustered_se <- vcovCL(reg2sls_high_com, cluster = ~ province)
coeftest(reg2sls_high_com, vcov. = clustered_se)

# Cognitive score====================
mean_cog <- mean(panel_data$cognitive_score)
low_skill <- filter(panel_data, panel_data$cognitive_score < mean_cog)
high_skill <- filter(panel_data, panel_data$cognitive_score >= mean_cog)

summary(cog1 <- lm(ln_income ~ ln_company + I(ln_company^2) + year + gender + education + # industry_type +
                     + age + I(age^2) + health + cognitive_score + non_cognitive_score + log(infra) +
                     ln_gdppc + city_industry + city_edu + city_tech +
                     cognitive_score:ln_company + non_cognitive_score:ln_company, low_skill))

summary(cog2 <- lm(ln_income ~ ln_company + I(ln_company^2) + year + gender + education + # industry_type +
                     + age + I(age^2) + health + cognitive_score + non_cognitive_score + log(infra) +
                     ln_gdppc + city_industry + city_edu + city_tech +
                     cognitive_score:ln_company + non_cognitive_score:ln_company, high_skill))

reg2sls_high_skill <- ivreg(ln_income ~ I(ln_company) + I(ln_company^2) + gender + education + year  
                            + age + I(age^2) + health
                            + city_industry + city_edu + city_tech
                            | log(cable_per_area) + I(log(cable_per_area)^2) + gender + education + year 
                            + age + I(age^2) + health
                            + city_industry + city_edu + city_tech,  
                          data = high_skill)
summary(reg2sls_high_skill, diagnostics = TRUE)
clustered_se <- vcovCL(reg2sls_high_skill, cluster = ~ province)
coeftest(reg2sls_high_skill, vcov. = clustered_se)

reg2sls_low_skill <- ivreg(ln_income ~ I(ln_company) + I(ln_company^2) + year + gender + education + # industry_type
                            + age + I(age^2) + health + cognitive_score + non_cognitive_score
                            + ln_gdppc + city_industry + city_edu + city_tech + log(infra)
                            + cognitive_score:ln_company + non_cognitive_score:ln_company
                            | log(cable_per_area) + I(log(cable_per_area)^2) + year + gender + education + # industry_type
                            + age + I(age^2) + health + cognitive_score + non_cognitive_score
                            + ln_gdppc + city_industry + city_edu + city_tech + log(infra)
                            + cognitive_score:log(cable_per_area) + non_cognitive_score:log(cable_per_area), 
                            data = low_skill)
summary(reg2sls_low_skill, diagnostics = TRUE)
clustered_se <- vcovCL(reg2sls_low_skill, cluster = ~ province)
coeftest(reg2sls_low_skill, vcov. = clustered_se)

# Non-cognitive score====================
mean_noncog <- mean(panel_data$non_cognitive_score)
low_noncog_skill <- filter(panel_data, panel_data$non_cognitive_score < mean_noncog)
high_noncog_skill <- filter(panel_data, panel_data$non_cognitive_score >= mean_noncog)

summary(noncog1 <- lm(ln_income ~ ln_company + I(ln_company^2) + year + gender + education + # industry_type +
                        + age + I(age^2) + health + cognitive_score + non_cognitive_score + log(infra) +
                        ln_gdppc + city_industry + city_edu + city_tech+
                        cognitive_score:ln_company + non_cognitive_score:ln_company, low_noncog_skill))

summary(noncog2 <- lm(ln_income ~ ln_company + I(ln_company^2) + year + gender + education + # industry_type +
                        + age + I(age^2) + health + cognitive_score + non_cognitive_score + log(infra) +
                        ln_gdppc + city_industry + city_edu + city_tech+
                        cognitive_score:ln_company + non_cognitive_score:ln_company, high_noncog_skill))

reg2sls_low_noncog_skill <- ivreg(ln_income ~ I(ln_company) + I(ln_company^2) + gender + education + year  
                                  + age + I(age^2) + health
                                  + city_industry + city_edu + city_tech
                                  | log(cable_per_area) + I(log(cable_per_area)^2) + gender + education + year 
                                  + age + I(age^2) + health
                                  + city_industry + city_edu + city_tech, 
                           data = low_noncog_skill)
summary(reg2sls_low_noncog_skill, diagnostics = TRUE)
clustered_se <- vcovCL(reg2sls_low_noncog_skill, cluster = ~ province)
coeftest(reg2sls_low_noncog_skill, vcov. = clustered_se)

reg2sls_high_noncog_skill <- ivreg(ln_income ~ I(ln_company) + I(ln_company^2) + gender + education + year  
                                   + age + I(age^2) + health
                                   + city_industry + city_edu + city_tech
                                   | log(cable_per_area) + I(log(cable_per_area)^2) + gender + education + year 
                                   + age + I(age^2) + health
                                   + city_industry + city_edu + city_tech, 
                                  data = high_noncog_skill)
summary(reg2sls_high_noncog_skill, diagnostics = TRUE)
clustered_se <- vcovCL(reg2sls_high_noncog_skill, cluster = ~ province)
coeftest(reg2sls_high_noncog_skill, vcov. = clustered_se)

# Industry==================
prime_industry <- c(1:2)
second_industry <- c(3:5)
tert_industry <- c(6:15)
prime_pop <- filter(panel_data, industry %in% prime_industry)
second_pop <- filter(panel_data, industry %in% second_industry)
tert_pop <- filter(panel_data, industry %in% tert_industry)

summary(ind1 <- lm(ln_income ~ ln_company + I(ln_company^2) + year + gender + education +
                    age + I(age^2) + health + cognitive_score + non_cognitive_score + log(infra) +
                    ln_gdppc + city_industry + city_edu + city_tech +
                     cognitive_score:ln_company + non_cognitive_score:ln_company, prime_pop))

summary(ind2 <- lm(ln_income ~ ln_company + I(ln_company^2) + year + gender + education+
                    age + I(age^2) + health + cognitive_score + non_cognitive_score + log(infra) +
                    ln_gdppc + city_industry + city_edu + city_tech +
                     cognitive_score:ln_company + non_cognitive_score:ln_company, second_pop))

summary(ind3 <- lm(ln_income ~ ln_company + I(ln_company^2) + year + gender + education+
                     age + I(age^2) + health + cognitive_score + non_cognitive_score + log(infra) +
                     ln_gdppc + city_industry + city_edu + city_tech +
                     cognitive_score:ln_company + non_cognitive_score:ln_company, tert_pop))

reg2sls_ind1 <- ivreg(ln_income ~ I(ln_company) + I(ln_company^2) + gender + education + year  
                      + age + I(age^2) + health
                      + city_industry + city_edu + city_tech
                      | log(cable_per_area) + I(log(cable_per_area)^2) + gender + education + year 
                      + age + I(age^2) + health
                      + city_industry + city_edu + city_tech,  
                                   data = prime_pop)
summary(reg2sls_ind1, diagnostics = TRUE)
clustered_se <- vcovCL(reg2sls_ind1, cluster = ~ province)
coeftest(reg2sls_ind1, vcov. = clustered_se)

reg2sls_ind2 <- ivreg(ln_income ~ I(ln_company) + I(ln_company^2) + year + gender + education
                      + age + I(age^2) + health + cognitive_score + non_cognitive_score
                      + ln_gdppc + city_industry + city_edu + city_tech + log(infra)
                      + cognitive_score:ln_company + non_cognitive_score:ln_company
                      | log(cable_per_area) + I(log(cable_per_area)^2) + year + gender + education
                      + age + I(age^2) + health + cognitive_score + non_cognitive_score
                      + ln_gdppc + city_industry + city_edu + city_tech + log(infra)
                      + cognitive_score:log(cable_per_area) + non_cognitive_score:log(cable_per_area), 
                      data = second_pop)
summary(reg2sls_ind2, diagnostics = TRUE)
clustered_se <- vcovCL(reg2sls_ind2, cluster = ~ province)
coeftest(reg2sls_ind2, vcov. = clustered_se)

reg2sls_ind3 <- ivreg(ln_income ~ I(ln_company) + I(ln_company^2) + year + gender + education
                      + age + I(age^2) + health + cognitive_score + non_cognitive_score
                      + ln_gdppc + city_industry + city_edu + city_tech + log(infra)
                      + cognitive_score:ln_company + non_cognitive_score:ln_company
                      | log(cable_per_area) + I(log(cable_per_area)^2) + year + gender + education
                      + age + I(age^2) + health + cognitive_score + non_cognitive_score
                      + ln_gdppc + city_industry + city_edu + city_tech + log(infra)
                      + cognitive_score:log(cable_per_area) + non_cognitive_score:log(cable_per_area), 
                      data = tert_pop)
summary(reg2sls_ind3, diagnostics = TRUE)
clustered_se <- vcovCL(reg2sls_ind3, cluster = ~ province)
coeftest(reg2sls_ind3, vcov. = clustered_se)

# Region==================
east_panel <- filter(panel_data, region=="east")
west_panel <- filter(panel_data, region=="west")
ne_panel <- filter(panel_data, region=="ne")
central_panel <- filter(panel_data, region=="central")

summary(region1 <- lm(ln_income ~ ln_company + I(ln_company^2) + year + gender + education+ industry_type +
                     age + I(age^2) + health + cognitive_score + non_cognitive_score + log(infra) +
                     ln_gdppc + city_industry + city_edu + city_tech +
                       cognitive_score:ln_company + non_cognitive_score:ln_company, east_panel))

summary(region2 <- lm(ln_income ~ ln_company + I(ln_company^2) + year + gender + education+ industry_type +
                        age + I(age^2) + health + cognitive_score + non_cognitive_score + log(infra) +
                        ln_gdppc + city_industry + city_edu + city_tech +
                        cognitive_score:ln_company + non_cognitive_score:ln_company, west_panel))

summary(region3 <- lm(ln_income ~ ln_company + I(ln_company^2) + year + gender + education+ industry_type +
                        age + I(age^2) + health + cognitive_score + non_cognitive_score + log(infra) +
                        ln_gdppc + city_industry + city_edu + city_tech +
                        cognitive_score:ln_company + non_cognitive_score:ln_company, ne_panel))

summary(region4 <- lm(ln_income ~ ln_company + I(ln_company^2) + year + gender + education+ industry_type +
                        age + I(age^2) + health + cognitive_score + non_cognitive_score + log(infra) +
                        ln_gdppc + city_industry + city_edu + city_tech +
                        cognitive_score:ln_company + non_cognitive_score:ln_company, central_panel))

reg2sls_east <- ivreg(ln_income ~ I(ln_company) + I(ln_company^2) + year + gender + education + industry_type
                                   + age + I(age^2) + health + cognitive_score + non_cognitive_score
                                   + ln_gdppc + city_industry + city_edu + city_tech + log(infra)
                                   + cognitive_score:ln_company + non_cognitive_score:ln_company
                                   | log(cable_per_area) + I(log(cable_per_area)^2) + year + gender + education + industry_type
                                   + age + I(age^2) + health + cognitive_score + non_cognitive_score
                                   + ln_gdppc + city_industry + city_edu + city_tech + log(infra)
                                   + cognitive_score:log(cable_per_area) + non_cognitive_score:log(cable_per_area), 
                                   data = east_panel)
summary(reg2sls_east, diagnostics = TRUE)
clustered_se <- vcovCL(reg2sls_east, cluster = ~ province)
coeftest(reg2sls_east, vcov. = clustered_se)

reg2sls_west <- ivreg(ln_income ~ I(ln_company) + I(ln_company^2) + year + gender + education + industry_type
                      + age + I(age^2) + health + cognitive_score + non_cognitive_score
                      + ln_gdppc + city_industry + city_edu + city_tech + log(infra)
                      + cognitive_score:ln_company + non_cognitive_score:ln_company
                      | log(cable_per_area) + I(log(cable_per_area)^2) + year + gender + education + industry_type
                      + age + I(age^2) + health + cognitive_score + non_cognitive_score
                      + ln_gdppc + city_industry + city_edu + city_tech + log(infra)
                      + cognitive_score:log(cable_per_area) + non_cognitive_score:log(cable_per_area), 
                      data = west_panel)
summary(reg2sls_west, diagnostics = TRUE)
clustered_se <- vcovCL(reg2sls_west, cluster = ~ province)
coeftest(reg2sls_west, vcov. = clustered_se)

reg2sls_ne <- ivreg(ln_income ~ I(ln_company) + I(ln_company^2) + year + gender + education + industry_type
                      + age + I(age^2) + health + cognitive_score + non_cognitive_score
                      + ln_gdppc + city_industry + city_edu + city_tech + log(infra)
                      + cognitive_score:ln_company + non_cognitive_score:ln_company
                      | log(cable_per_area) + I(log(cable_per_area)^2) + year + gender + education + industry_type
                      + age + I(age^2) + health + cognitive_score + non_cognitive_score
                      + ln_gdppc + city_industry + city_edu + city_tech + log(infra)
                      + cognitive_score:log(cable_per_area) + non_cognitive_score:log(cable_per_area), 
                      data = ne_panel)
summary(reg2sls_ne, diagnostics = TRUE)
clustered_se <- vcovCL(reg2sls_ne, cluster = ~ province)
coeftest(reg2sls_ne, vcov. = clustered_se)

reg2sls_central <- ivreg(ln_income ~ I(ln_company) + I(ln_company^2) + year + gender + education + industry_type
                    + age + I(age^2) + health + cognitive_score + non_cognitive_score
                    + ln_gdppc + city_industry + city_edu + city_tech + log(infra)
                    + cognitive_score:ln_company + non_cognitive_score:ln_company
                    | log(cable_per_area) + I(log(cable_per_area)^2) + year + gender + education + industry_type
                    + age + I(age^2) + health + cognitive_score + non_cognitive_score
                    + ln_gdppc + city_industry + city_edu + city_tech + log(infra)
                    + cognitive_score:log(cable_per_area) + non_cognitive_score:log(cable_per_area), 
                    data = central_panel)
summary(reg2sls_central, diagnostics = TRUE)
clustered_se <- vcovCL(reg2sls_central, cluster = ~ province)
coeftest(reg2sls_central, vcov. = clustered_se)
# Education==================
panel_data$education_bin <- case_when(panel_data$education %in% c(0:6) ~ "low",
                                      panel_data$education %in% c(7:12) ~ "medium",
                                      panel_data$education > 12 ~ "high")
low_edu <- filter(panel_data, education_bin == "low")
second_edu <- filter(panel_data, education_bin == "medium")
high_edu <- filter(panel_data, education_bin == "high")

# summary(edu1 <- lm(income ~ fitted_ln_company + I(fitted_ln_company^2) + year + gender + education+ industry_type +
#                      age + I(age^2) + health + cognitive_score + non_cognitive_score + log(infra) +
#                      ln_gdppc + city_industry + city_edu + city_tech, low_edu))
# 
# summary(edu2 <- lm(income ~ fitted_ln_company + I(fitted_ln_company^2) + year + gender + education+ industry_type +
#                      age + I(age^2) + health + cognitive_score + non_cognitive_score + log(infra) +
#                      ln_gdppc + city_industry + city_edu + city_tech, second_edu))
# 
# summary(edu3 <- lm(income ~ fitted_ln_company + I(fitted_ln_company^2) + year + gender + education+ industry_type +
#                      age + I(age^2) + health + cognitive_score + non_cognitive_score + log(infra) +
#                      ln_gdppc + city_industry + city_edu + city_tech, high_edu))

## DID
# summary(lm(ln_income ~ Yangtze + year + Yangtze:year, panel_data))
## granger causality test
# print(granger_test <- grangertest(ln_income ~ fitted_ln_company, data = panel_data))

#====================== Robustness ======================
# main + cognitive
cluster_se_main <- vcovCL(reg1, cluster = ~city)
coeftest(reg1, vcov = cluster_se_main)

cluster_se_cog1 <- vcovCL(cog1, cluster = ~city)
coeftest(cog1, vcov = cluster_se_cog1)

cluster_se_cog2 <- vcovCL(cog2, cluster = ~city)
coeftest(cog2, vcov = cluster_se_cog2)

# IV + cognitive
cluster_se_iv_main_00 <- vcovCL(main_iv_00, cluster = ~province)
coeftest(main_iv_00, vcov = cluster_se_iv_main_00)

cluster_se_iv_main_01 <- vcovCL(main_iv_01, cluster = ~province)
coeftest(main_iv_01, vcov = cluster_se_iv_main_01)

cluster_se_iv_main <- vcovCL(main_iv, cluster = ~province)
coeftest(main_iv, vcov = cluster_se_iv_main)

cluster_se_iv_cog1 <- vcovCL(cog1_iv, cluster = ~province)
coeftest(cog1_iv, vcov = cluster_se_iv_cog1)

cluster_se_iv_cog2 <- vcovCL(cog2_iv, cluster = ~province)
coeftest(cog2_iv, vcov = cluster_se_iv_cog2)

# IV + non-cognitive
cluster_se_noncog1 <- vcovCL(noncog1, cluster = ~province)
coeftest(noncog1, vcov = cluster_se_noncog1)

cluster_se_noncog2 <- vcovCL(noncog2, cluster = ~province)
coeftest(noncog2, vcov = cluster_se_noncog2)

# IV + industry
cluster_se_ind1 <- vcovCL(ind1, cluster = ~province)
coeftest(ind1, vcov = cluster_se_ind1)

cluster_se_ind2 <- vcovCL(ind2, cluster = ~province)
coeftest(ind2, vcov = cluster_se_ind2)

cluster_se_ind3 <- vcovCL(ind3, cluster = ~province)
coeftest(ind3, vcov = cluster_se_ind3)

# IV + region
cluster_se_region1 <- vcovCL(region1, cluster = ~province)
coeftest(region1, vcov = cluster_se_region1)

cluster_se_region2 <- vcovCL(region2, cluster = ~province)
coeftest(region2, vcov = cluster_se_region2)

cluster_se_region3 <- vcovCL(region3, cluster = ~province)
coeftest(region3, vcov = cluster_se_region3)

cluster_se_region4 <- vcovCL(region4, cluster = ~province)
coeftest(region4, vcov = cluster_se_region4)

 # IV + education bin
cluster_se_edu1 <- vcovCL(edu1, cluster = ~province)
coeftest(edu1, vcov = cluster_se_edu1)

cluster_se_edu2 <- vcovCL(edu1, cluster = ~province)
coeftest(edu2, vcov = cluster_se_edu2)

cluster_se_edu3 <- vcovCL(edu1, cluster = ~province)
coeftest(edu3, vcov = cluster_se_edu3)
