rm(list=ls())

# getwd()
# setwd("Cleaned Data")

ai <- read.csv("ai.csv")
city <- read.csv("city.csv")
clds_panel <- read.csv("clds_panel.csv")
cable_per_area <- read.csv("cable_per_area.csv")

#====================== Merge ======================
ai_city <- merge(city, ai, by.x=c("city_code", "year"), by.y=c("city", "year"), all=T) |> 
  arrange(city_code, year)
ai_city <- select(ai_city, -city_invest) 
ai_city$city_code <- as.numeric(ai_city$city_code)
ai_city$year <- as.numeric(ai_city$year)

# AI & City = Panel
ai_city <- ai_city |>
  group_by(city_code) |>
  mutate(lag_company = lag(company, n = 1)) |>
  ungroup()

# CLDS & Panel = Panel
panel_data <- merge(clds_panel, ai_city, by.x=c("city","year"), by.y=c("city_code", "year"), all.x=T) |> 
  filter(income > 0)
panel_data$province_cn <- case_when(panel_data$province==650000 ~ "新疆维吾尔自治区",
                                    panel_data$province==510000 ~ "四川省",
                                    panel_data$province==530000 ~ "云南省",
                                    panel_data$province==430000 ~ "湖南省",
                                    panel_data$province==520000 ~ "贵州省",
                                    panel_data$province==150000 ~ "内蒙古自治区",
                                    TRUE ~ panel_data$province_cn
                                    )
panel_data <- panel_data |> mutate(ln_income = log(income),
                                   ln_company = log(company + 1),
                                   year = as.factor(year),
                                   industry = as.factor(industry),
                                   robot = log(robot_install + 1))

# IV & Panel = Panel
panel <- merge(panel_data, cable_per_area, by=c("province_cn", "year"), all=T)
panel <- panel[!(is.na(panel$robot_install) & is.na(panel$company)),]

#====================== Panel Data cleaning ======================
panel <- panel|>
  group_by(IID) |>
  filter(n_distinct(year) == 2) |>
  ungroup()

prime_industry <- c(1:2)
second_industry <- c(3:5)
tert_industry <- c(6:16)
panel$industry_type <- case_when(panel$industry %in% prime_industry ~ 1,
                                 panel$industry %in% second_industry ~ 2,
                                 panel$industry %in% tert_industry ~ 3)
panel$industry_type <- as.factor(panel$industry_type)

# write.csv(panel, "panel.csv", row.names = FALSE)

rm(list=ls())
