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

setwd("/Users/ciciliu/Documents/2024 Fall/ECON 490/thesis")

#===================== CLDS =====================
setwd("CLDS")

col_names <- c('province', 'city', 'income', 'industry', 
               'foreign_language', 'fluency', 'read', 'write',
               'neuroticism', 'conscientiousness', 'extraversion', 'agreeableness', 'openness',
               'education', 'gender', 'age', 'health', 'rtype')

# entropy-weighted sum for skills
ewm <- function(data, columns) {
  normalized_data <- data[, columns] / rowSums(data[, columns], na.rm = TRUE)
  m <- nrow(normalized_data)
  k <- 1 / log(m)
  
  # Calculate entropy
  entropy <- sapply(normalized_data, function(p) {
    p[p == 0] <- .Machine$double.eps
    -k * sum(p * log(p), na.rm = TRUE)
  })
  
  # Calculate diversity and weights
  diversity <- 1 - entropy
  weights <- diversity / sum(diversity)
  
  # Return weights only
  weights
}

non_cognitive_columns <- c("neuroticism", "conscientiousness",
                           "extraversion","agreeableness","openness")
cognitive_columns <- c("foreign_language", "fluency", "read", "write")

# 2016 ==============================================
variables <- c('CITY', 'I3a_6', 'I3a_8',
               'I1_8', 'I10_9', 'I2_12_1', 'I2_12_2', 
               'I9_4_5_w16', 'I7_8_1', 'I6_3', 'I7_11', 'I7_4_5',
               'I2_1', 'gender', 'age', 'I9_4_1', 'rtype')

clds16 <- read_dta("CLDS2016individual_(STATA)_171106.dta")
clds16$age <- 2016 - clds16$birthyear

work_pop16 <- clds16 |>
  filter(age <= 65 & age >=18) |> 
  filter(work == 1 | (work == 2 & I3d_9 != 2)) |>
  drop_na(I3a_6) |>
  filter(I3a_6 != 99999 & I3a_6 != 99997 & I3a_6 != 99998)

education_income16 <- work_pop16 |>
  group_by(I2_1) |>
  summarize(no_16 = n(),
            mean_income_16 = mean(I3a_6))

# education
work_pop16$I2_1 <- ifelse(work_pop16$I2_1_1 != 1, work_pop16$I2_1 - 1, work_pop16$I2_1)
work_pop16$I2_1 <- case_when(work_pop16$I2_1 == 1 ~ 0,
                             work_pop16$I2_1 == 2 ~ 6,
                             work_pop16$I2_1 == 3 ~ 9,
                             work_pop16$I2_1 == 4 ~ 12,
                             work_pop16$I2_1 == 5 ~ 12,
                             work_pop16$I2_1 == 6 ~ 12,
                             work_pop16$I2_1 == 7 ~ 12,
                             work_pop16$I2_1 == 8 ~ 15,
                             work_pop16$I2_1 == 9 ~ 16,
                             work_pop16$I2_1 == 10 ~ 19,
                             work_pop16$I2_1 == 11 ~ 22,
                             TRUE ~ NA)

work_pop16$I2_1 <- ifelse(work_pop16$I2_1_1 != 1, work_pop16$I2_1+work_pop16$I2_1_2_0, work_pop16$I2_1)
work_pop16 <- work_pop16 |> filter(I2_1 < 99997)

work_pop16 <- work_pop16[,c('IID2016', 'PROV2016', variables)]
colnames(work_pop16) <- c('IID', col_names)

work_pop16$year <- 2016
work_pop16$city <- case_when(work_pop16$city == 110200 ~ 110100,
                             work_pop16$city == 120200 ~ 120100,
                             work_pop16$city == 310200 ~ 310100,
                             work_pop16$city == 500200 ~ 500100,
                             TRUE ~ work_pop16$city)
work_pop16$province <- work_pop16$province*10000

work_pop16$gender <- ifelse(work_pop16$gender==2, 0, 1)

work_pop16 <- drop_na(work_pop16)
work_pop16 <- filter(work_pop16, industry > 0)

# entropy-weighted skills
work_pop16$foreign_language <- ifelse(work_pop16$foreign_language==2, 0, 1)

reverse_levels <- function(x) {
  min_val <- min(x, na.rm = TRUE)
  max_val <- max(x, na.rm = TRUE)
  max_val + min_val - x
}
work_pop16 <- work_pop16 %>%
  mutate(across(c("fluency", "read", "write", "neuroticism"), reverse_levels))

non_cognitive_weights <- ewm(work_pop16, non_cognitive_columns)
cognitive_weights <- ewm(work_pop16, cognitive_columns)
comprehensive_weights <- ewm(work_pop16, c(non_cognitive_columns, cognitive_columns))

work_pop16$non_cognitive_score <- rowSums(work_pop16[non_cognitive_columns] * non_cognitive_weights)
work_pop16$cognitive_score <- rowSums(work_pop16[cognitive_columns] * cognitive_weights)
work_pop16$comprehensive_score <- rowSums(work_pop16[c(non_cognitive_columns, cognitive_columns)] * comprehensive_weights)

panel_16 <- filter(work_pop16, rtype == 1) # panel variable

work_pop16 <- select(work_pop16, -rtype, -non_cognitive_columns, -cognitive_columns)
panel_16 <- select(panel_16, -rtype, -non_cognitive_columns, -cognitive_columns)


# 2014 ==============================================
variables <- c('CITY', 'I3a_6', 'I3a_8',
               'I1_8', 'I10_9', 'I2_12_1', 'I2_12_2', 
               'I9_4_4', 'I7_8_1', 'I6_3', 'I7_11', 'I7_4_5',
               'I2_1', 'gender', 'age', 'I9_4_1', 'rtype')

clds14 <- read_dta("CLDS2014-individual-170707-STATA-release.dta")
clds14$age <- 2014 - clds14$birthyear

work_pop14 <- clds14 |>
  filter(age <= 65 & age >=18) |> 
  filter(work_r == 1 | (work_r == 2 & I3d_9 != 2)) |>
  drop_na(I3a_6) |>
  filter(I3a_6 != 99999 & I3a_6 != 99997 & I3a_6 != 99998)

education_income14 <- work_pop14 |>
  group_by(I2_1) |>
  summarize(no_14 = n(),
            mean_income_14 = mean(I3a_6))

# education
work_pop14$I2_1 <- ifelse(work_pop14$I2_1_1 != 1, work_pop14$I2_1 - 1, work_pop14$I2_1)
work_pop14$I2_1 <- case_when(work_pop14$I2_1 == 1 ~ 0,
                             work_pop14$I2_1 == 2 ~ 6,
                             work_pop14$I2_1 == 3 ~ 9,
                             work_pop14$I2_1 == 4 ~ 12,
                             work_pop14$I2_1 == 5 ~ 12,
                             work_pop14$I2_1 == 6 ~ 12,
                             work_pop14$I2_1 == 7 ~ 12,
                             work_pop14$I2_1 == 8 ~ 15,
                             work_pop14$I2_1 == 9 ~ 16,
                             work_pop14$I2_1 == 10 ~ 19,
                             work_pop14$I2_1 == 11 ~ 22,
                             TRUE ~ NA)

work_pop14$I2_1 <- ifelse(work_pop14$I2_1_1 != 1, work_pop14$I2_1+work_pop14$I2_1_2_0, work_pop14$I2_1)
work_pop14 <- work_pop14 |> filter(I2_1 < 99997)

work_pop14 <- work_pop14[,c('IID2014', 'PROVINCE', variables)]
colnames(work_pop14) <- c('IID', col_names)
work_pop14$year <- 2014
work_pop14$city <- case_when(work_pop14$city == 110200 ~ 110100,
                             work_pop14$city == 120200 ~ 120100,
                             work_pop14$city == 310200 ~ 310100,
                             work_pop14$city == 500200 ~ 500100,
                             TRUE ~ work_pop14$city)

work_pop14$gender <- ifelse(work_pop14$gender==2, 0, 1)

work_pop14 <- drop_na(work_pop14)
work_pop14 <- filter(work_pop14, industry > 0)

# entropy-weighted skills
work_pop14$foreign_language <- ifelse(work_pop14$foreign_language==2, 0, 1)

reverse_levels <- function(x) {
  min_val <- min(x, na.rm = TRUE)
  max_val <- max(x, na.rm = TRUE)
  max_val + min_val - x
}
work_pop14 <- work_pop14 %>%
  mutate(across(c("fluency", "read", "write", "neuroticism"), reverse_levels))

non_cognitive_weights <- ewm(work_pop14, non_cognitive_columns)
cognitive_weights <- ewm(work_pop14, cognitive_columns)
comprehensive_weights <- ewm(work_pop14, c(non_cognitive_columns, cognitive_columns))

work_pop14$non_cognitive_score <- rowSums(work_pop14[non_cognitive_columns] * non_cognitive_weights)
work_pop14$cognitive_score <- rowSums(work_pop14[cognitive_columns] * cognitive_weights)
work_pop14$comprehensive_score <- rowSums(work_pop14[c(non_cognitive_columns, cognitive_columns)] * comprehensive_weights)

panel_14 <- filter(work_pop14, rtype == 1) # panel variable

work_pop14 <- select(work_pop14, -rtype, -non_cognitive_columns, -cognitive_columns)
panel_14 <- select(panel_14, -rtype, -non_cognitive_columns, -cognitive_columns)

# write.csv(work_pop14, "clds14.csv", row.names = FALSE)

#======================= CLDS panel =======================
clds_panel <- bind_rows(panel_16, work_pop14) |>
  group_by(IID) |>
  ungroup() |>
  arrange(IID, year)

clds_panel$IID <- format(clds_panel$IID, scientific=F)
clds_panel$health <- reverse_levels(clds_panel$health)

# filter out outliers
z_scores <- scale(clds_panel$income)
outliers <- clds_panel$income[abs(z_scores) > 3]
print(outliers)
clds_panel <- clds_panel |> filter(!income %in% outliers)

setwd("../Cleaned Data")
# write.csv(clds_panel, "clds_panel.csv", row.names=FALSE)
rm(list=ls())

#======================= AI =======================
setwd("../ML")
company <- read.xlsx("地级市人工智能企业存量（1990-2023）.xlsx")
company <- company[, c(4,3,1,2,6,5)]
colnames(company) <- c("province_cn", "city_cn", "city", "year", "company", "Yangtze")

robot <- read.xlsx("各城市机器人安装密度、存量密度数据（2008-2020）.xlsx")
robot <- robot |> select(3,4,38,39)
colnames(robot) <- c("year", "city_cn", "robot_install", "robot_stock")

ai <- merge(robot, company, by=c("city_cn", "year"), all=T)
ai$city <- ifelse(grepl("^\\d{2}0000$", ai$city),
                  gsub("0000$", "0100", ai$city),
                  ai$city)
ai$city <- as.numeric(ai$city)
ai$city <- ifelse(ai$city==500000, 500100, ai$city)
ai <- arrange(ai, city)
ai <- ai[!(is.na(ai$robot_install) & is.na(ai$company)),]

ne <- c("辽宁省", "吉林省","黑龙江省")
east <- c("北京市", "天津市", "河北省", "上海市", "江苏省", "浙江省", "福建省",
          "山东省","广东省","海南省","台湾省","香港特别行政区","澳门特别行政区")
middle <- c("山西省", "安徽省", "江西省", "河南省", "湖北省", "湖南省")
ai$region <- case_when(ai$province_cn %in% ne ~ "ne",
                       ai$province_cn %in% east ~ "east",
                       ai$province_cn %in% middle ~ "central",
                       TRUE ~ "west")

setwd("../Cleaned Data")
# write.csv(ai, "ai.csv", row.names = FALSE)
rm(list=ls())

#======================= City Controls =======================
setwd("../Controls")

# Industrial Structure %
ind <- read.xlsx("industrial structure.xlsx")
ind <- ind[c("年份", "地区", "第三产业增加值占GDP比重(%)")]
colnames(ind) <- c("year", "city_cn", "city_industry")
ind$city_industry <- ind$city_industry / 100

# Technology & Education Investment
expend <- read.xlsx("city expenditure.xlsx", sheet=2)
tech_edu <- expend[,c(1:3, 9:11)]
colnames(tech_edu) <- c("city_code", "year", "city_cn", "city_invest", "city_tech", "city_edu")
tech_edu$city_tech <- tech_edu$city_tech / tech_edu$city_invest
tech_edu$city_edu <- tech_edu$city_edu / tech_edu$city_invest 

# Merge
city <- merge(tech_edu, ind, by=c("city_cn", "year"), all=T) |> arrange(city_code, city_cn, year)
city$city_code <- ifelse(grepl("^\\d{2}0000$", city$city_code),
                         gsub("0000$", "0100", city$city_code),
                         city$city_code)
city$city_code <- ifelse(city$city_code==500000, 500100, city$city_code)

setwd("../Cleaned Data")
# write.csv(city, "city.csv", row.names = F)

setwd("../")
rm(list=ls())