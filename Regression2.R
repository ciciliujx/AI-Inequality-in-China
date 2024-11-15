# setwd("../Cleaned Data")

library(ineq)

normalize_min_max <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

match <- function(ai, skill) {
  u = 2*sqrt(ai*skill)/(ai+skill)
  v = 0.5*(ai+skill)
  return (sqrt(u*v))
}

panel <- read.csv("panel.csv")

str(panel)

panel$city <- as.factor(panel$city)
panel$industry_type <- as.factor(panel$industry_type)

panel <- panel |>
  mutate(across(c(ln_company, robot, comprehensive_score, cognitive_score, 
                  non_cognitive_score), normalize_min_max))

#===================== company =====================
#===================== company =====================
#===================== company =====================

panel$skill_match <- match(panel$ln_company, panel$comprehensive_score)
panel$cogskill_match <- match(panel$ln_company, panel$cognitive_score)
panel$noncogskill_match <- match(panel$ln_company, panel$non_cognitive_score)

# Raw + no skill
summary(reg1 <- lm(ln_income ~ ln_company + I(ln_company^2) + year + gender + education +
                     + age + I(age^2) + health +
                     + city_industry + city_edu + city_tech, panel))
cluster_se_main <- vcovCL(reg1, cluster = ~city)
coeftest(reg1, vcov = cluster_se_main)

# IV + no skill
reg2sls <- ivreg(ln_income ~ I(ln_company) + I(ln_company^2) + gender + education + year  
                 + age + I(age^2) + health
                 + city_industry + city_edu + city_tech
                 | log(cable_per_area) + I(log(cable_per_area)^2) + gender + education + year 
                 + age + I(age^2) + health
                 + city_industry + city_edu + city_tech, 
                 data = panel)
summary(reg2sls, diagnostics = TRUE)
cluster_se_main <- vcovCL(reg2sls, cluster = ~city)
coeftest(reg2sls, vcov = cluster_se_main)

# IV + cognitive skill
reg2sls <- ivreg(ln_income ~ I(ln_company) + I(ln_company^2) + gender + education + year  
                 + age + I(age^2) + health + cognitive_score + non_cognitive_score +
                 + city_industry + city_edu + city_tech
                 | log(cable_per_area) + I(log(cable_per_area)^2) + gender + education + year 
                 + age + I(age^2) + health + cognitive_score + non_cognitive_score +
                 + city_industry + city_edu + city_tech, 
                 data = panel)
summary(reg2sls, diagnostics = TRUE)
cluster_se_main <- vcovCL(reg2sls, cluster = ~city)
coeftest(reg2sls, vcov = cluster_se_main)

# IV + industry_type
reg2sls <- ivreg(ln_income ~ I(ln_company) + I(ln_company^2) + gender + education + year  
                 + age + I(age^2) + health+ industry_type +
                 + city_industry + city_edu + city_tech
                 | log(cable_per_area) + I(log(cable_per_area)^2) + gender + education + year 
                 + age + I(age^2) + health+ industry_type +
                 + city_industry + city_edu + city_tech, 
                 data = panel)
summary(reg2sls, diagnostics = TRUE)
cluster_se_main <- vcovCL(reg2sls, cluster = ~city)
coeftest(reg2sls, vcov = cluster_se_main)

# =================== AI:skill match =================== 
# comprehensive score==================
summary(match1 <- lm(skill_match ~ ln_company + I(ln_company^2) +
                       year + gender + education + age + I(age^2) + health + 
                       comprehensive_score + city_industry + 
                       city_edu + city_tech, panel))
# panel$match_residuals <- residuals(match1)
panel$match_com <- fitted(match1)
cluster_se_01 <- vcovCL(match1, cluster = ~city)
coeftest(match1, vcov = cluster_se_01)
summary(reg1 <- lm(log(income) ~ match_com + ln_company + I(ln_company^2) +
                     year + gender + education + age + I(age^2) + health + 
                     city_industry + city_edu + city_tech, panel))
cluster_se_1 <- vcovCL(reg1, cluster = ~city)
coeftest(reg1, vcov = cluster_se_1)
# vif(reg1)
# cognitive skill==================
summary(match2 <- lm(cogskill_match ~ ln_company + I(ln_company^2) + 
                       year + gender + education + age + I(age^2) + health + 
                       cognitive_score + city_industry + 
                       city_edu + city_tech, panel))
panel$match_cog <- fitted(match2)
cluster_se_02 <- vcovCL(match2, cluster = ~city)
coeftest(match2, vcov = cluster_se_02)
summary(reg2 <- lm(log(income) ~ match_cog + ln_company + I(ln_company^2) + 
                     year + gender + education + age + I(age^2) + health + 
                    city_industry + 
                     city_edu + city_tech, panel))
cluster_se_2 <- vcovCL(reg2, cluster = ~city)
coeftest(reg2, vcov = cluster_se_2)
# bptest(reg2)
# Non-cognitive skill==================
summary(match3 <- lm(noncogskill_match ~ ln_company + I(ln_company^2) + 
                       year + gender + education + age + I(age^2) + health + 
                       non_cognitive_score + city_industry + 
                       city_edu + city_tech, panel))
panel$match_noncog <- fitted(match3)
cluster_se_03 <- vcovCL(match3, cluster = ~city)
coeftest(match3, vcov = cluster_se_03)
summary(reg3 <- lm(log(income) ~ match_noncog + ln_company + I(ln_company^2) + 
                     year + gender + education + age + I(age^2) + health + 
                      city_industry + 
                     city_edu + city_tech, panel))
cluster_se_3 <- vcovCL(reg3, cluster = ~city)
coeftest(reg3, vcov = cluster_se_3)
# coeftest(reg3, vcov = vcovHC(reg, type = "HC1"))
# Ramsey RESET test
resettest(reg3, power=2, type="fitted")
bptest(reg3)
# Industry==================
prime_pop <- filter(panel, industry_type ==1)
second_pop <- filter(panel, industry_type ==2)
tert_pop <- filter(panel, industry_type ==3)

# prime
summary(match_ind1 <- lm(skill_match ~ ln_company + I(ln_company^2) + 
                           year + gender + education + age + I(age^2) + health + 
                           comprehensive_score + city_industry + 
                           city_edu + city_tech, prime_pop))
prime_pop$match_com <- fitted(match_ind1)
summary(ind1 <- lm(log(income) ~ match_com + ln_company + I(ln_company^2) + 
                     year + gender + education + age + I(age^2) + health + 
                     city_industry + 
                     city_edu + city_tech, prime_pop))
cluster_se_main <- vcovCL(ind1, cluster = ~city)
coeftest(ind1, vcov = cluster_se_main)
#cognitive
summary(match_ind1_cog <- lm(cogskill_match ~ ln_company + I(ln_company^2) + 
                           year + gender + education + age + I(age^2) + health + 
                           cognitive_score + city_industry + 
                           city_edu + city_tech, prime_pop))
cluster_se_main <- vcovCL(match_ind1_cog, cluster = ~city)
coeftest(match_ind1_cog, vcov = cluster_se_main)
prime_pop$match_cog <- fitted(match_ind1_cog)
summary(ind1 <- lm(log(income) ~ match_cog + ln_company + I(ln_company^2) + 
                     year + gender + education + age + I(age^2) + health + 
                     city_industry + 
                     city_edu + city_tech, prime_pop))
cluster_se_main <- vcovCL(ind1, cluster = ~city)
coeftest(ind1, vcov = cluster_se_main)
#non-cognitive
summary(match_ind1_noncog <- lm(noncogskill_match ~ ln_company + I(ln_company^2) + 
                           year + gender + education + age + I(age^2) + health + 
                           non_cognitive_score + city_industry + 
                           city_edu + city_tech, prime_pop))
cluster_se_main <- vcovCL(match_ind1_noncog, cluster = ~city)
coeftest(match_ind1_noncog, vcov = cluster_se_main)
prime_pop$match_noncog <- fitted(match_ind1_noncog)
summary(ind1 <- lm(log(income) ~ match_noncog + ln_company + I(ln_company^2) + 
                     year + gender + education + age + I(age^2) + health + 
                     city_industry + 
                     city_edu + city_tech, prime_pop))
cluster_se_main <- vcovCL(ind1, cluster = ~city)
coeftest(ind1, vcov = cluster_se_main)

#secondary
summary(match_ind2 <- lm(skill_match ~ ln_company + I(ln_company^2) + 
                           year + gender + education + age + I(age^2) + health + 
                           comprehensive_score + city_industry + 
                           city_edu + city_tech, second_pop))
second_pop$match_com <- fitted(match_ind2)
summary(ind2 <- lm(log(income) ~ match_com + ln_company + I(ln_company^2) + 
                     year + gender + education + age + I(age^2) + health + 
                      + city_industry + 
                     city_edu + city_tech, second_pop))
cluster_se_main <- vcovCL(ind2, cluster = ~city)
coeftest(ind2, vcov = cluster_se_main)
#cognitive
summary(match_ind2_cog <- lm(cogskill_match ~ ln_company + I(ln_company^2) + 
                               year + gender + education + age + I(age^2) + health + 
                               cognitive_score + city_industry + 
                               city_edu + city_tech, second_pop))
cluster_se_main <- vcovCL(match_ind2_cog, cluster = ~city)
coeftest(match_ind2_cog, vcov = cluster_se_main)
second_pop$match_cog <- fitted(match_ind2_cog)
summary(ind2 <- lm(log(income) ~ match_cog + ln_company + I(ln_company^2) + 
                     year + gender + education + age + I(age^2) + health + 
                     city_industry + 
                     city_edu + city_tech, second_pop))
cluster_se_main <- vcovCL(ind2, cluster = ~city)
coeftest(ind2, vcov = cluster_se_main)
#non-cognitive
summary(match_ind2_noncog <- lm(noncogskill_match ~ ln_company + I(ln_company^2) + 
                                  year + gender + education + age + I(age^2) + health + 
                                  non_cognitive_score + city_industry + 
                                  city_edu + city_tech, second_pop))
second_pop$match_noncog <- fitted(match_ind2_noncog)
summary(ind2 <- lm(log(income) ~ match_noncog + ln_company + I(ln_company^2) + 
                     year + gender + education + age + I(age^2) + health + 
                     city_industry + 
                     city_edu + city_tech, second_pop))
cluster_se_main <- vcovCL(ind2, cluster = ~city)
coeftest(ind2, vcov = cluster_se_main)

#tertiary
summary(match_ind3 <- lm(skill_match ~ ln_company + I(ln_company^2) + 
                           year + gender + education + age + I(age^2) + health + 
                           comprehensive_score + city_industry + 
                           city_edu + city_tech, tert_pop))
tert_pop$match_com <- fitted(match_ind3)
summary(ind3 <- lm(log(income) ~ match_com + ln_company + I(ln_company^2) + 
                     year + gender + education + age + I(age^2) + health + 
                      + city_industry + 
                     city_edu + city_tech, tert_pop))
cluster_se_main <- vcovCL(ind3, cluster = ~city)
coeftest(ind3, vcov = cluster_se_main)
#cognitive
summary(match_ind3_cog <- lm(cogskill_match ~ ln_company + I(ln_company^2) + 
                               year + gender + education + age + I(age^2) + health + 
                               cognitive_score + city_industry + 
                               city_edu + city_tech, tert_pop))
cluster_se_main <- vcovCL(match_ind3_cog, cluster = ~city)
coeftest(match_ind3_cog, vcov = cluster_se_main)
tert_pop$match_cog <- fitted(match_ind3_cog)
summary(ind3 <- lm(log(income) ~ match_cog + ln_company + I(ln_company^2) + 
                     year + gender + education + age + I(age^2) + health + 
                     city_industry + 
                     city_edu + city_tech, tert_pop))
cluster_se_main <- vcovCL(ind3, cluster = ~city)
coeftest(ind3, vcov = cluster_se_main)
#non-cognitive
summary(match_ind2_noncog <- lm(noncogskill_match ~ ln_company + I(ln_company^2) + 
                                  year + gender + education + age + I(age^2) + health + 
                                  non_cognitive_score + city_industry + 
                                  city_edu + city_tech, tert_pop))
tert_pop$match_noncog <- fitted(match_ind2_noncog)
summary(ind3 <- lm(log(income) ~ match_noncog + ln_company + I(ln_company^2) + 
                     year + gender + education + age + I(age^2) + health + 
                     city_industry + 
                     city_edu + city_tech, tert_pop))
cluster_se_main <- vcovCL(ind3, cluster = ~city)
coeftest(ind2, vcov = cluster_se_main)

# Education==================
panel$education_bin <- case_when(panel$education %in% c(0:6) ~ "low",
                                      panel$education %in% c(7:12) ~ "medium",
                                      panel$education > 12 ~ "high")
low_edu <- filter(panel, education_bin == "low")
second_edu <- filter(panel, education_bin == "medium")
high_edu <- filter(panel, education_bin == "high")

# low---------------------
#cognitive
summary(match_low_cog <- lm(cogskill_match ~ ln_company + I(ln_company^2) + 
                               year + gender + education + age + I(age^2) + health + 
                               cognitive_score + city_industry + 
                               city_edu + city_tech, low_edu))
low_edu$match_cog <- fitted(match_low_cog)
summary(edu_low_cog <- lm(log(income) ~ match_cog + ln_company + I(ln_company^2) + 
                     year + gender + education + age + I(age^2) + health + 
                     city_industry + 
                     city_edu + city_tech, low_edu))
cluster_se_main <- vcovCL(edu_low_cog, cluster = ~city)
coeftest(edu_low_cog, vcov = cluster_se_main)
#non-cognitive
summary(match_low_noncog <- lm(noncogskill_match ~ ln_company + I(ln_company^2) + 
                                  year + gender + education + age + I(age^2) + health + 
                                  non_cognitive_score + city_industry + 
                                  city_edu + city_tech, low_edu))
low_edu$match_noncog <- fitted(match_low_noncog)
summary(edu_low_noncog <- lm(log(income) ~ match_noncog + ln_company + I(ln_company^2) + 
                     year + gender + education + age + I(age^2) + health + 
                     city_industry + 
                     city_edu + city_tech, low_edu))
cluster_se_main <- vcovCL(edu_low_noncog, cluster = ~city)
coeftest(edu_low_noncog, vcov = cluster_se_main)

# secondary---------------------
#cognitive
summary(match_second_cog <- lm(cogskill_match ~ ln_company + I(ln_company^2) + 
                              year + gender + education + age + I(age^2) + health + 
                              cognitive_score + city_industry + 
                              city_edu + city_tech, second_edu))
second_edu$match_cog <- fitted(match_second_cog)
summary(edu_second_cog <- lm(log(income) ~ match_cog + ln_company + I(ln_company^2) + 
                            year + gender + education + age + I(age^2) + health + 
                            city_industry + 
                            city_edu + city_tech, second_edu))
cluster_se_main <- vcovCL(edu_second_cog, cluster = ~city)
coeftest(edu_second_cog, vcov = cluster_se_main)
#non-cognitive
summary(match_second_noncog <- lm(noncogskill_match ~ ln_company + I(ln_company^2) + 
                                 year + gender + education + age + I(age^2) + health + 
                                 non_cognitive_score + city_industry + 
                                 city_edu + city_tech, second_edu))
second_edu$match_noncog <- fitted(match_second_noncog)
summary(edu_second_noncog <- lm(log(income) ~ match_noncog + ln_company + I(ln_company^2) + 
                               year + gender + education + age + I(age^2) + health + 
                               city_industry + 
                               city_edu + city_tech, second_edu))
cluster_se_main <- vcovCL(edu_second_noncog, cluster = ~city)
coeftest(edu_second_noncog, vcov = cluster_se_main)

# High---------------------
#cognitive
summary(match_high_cog <- lm(cogskill_match ~ ln_company + I(ln_company^2) + 
                              year + gender + education + age + I(age^2) + health + 
                              cognitive_score + city_industry + 
                              city_edu + city_tech, high_edu))
high_edu$match_cog <- fitted(match_high_cog)
cluster_se_02 <- vcovCL(match_high_cog, cluster = ~city)
coeftest(match_high_cog, vcov = cluster_se_02)
summary(edu_high_cog <- lm(log(income) ~ match_cog + ln_company + I(ln_company^2) + 
                            year + gender + education + age + I(age^2) + health + 
                            city_industry + 
                            city_edu + city_tech, high_edu))
cluster_se_main <- vcovCL(edu_high_cog, cluster = ~city)
coeftest(edu_high_cog, vcov = cluster_se_main)
#non-cognitive
summary(match_high_noncog <- lm(noncogskill_match ~ ln_company + I(ln_company^2) + 
                                 year + gender + education + age + I(age^2) + health + 
                                 non_cognitive_score + city_industry + 
                                 city_edu + city_tech, high_edu))
high_edu$match_noncog <- fitted(match_high_noncog)
summary(edu_high_noncog <- lm(log(income) ~ match_noncog + ln_company + I(ln_company^2) + 
                               year + gender + education + age + I(age^2) + health + 
                               city_industry + 
                               city_edu + city_tech, high_edu))
cluster_se_main <- vcovCL(edu_high_noncog, cluster = ~city)
coeftest(edu_high_noncog, vcov = cluster_se_main)

#===================== robot =====================
#===================== robot =====================
#===================== robot =====================

#===================== regression =====================
panel$skill_match <- match(panel$robot, panel$comprehensive_score)
panel$cogskill_match <- match(panel$robot, panel$cognitive_score)
panel$noncogskill_match <- match(panel$robot, panel$non_cognitive_score)
panel$skill_match_iv <- match(log(panel$cable_per_area+1), panel$comprehensive_score)
panel$cogskill_match_iv <- match(log(panel$cable_per_area+1), panel$cognitive_score)
panel$noncogskill_match_iv <- match(log(panel$cable_per_area+1), panel$non_cognitive_score)

# Raw, IV, no skill
summary(reg1 <- lm(log(income) ~ robot + I(robot^2) + year + gender + education
                   + age + I(age^2) + health
                   + city_industry + city_edu + city_tech, panel))
cluster_se_main <- vcovCL(reg1, cluster = ~city)
coeftest(reg1, vcov = cluster_se_main)

reg2sls <- ivreg(log(income) ~ I(robot) + I(robot^2) + gender + education + year 
                 + age + I(age^2) + health + #cognitive_score + non_cognitive_score
                 + city_industry + city_edu + city_tech #+ log(infra)
                 #+ cognitive_score:robot + non_cognitive_score:robot
                 | log(cable_per_area) + I(log(cable_per_area)^2) + gender + education + year
                 + age + I(age^2) + health + #cognitive_score + non_cognitive_score
                 + city_industry + city_edu + city_tech, #+ log(infra)
                 #+ cognitive_score:log(cable_per_area) + non_cognitive_score:log(cable_per_area), 
                 data = panel)
summary(reg2sls, diagnostics = TRUE)
cluster_se_main <- vcovCL(reg2sls, cluster = ~city)
coeftest(reg2sls, vcov = cluster_se_main)

# IV: robot
reg2sls <- ivreg(log(income) ~ I(robot) + I(robot^2) + gender + education + year 
                 + age + I(age^2) + health + cognitive_score + non_cognitive_score
                 + city_industry + city_edu + city_tech #+ log(infra)
                 + cognitive_score:robot + non_cognitive_score:robot
                 | log(cable_per_area) + I(log(cable_per_area)^2) + gender + education + year
                 + age + I(age^2) + health + cognitive_score + non_cognitive_score
                 + city_industry + city_edu + city_tech #+ log(infra)
                 + cognitive_score:log(cable_per_area) + non_cognitive_score:log(cable_per_area), 
                 data = panel)
summary(reg2sls, diagnostics = TRUE)
cluster_se_main <- vcovCL(reg2sls, cluster = ~city)
coeftest(reg2sls, vcov = cluster_se_main)

# Raw + skill match
summary(reg1 <- lm(log(income) ~ robot + I(robot^2) + year + gender + education
                     + age + I(age^2) + health + cognitive_score + non_cognitive_score 
                     + city_industry + city_edu + city_tech 
                   + cognitive_score:robot + non_cognitive_score:robot, panel))
summary(reg1 <- lm(log(income) ~ robot + I(robot^2) + year + gender + education
                   + age + I(age^2) + health #+ cognitive_score + non_cognitive_score 
                   + city_industry + city_edu + city_tech 
                   + skill_match, panel))
cluster_se_main <- vcovCL(reg1, cluster = ~city)
coeftest(reg1, vcov = cluster_se_main)
vif(reg1)

# + cognitive skill match
summary(reg1 <- lm(log(income) ~ robot + I(robot^2) + year + gender + education
                   + age + I(age^2) + health #+ cognitive_score + non_cognitive_score 
                   + city_industry + city_edu + city_tech 
                   + cogskill_match, panel))
cluster_se_main <- vcovCL(reg1, cluster = ~city)
coeftest(reg1, vcov = cluster_se_main)
vif(reg1)

# + noncognitive skill match
summary(reg1 <- lm(log(income) ~ robot + I(robot^2) + year + gender + education
                   + age + I(age^2) + health #+ cognitive_score + non_cognitive_score 
                   + city_industry + city_edu + city_tech 
                   + noncogskill_match, panel))
cluster_se_main <- vcovCL(reg1, cluster = ~city)
coeftest(reg1, vcov = cluster_se_main)
vif(reg1)

# summary(reg1 <- lm(income ~ robot_stock + I(robot_stock^2) + year + gender + education
#                    + age + I(age^2) + health + cognitive_score + non_cognitive_score 
#                    + city_industry + city_edu + city_tech
#                    + cognitive_score:robot_stock + non_cognitive_score:robot_stock, panel))
# cluster_se_main <- vcovCL(reg1, cluster = ~city_cn)
# coeftest(reg1, vcov = cluster_se_main)

# IV: robot stock
reg2sls <- ivreg(log(income) ~ robot_stock + I(robot_stock^2) + gender + education + year +#industry_type
                   + age + I(age^2) + health + cognitive_score + non_cognitive_score
                 + city_industry + city_edu + city_tech #+ log(infra)
                 + cognitive_score:robot_stock + non_cognitive_score:robot_stock
                 | log(cable_per_area) + I(log(cable_per_area)^2) + gender + education + year +#industry_type
                   + age + I(age^2) + health + cognitive_score + non_cognitive_score
                 + city_industry + city_edu + city_tech #+ log(infra)
                 + cognitive_score:log(cable_per_area) + non_cognitive_score:log(cable_per_area), 
                 data = panel)
summary(reg2sls, diagnostics = TRUE)
cluster_se_main <- vcovCL(reg2sls, cluster = ~city)
coeftest(reg2sls, vcov = cluster_se_main)

# =================== AI:skill match =================== 

# comprehensive
summary(match1 <- lm(skill_match ~ robot + I(robot^2) + 
                       year + gender + education + age + I(age^2) + health + 
                       comprehensive_score + city_industry + 
                       city_edu + city_tech, panel))
panel$match_com <- fitted(match1)
summary(reg1 <- lm(log(income) ~ match_com + robot + I(robot^2) + 
          year + gender + education + age + I(age^2) + health + 
          city_industry + 
          city_edu + city_tech, panel))
cluster_se_main <- vcovCL(reg1, cluster = ~city)
coeftest(reg1, vcov = cluster_se_main)

vif(reg1)
# cognitive skill==================
summary(match2 <- lm(cogskill_match ~ robot + I(robot^2) + 
                       year + gender + education + age + I(age^2) + health + 
                       comprehensive_score + city_industry + 
                       city_edu + city_tech, panel))
panel$match_cog <- fitted(match2)
str(panel)
summary(reg2 <- lm(log(income) ~ cogskill_match + robot + I(robot^2) + 
                     year + gender + education + age + I(age^2) + health + 
                     comprehensive_score + city_industry + 
                     city_edu + city_tech, panel))
cluster_se_main <- vcovCL(reg2, cluster = ~city_cn)
coeftest(reg2, vcov = cluster_se_main)

# Non-cognitive skill==================
summary(match3 <- lm(noncogskill_match ~ robot + I(robot^2) + 
                       year + gender + education + age + I(age^2) + health + 
                       comprehensive_score + city_industry + 
                       city_edu + city_tech, panel))
panel$match_noncog <- fitted(match3)
str(panel)
summary(reg3 <- lm(log(income) ~ noncogskill_match + robot + I(robot^2) + 
                     year + gender + education + age + I(age^2) + health + 
                     comprehensive_score + city_industry + 
                     city_edu + city_tech, panel))
cluster_se_main <- vcovCL(reg3, cluster = ~city)
coeftest(reg3, vcov = cluster_se_main)

# Industry==================
prime_pop <- filter(panel, industry_type ==1)
second_pop <- filter(panel, industry_type ==2)
tert_pop <- filter(panel, industry_type ==3)

# prime
summary(match_ind1 <- lm(skill_match ~ robot + I(robot^2) + 
                       year + gender + education + age + I(age^2) + health + 
                       comprehensive_score + city_industry + 
                       city_edu + city_tech, prime_pop))
prime_pop$match_com <- fitted(match_ind1)
summary(ind1 <- lm(log(income) ~ match_com + robot + I(robot^2) + 
                     year + gender + education + age + I(age^2) + health + 
                     comprehensive_score + city_industry + 
                     city_edu + city_tech, prime_pop))
cluster_se_main <- vcovCL(ind1, cluster = ~city)
coeftest(ind1, vcov = cluster_se_main)
#secondary
summary(match_ind2 <- lm(skill_match ~ robot + I(robot^2) + 
                           year + gender + education + age + I(age^2) + health + 
                           comprehensive_score + city_industry + 
                           city_edu + city_tech, second_pop))
second_pop$match_com <- fitted(match_ind2)
summary(ind2 <- lm(log(income) ~ match_com + robot + I(robot^2) + 
                     year + gender + education + age + I(age^2) + health + 
                     comprehensive_score + city_industry + 
                     city_edu + city_tech, second_pop))
cluster_se_main <- vcovCL(ind2, cluster = ~city)
coeftest(ind2, vcov = cluster_se_main)
#tertiary
summary(match_ind3 <- lm(skill_match ~ robot + I(robot^2) + 
                           year + gender + education + age + I(age^2) + health + 
                           comprehensive_score + city_industry + 
                           city_edu + city_tech, tert_pop))
tert_pop$match_com <- fitted(match_ind3)
summary(ind3 <- lm(log(income) ~ match_com + robot + I(robot^2) + 
                     year + gender + education + age + I(age^2) + health + 
                     comprehensive_score + city_industry + 
                     city_edu + city_tech, tert_pop))
cluster_se_main <- vcovCL(ind3, cluster = ~city)
coeftest(ind3, vcov = cluster_se_main)

# Region==================
east_panel <- filter(panel, region=="east")
west_panel <- filter(panel, region=="west")
ne_panel <- filter(panel, region=="ne")
central_panel <- filter(panel, region=="central")

#east
summary(match_east <- lm(skill_match ~ robot + I(robot^2) + 
                           year + gender + education + age + I(age^2) + health + 
                           comprehensive_score + city_industry + 
                           city_edu + city_tech, east_panel))
east_panel$match_com <- fitted(match_east)
summary(east <- lm(log(income) ~ match_com + robot + I(robot^2) + 
                     year + gender + education + age + I(age^2) + health + 
                     comprehensive_score + city_industry + 
                     city_edu + city_tech, east_panel))
cluster_se_main <- vcovCL(east, cluster = ~city)
coeftest(east, vcov = cluster_se_main)

#west
summary(match_west <- lm(skill_match ~ robot + I(robot^2) + 
                           year + gender + education + age + I(age^2) + health + 
                           comprehensive_score + city_industry + 
                           city_edu + city_tech, west_panel))
west_panel$match_com <- fitted(match_west)
summary(west <- lm(log(income) ~ match_com + robot + I(robot^2) + 
                     year + gender + education + age + I(age^2) + health + 
                     comprehensive_score + city_industry + 
                     city_edu + city_tech, west_panel))
cluster_se_main <- vcovCL(west, cluster = ~city)
coeftest(west, vcov = cluster_se_main)

#ne
summary(match_ne <- lm(skill_match ~ robot + I(robot^2) + 
                           year + gender + education + age + I(age^2) + health + 
                           comprehensive_score + city_industry + 
                           city_edu + city_tech, ne_panel))
ne_panel$match_com <- fitted(match_ne)
summary(ne <- lm(log(income) ~ match_com + robot + I(robot^2) + 
                     year + gender + education + age + I(age^2) + health + 
                     comprehensive_score + city_industry + 
                     city_edu + city_tech, ne_panel))
cluster_se_main <- vcovCL(ne, cluster = ~city)
coeftest(ne, vcov = cluster_se_main)

#central
summary(match_central <- lm(skill_match ~ robot + I(robot^2) + 
                         year + gender + education + age + I(age^2) + health + 
                         comprehensive_score + city_industry + 
                         city_edu + city_tech, central_panel))
central_panel$match_com <- fitted(match_central)
summary(central <- lm(log(income) ~ match_com + robot + I(robot^2) + 
                   year + gender + education + age + I(age^2) + health + 
                   comprehensive_score + city_industry + 
                   city_edu + city_tech, central_panel))
cluster_se_main <- vcovCL(central, cluster = ~city)
coeftest(central, vcov = cluster_se_main)

#===================== gini =====================
city_gini <- panel |> group_by(city, year) |> summarize(gini = ineq(income, type = "Gini"))
panel_gini <- left_join(panel, city_gini, by = c("city", "year"))
panel_gini <- panel_gini |> select(city, gini, year, infra, ln_gdppc, city_tech, city_edu, city_industry,
                                   ln_company)
panel_gini <- unique(panel_gini) |> arrange(city, year)
panel_gini$infra <- log(panel_gini$infra)
summary(lm.model <- lm(gini ~ . + I(ln_company^2) + I(ln_gdppc^2) - city, panel_gini))

# ===================== EDA =====================

# by region
# ai_region_year <- robot |> group_by(year, city_cn) |> summarize(robot = sum(robot_install, na.rm=T))
# ai_region_year <- ai_region_year |>
#   arrange(region, year) |>
#   group_by(region) |> 
#   mutate(
#     lag_company = dplyr::lag(company),
#     growth_rate = (round(company / lag_company, 4) - 1) * 100 
#   ) |>
#   ungroup()
# 
# ai_trend <- ggplot(ai_region_year, aes(year, robot, color=city_cn)) +
#   geom_line() +
#   geom_point() +
#   labs(x = "Year",
#        y = "AI Companies") +
#   scale_color_manual(values = c("ne" = "orange", "east" = "darkblue", "central" = "#d62728",
#                                 "west" = "darkgreen"),
#                      labels = c("ne" = "North East", "east" = "Eastern",
#                                 "central" = "Central", "west" = "Western")) +
#   scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
#   theme_minimal() + 
#   theme(panel.grid.major = element_blank(),                        
#         panel.grid.minor = element_blank(), 
#         panel.grid.major.y = element_line(color = "grey80"),
#         axis.ticks = element_line(color = "black"),
#         axis.ticks.length = unit(0.15, "cm"),
#         legend.position = c(0.15, 0.8),
#         axis.line = element_line(color = "black"),
#         legend.background = element_rect(fill = "white", color = "black"))
# ai_trend
