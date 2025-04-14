library(readxl)
library(dplyr)
library(stringr)


getwd()
setwd("/Users/hawatoumbou/Documents/GitHub/ht_portfolio_work")
projects <- read_excel("data/Clean_Project.xlsx")
govtech <- read_excel("data/Clean_GovTech.xlsx")

#GTMI 2022 Data 
gtmi_2022 <- govtech %>%
  filter(Year == 2022) %>%
  select(Country = Economy, GTMI, Grp) %>%
  mutate(Country = str_trim(Country))
gtmi_2022


convert_rating <- function(x) {
  case_when(
    x == "HS" ~ 4,
    x == "S"  ~ 3,
    x == "MS" ~ 2,
    x == "MU" ~ 1,
    x == "U"  ~ 0,
    TRUE ~ NA_real_
  )
}

projects_clean <- projects %>%
  mutate(
    IEG_num = convert_rating(`IEG Out`),
    ICR_num = convert_rating(`ICR Out`),
    Country = str_trim(Country)
  )

projects_clean <- projects_clean %>%
  mutate(
    Outcome_Score = rowMeans(select(., IEG_num, ICR_num), na.rm = TRUE)
  ) %>%
  filter(!is.na(Country), !is.na(`ICT Cost ($m)`))


merged_data <- left_join(projects_clean, gtmi_2022, by = "Country")

summary(merged_data$GTMI)
summary(merged_data$Outcome_Score)


library(ggplot2)

ggplot(merged_data, aes(x = GTMI, y = Outcome_Score)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  labs(title = "GTMI (2022) vs. World Bank Project Outcome Score",
       x = "GovTech Maturity Index (2022)",
       y = "Average Outcome Score (IEG + ICR)")

#Trend Positive but weak

model <- lm(Outcome_Score ~ GTMI + `ICT Cost ($m)` + `Tot WB ($m)` + Region, data = merged_data)
summary(model)

# ICT Cost is significant as it suggest the higher ICT investments are associated with slightly better outcomes


model_interaction <- lm(Outcome_Score ~ GTMI * `ICT Cost ($m)` + `Tot WB ($m)` + Region, data = merged_data)
summary(model_interaction)


#Average Outcome Score by Group:
merged_data %>%
  filter(!is.na(Grp)) %>%
  group_by(Grp) %>%
  summarise(
    Avg_Outcome = mean(Outcome_Score, na.rm = TRUE),
    Avg_ICT = mean(`ICT Cost ($m)`, na.rm = TRUE),
    Count = n()
  )
#Boxplot by GTMI Group: 
ggplot(merged_data, aes(x = Grp, y = Outcome_Score, fill = Grp)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.color = "black") +
  labs(title = "World Bank Project Outcomes by GTMI Group (2022)",
       x = "GTMI Group (A = High, C = Low)",
       y = "Project Outcome Score (Avg of IEG + ICR)") +
  theme_minimal()

merged_data1 <- merged_data %>%
  mutate(
    Outcome_Label = case_when(
      Outcome_Score >= 3 ~ "High Success",
      Outcome_Score < 2 ~ "Low Success",
      TRUE ~ "Medium"
    ),
    GTMI_Level = case_when(
      GTMI >= 0.7 ~ "High GTMI",
      GTMI < 0.4 ~ "Low GTMI",
      TRUE ~ "Medium GTMI"
    )
  )
table(merged_data1$GTMI_Level, merged_data$Outcome_Label)

anova_model <- aov(Outcome_Score ~ Grp, data = merged_data)
summary(anova_model)
#An ANOVA test revealed statistically significant differences in project outcome scores across GTMI groups (F(3,928) = 4.59, p = 0.00338). This suggests that GovTech maturity classification (A, B, C) is meaningfully associated with variation in public project success.


merged_data <- merged_data %>%
  mutate(Outcome_per_Million = Outcome_Score / `ICT Cost ($m)`)
merged_data

merged_data <- merged_data %>%
  mutate(Project_At_Risk = ifelse(Outcome_Score < 2, 1, 0))
 

##Structural Barriers Analysis

#HIGH- GTMI, Low- Success Countries
high_gtmi_low_outcome <- merged_data %>%
  filter(GTMI >= 0.7, Outcome_Score < 2) %>%
  select(Country, GTMI, Outcome_Score, `ICT Cost ($m)`, Region, Practice)
high_gtmi_low_outcome


high_gtmi <- merged_data %>%
  filter(GTMI >= 0.7)

high_gtmi <- high_gtmi %>%
  mutate(Success_Group = case_when(
    Outcome_Score >= 3 ~ "High Outcome",
    Outcome_Score < 2 ~ "Low Outcome",
    TRUE ~ "Medium Outcome"
  ))

binary_cols <- c("Gov Sys", "e-Serv", "Open Gov", "ID4D", "DT", "Enablers")

comparison_table <- high_gtmi %>%
  group_by(Success_Group) %>%
  summarise(across(all_of(binary_cols), ~ mean(. == "Yes", na.rm = TRUE)))


high_gtmi %>%
  summarise(across(all_of(binary_cols), ~ mean(. == "Yes", na.rm = TRUE)))

merged_data %>%
  summarise(across(all_of(binary_cols), ~ mean(. == "Yes", na.rm = TRUE)))

comparison_table_long_clean <- comparison_table_long %>%
  filter(!is.na(Usage_Rate)) %>%
  mutate(Usage_Percent = round(Usage_Rate * 100, 1))

high_gtmi_low_outcome %>%
  count(Country, sort = TRUE)


#Risk Matrix 
portfolio_risk <- merged_data %>%
  group_by(Country) %>%
  summarise(
    Avg_GTMI = mean(GTMI, na.rm = TRUE),
    Avg_Outcome = mean(Outcome_Score, na.rm = TRUE),
    Projects = n()
  ) %>%
  filter(!is.na(Avg_GTMI), !is.na(Avg_Outcome)) %>% 
  mutate(
    Tier = case_when(
      Avg_GTMI >= 0.7 & Avg_Outcome >= 2.5 ~ "Scale Investment",
      Avg_GTMI >= 0.7 & Avg_Outcome < 2   ~ "Reform First",
      Avg_GTMI < 0.4 & Avg_Outcome >= 2.5 ~ "Innovate/Test",
      Avg_Outcome < 2                     ~ "Pause Investment",
      TRUE                                ~ "Review Further"
    )
  )

portfolio_risk

ggplot(portfolio_risk, aes(x = Avg_GTMI, y = Avg_Outcome, color = Tier, size = Projects)) +
  geom_point(alpha = 0.8) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0.7, linetype = "dashed", color = "gray") +
  scale_size_continuous(range = c(3, 10)) +
  labs(title = "Portfolio Risk Matrix: GTMI vs. Project Outcome",
       subtitle = "Countries grouped by recommended investment strategy",
       x = "Average GTMI (2022)",
       y = "Average Project Outcome Score",
       color = "Investment Tier",
       size = "Number of Projects") +
  theme_minimal() +
  theme(legend.position = "bottom")

#The Portfolio Risk Matrix reveals that while countries with high digital maturity (GTMI ≥ 0.7) tend to perform better, several digitally mature countries exhibit weak project outcomes — suggesting a structural misalignment between digital readiness and reform implementation. 
#These countries, such as [insert examples], should be prioritized for design reform, not just additional investment.



library(ggrepel)

ggplot(portfolio_risk, aes(x = Avg_GTMI, y = Avg_Outcome, color = Tier, size = Projects)) +
  geom_point(alpha = 0.8) +
  geom_text_repel(
    aes(label = Country),
    size = 2,
    max.overlaps = 25,       
    min.segment.length = 0.3,
    box.padding = 0.4,
    point.padding = 0.2,
    segment.color = "gray80"
  ) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = 0.7, linetype = "dashed", color = "gray60") +
  scale_size_continuous(range = c(3, 10)) +
  labs(
    title = "Portfolio Risk Matrix: GTMI vs. Project Outcome",
    subtitle = "Countries grouped by recommended investment strategy",
    x = "Average GTMI (2022)",
    y = "Average Project Outcome Score",
    color = "Investment Tier",
    size = "Number of Projects"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

portfolio_risk <- portfolio_risk %>%
  left_join(desi_summary, by = "Country") %>%
  mutate(Weak_Area = case_when(
    DESI_AI < 0.3 ~ "Low AI Adoption",
    DESI_Cloud < 0.3 ~ "Low Cloud Use",
    DESI_eGov < 0.3 ~ "eGov Weakness",
    TRUE ~ "None"
  ))


