if (!require("pacman")) install.packages("pacman")
pacman::p_load(here,
               readr, readxl, janitor, forcats, stringr,
               dplyr,
               install = TRUE,
               update = FALSE)

data.path <- file.path(here(), "1_data")

data <-
  read_excel(
    file.path(data.path, "unemployment-inactivity-hiring.xlsx")
  ) %>%
  clean_names() %>%
  mutate(
    callback_count_non_working = round(callback_count_non_working, 0),
    callback_count_working = round(callback_count_working, 0),
    unemployment_duration_months_sq = unemployment_duration_months^2,
    unemployment_duration_category =
      case_when(unemployment_duration_months <= 6 ~ "1 to 6 months",
                unemployment_duration_months >= 7 & unemployment_duration_months <= 12 ~ "7 to 12 months",
                unemployment_duration_months >= 13 & unemployment_duration_months <= 18 ~ "13 to 18 months",
                unemployment_duration_months >= 19 & unemployment_duration_months <= 36 ~ "19 to 36 months",
                TRUE ~ "Other") %>%
      factor() %>%
      fct_relevel(c("1 to 6 months", "7 to 12 months", "13 to 18 months", "19 to 36 months")),
    region = factor(region,
                    levels = c(
                      "America (Northern)",
                      "Europe (Western)",
                      "Europe (Northern)",
                      "Europe (Central)",
                      "Asia (Eastern)"
                    )),
    region_agg =
      case_when(str_detect(region, "Europe") ~ "Europe",
                str_detect(region, "America") ~ "America",
                str_detect(region, "Asia") ~ "Asia",
                TRUE ~ "Other") %>%
      factor() %>%
      fct_relevel(c("America", "Europe", "Asia")),
    country = factor(country) %>%
      fct_relevel("USA"),
    gender = factor(gender,
                    levels = c(
                      "Male",
                      "Female",
                      "Mixed"
                    )),
    age_categorical = factor(age_categorical,
                             levels = c(
                               "Prime-aged (31-50)",
                               "Young (21-30)",
                               "Old (51+)",
                               "Unknown"
                             )),
    unemployment_rate_categorical5 =
      case_when(unemployment_rate < 5 ~ "Low to moderate [3.1%, 5.0%)",
                unemployment_rate >= 5 & unemployment_rate < 11 ~ "Moderate to high [5.0%, 10.4%]") %>%
      factor(levels = c("Low to moderate [3.1%, 5.0%)", "Moderate to high [5.0%, 10.4%]")),
    unemployment_rate_categorical6 =
      case_when(unemployment_rate < 6 ~ "Low to moderate [3.1%, 6.0%)",
                unemployment_rate >= 6 & unemployment_rate < 11 ~ "Moderate to high [6.0%, 10.4%]") %>%
      factor(levels = c("Low to moderate [3.1%, 6.0%)", "Moderate to high [6.0%, 10.4%]")),
    unemployment_rate_categorical7 =
      case_when(unemployment_rate < 7 ~ "Low to moderate [3.1%, 7.0%)",
                unemployment_rate >= 7 & unemployment_rate < 11 ~ "Moderate to high [7.0%, 10.4%]") %>%
      factor(levels = c("Low to moderate [3.1%, 7.0%)", "Moderate to high [7.0%, 10.4%]")),
    rr = (callback_count_non_working/application_count_non_working)/
      (callback_count_working/application_count_working),
    logrr = log(rr),
    selogrr = sqrt(1/callback_count_non_working + 1/callback_count_working +
                     1/application_count_non_working + 1/application_count_working),
    or = (callback_count_non_working/(application_count_non_working - callback_count_non_working))/
      (callback_count_working/(application_count_working - callback_count_working)),
    logor = log(or),
    selogor = sqrt(1/callback_count_non_working + 1/callback_count_working +
                     1/(application_count_non_working - callback_count_non_working) +
                     1/(application_count_working - callback_count_working))
  ) %>%
  filter(treatment == "Unemployed") %>%
  select(-(interview_invitation_rate_non_working:positive_reaction_count_working)) %>%
  relocate(unemployment_duration_category, .after = unemployment_duration_months) %>%
  relocate(region_agg, .after = region) %>%
  relocate(rr, logrr, selogrr, or, logor, selogor, .after = application_count_working)

write_csv(data,
          file = file.path(data.path, "unemployment-inactivity-hiring_clean.csv"))

outliers <-
  c(# Detected through dmetar::find.outliers()
    # Influence through dmetar::InfluenceAnalysis() where influence on heterogeneity > 10 or influence on result > 8
    ## Low influence on pooled result and low heterogeneity contribution
    "Pedulla 1-1", "Tomlin 1-1",
    
    ## Low influence on pooled result and high heterogeneity contribution
    "Birkelund et al. (2017) 1-1", "Birkelund et al. (2017) 1-2",
    "Oberholzer-Gee (2008) 1-5", "Farber et al. (2016) 1-3", "Weisshaar (2018) 1-1", # also visually detected
    
    # High influence on pooled result and low heterogeneity contribution
    "Farber et al. (2019) 1-1", "Nunley et al. (2017) 1-3", "Kroft et al. (2013) 1-36",
    
    ## High influence on pooled result and high heterogeneity contribution
    "Oberholzer-Gee (2008) 1-2", "Duguet et al. (2018) 1-1", "Oberholzer-Gee (2008) 1-1",
    "Farber et al. (2019) 1-3" # also visually detected
    )

data <-
  data %>%
  mutate(outlier = case_when(uid %in% outliers ~ TRUE,
                             !(uid %in% outliers) ~ FALSE))

data.out.adj <-
  data %>%
  filter(!(uid %in% outliers))


data.0m6m <-
  data %>%
  filter(unemployment_duration_months <= 6)

data.0m6m.out.adj <-
  data.0m6m %>%
  filter(!(uid %in% c("Duguet et al. (2018) 1-1", "Farber et al. (2016) 1-3", "Oberholzer-Gee (2019) 1-3")))


data.6m12m <-
  data %>%
  filter(unemployment_duration_months >= 7 & unemployment_duration_months <= 12)

data.6m12m.out.adj <-
  data.6m12m %>%
  filter(!(uid %in% c("Oberholzer-Gee (2008) 1-2", "Pedulla (2018) 1-1")))


data.12m18m <-
  data %>%
  filter(unemployment_duration_months >= 13 & unemployment_duration_months <= 18)

data.12m18m.out.adj <-
  data.12m18m


data.18m36m <-
  data %>%
  filter(unemployment_duration_months >= 19 & unemployment_duration_months <= 36)

data.18m36m.out.adj <-
  data.18m36m %>%
  filter(!(uid %in% c("Kristal et al. (2023) 1-2", "Kroft et al. (2013) 1-31", "Oberholzer-Gee (2008) 1-5")))
