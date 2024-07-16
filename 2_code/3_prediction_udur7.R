# Define a function to mutate data sets similarly
mutate_ur <- function(data) {
  data %>%
    mutate(`age_categoricalYoung (21-30)` = case_when(age_categorical == "Young (21-30)" ~ 1,
                                                      TRUE ~ 0),
           `age_categoricalOld (51+)` = case_when(age_categorical == "Old (51+)" ~ 1,
                                                  TRUE ~ 0),
           `age_categoricalUnknown` = case_when(age_categorical == "Unknown" ~ 1,
                                                TRUE ~ 0),
           genderFemale = case_when(gender == "Female" ~ 1,
                                    TRUE ~ 0),
           genderMixed = case_when(gender == "Mixed" ~ 1,
                                   TRUE ~ 0),
           `regionEurope (Western)` = case_when(region == "Europe (Western)" ~ 1,
                                                TRUE ~ 0),
           `regionEurope (Northern)` = case_when(region == "Europe (Northern)" ~ 1,
                                                 TRUE ~ 0),
           `regionEurope (Central)` = case_when(region == "Europe (Central)" ~ 1,
                                                TRUE ~ 0),
           `regionAsia (Eastern)` = case_when(region == "Asia (Eastern)" ~ 1,
                                              TRUE ~ 0))
}

data.ur.low <- data %>%
  filter(unemployment_rate < 7) %>%
  mutate_ur()

data.ur.high <- data %>%
  filter(unemployment_rate >= 7) %>%
  mutate_ur()

# Define a function to create frequency tables
create_table <- function(data, column, new_name) {
  (table(data[[column]]) / nrow(data)) %>%
    as.data.frame() %>%
    as_tibble() %>%
    clean_names() %>%
    rename(!!new_name := var1)
}

# Define a function for metabin and rma models
create_metabin_rma <- function(d) {
  ma_data <- metabin_cstm(data = d)
  
  rma(
    yi = ma_data$TE,
    sei = ma_data$seTE,
    data = d,
    method = "REML",
    mods = ~ unemployment_duration_months + response_type +
      region + years_average +
      gender + age_categorical,
    test = "knha"
  )
}

# Process data.ur.low
## Create frequency tables
ctable.ur.low <- create_table(data.ur.low, "region", "region")
rtable.ur.low <- create_table(data.ur.low, "response_type", "rtype")
gtable.ur.low <- create_table(data.ur.low, "gender", "gender")
atable.ur.low <- create_table(data.ur.low, "age_categorical", "acat")

## Run regression
mreg.ur.low <- create_metabin_rma(data.ur.low)

## Create matrix
newmods.matrix.ur.low <- matrix(0, nrow = nrow(data.ur.low), ncol = 11)

colnames(newmods.matrix.ur.low) <- c(
  "unemployment_duration_months",
  "response_typePositive reaction",
  "regionEurope (Western)", "regionEurope (Northern)", "regionEurope (Central)", "regionAsia (Eastern)",
  "years_average",
  "genderFemale",
  "age_categoricalYoung (21-30)", "age_categoricalOld (51+)", "age_categoricalUnknown"
)

newmods.matrix.ur.low[, "unemployment_duration_months"] <- data.ur.low$unemployment_duration_months
newmods.matrix.ur.low[, "years_average"] <- mean(data.ur.low$years_average)

newmods.matrix.ur.low[, "response_typePositive reaction"] <- rtable.ur.low %>% filter(rtype == "Positive reaction") %>% pull()

newmods.matrix.ur.low[, "regionEurope (Western)"] <- ctable.ur.low %>% filter(region == "Europe (Western)") %>% pull()
newmods.matrix.ur.low[, "regionEurope (Northern)"] <- ctable.ur.low %>% filter(region == "Europe (Northern)") %>% pull()
newmods.matrix.ur.low[, "regionEurope (Central)"] <- ctable.ur.low %>% filter(region == "Europe (Central)") %>% pull()
newmods.matrix.ur.low[, "regionAsia (Eastern)"] <- ctable.ur.low %>% filter(region == "Asia (Eastern)") %>% pull()

newmods.matrix.ur.low[, "genderFemale"] <- gtable.ur.low %>% filter(gender == "Female") %>% pull()

newmods.matrix.ur.low[, "age_categoricalYoung (21-30)"] <- atable.ur.low %>% filter(acat == "Young (21-30)") %>% pull()
newmods.matrix.ur.low[, "age_categoricalOld (51+)"] <- atable.ur.low %>% filter(acat == "Old (51+)") %>% pull()
newmods.matrix.ur.low[, "age_categoricalUnknown"] <- atable.ur.low %>% filter(acat == "Unknown") %>% pull()

## Predict y*
mreg.ur.low.pred <- predict(mreg.ur.low %>%
                              robust(cluster = cluster,
                                     adjust = TRUE,
                                     clubSandwich = TRUE),
                            newmods = newmods.matrix.ur.low,
                            transf = exp) %>%
  as_tibble() %>%
  mutate(x = as.numeric(rownames(.)),
         ud = data.ur.low$unemployment_duration_months)

# Process data.ur.high
## Create frequency tables
ctable.ur.high <- create_table(data.ur.high, "region", "region")
rtable.ur.high <- create_table(data.ur.high, "response_type", "rtype")
gtable.ur.high <- create_table(data.ur.high, "gender", "gender")
atable.ur.high <- create_table(data.ur.high, "age_categorical", "acat")

## Run regression
mreg.ur.high <- create_metabin_rma(data.ur.high)

## Create matrix
newmods.matrix.ur.high <- matrix(0, nrow = nrow(data.ur.high), ncol = 5)

colnames(newmods.matrix.ur.high) <- c(
  "unemployment_duration_months",
  "response_typePositive reaction",
  "regionEurope (Western)",
  "years_average",
  "age_categoricalYoung (21-30)"
)

newmods.matrix.ur.high[, "unemployment_duration_months"] <- data.ur.high$unemployment_duration_months
newmods.matrix.ur.high[, "years_average"] <- mean(data.ur.high$years_average)

newmods.matrix.ur.high[, "response_typePositive reaction"] <- rtable.ur.high %>% filter(rtype == "Positive reaction") %>% pull()

newmods.matrix.ur.high[, "regionEurope (Western)"] <- ctable.ur.high %>% filter(region == "Europe (Western)") %>% pull()

newmods.matrix.ur.high[, "age_categoricalYoung (21-30)"] <- atable.ur.high %>% filter(acat == "Young (21-30)") %>% pull()


## Predict y*
mreg.ur.high.pred <- predict(mreg.ur.high %>%
                               robust(cluster = cluster,
                                      adjust = TRUE,
                                      clubSandwich = TRUE),
                             newmods = newmods.matrix.ur.high,
                             transf = exp) %>%
  as_tibble() %>%
  mutate(x = as.numeric(rownames(.)),
         ud = data.ur.high$unemployment_duration_months)
