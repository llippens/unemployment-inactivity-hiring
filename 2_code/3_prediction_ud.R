# Step 1: Generate sequence for unemployment_duration_months
udm_seq <- data$unemployment_duration_months

data_pred <-
  data %>%
  mutate(`age_categoricalYoung (21-30)` = case_when(age_categorical == "Young (21-30)" ~ 1,
                                                    TRUE ~ 0),
         `age_categoricalOld (51+)` = case_when(age_categorical == "Old (51+)" ~ 1,
                                                TRUE ~ 0),
         `age_categoricalUnknown` = case_when(age_categorical == "Unknown" ~ 1,
                                              TRUE ~ 0),
         genderFemale = case_when(gender == "Female" ~ 1,
                                  TRUE ~ 0),
         countryChina = case_when(country == "China" ~ 1,
                                  TRUE ~ 0),
         countryFrance = case_when(country == "France" ~ 1,
                                   TRUE ~ 0),
         countryNorway = case_when(country == "Norway" ~ 1,
                                   TRUE ~ 0),
         countrySweden = case_when(country == "Sweden" ~ 1,
                                   TRUE ~ 0),
         countrySwitzerland = case_when(country == "Switzerland" ~ 1,
                                        TRUE ~ 0),
         countryUK = case_when(country == "UK" ~ 1,
                               TRUE ~ 0))

ctable <-
  (table(data$country)/nrow(data)) %>%
  as.data.frame() %>%
  as_tibble() %>%
  clean_names() %>%
  rename(country = var1)

rtable <-
  (table(data$response_type)/nrow(data)) %>%
  as.data.frame() %>%
  as_tibble() %>%
  clean_names() %>%
  rename(rtype = var1)

gtable <-
  (table(data$gender)/nrow(data)) %>%
  as.data.frame() %>%
  as_tibble() %>%
  clean_names() %>%
  rename(gender = var1)

atable <-
  (table(data$age_categorical)/nrow(data)) %>%
  as.data.frame() %>%
  as_tibble() %>%
  clean_names() %>%
  rename(acat = var1)

# Step 2: Map the constant moderator values to the model encoding
newmods_matrix <- matrix(0, nrow = length(udm_seq), ncol = 14)
colnames(newmods_matrix) <- c("unemployment_duration_months",
                              "unemployment_rate",
                              "response_typePositive reaction",
                              "countryChina", "countryFrance", "countryNorway", "countrySweden", "countrySwitzerland", "countryUK",
                              "years_average",
                              "genderFemale",
                              "age_categoricalYoung (21-30)", "age_categoricalOld (51+)", "age_categoricalUnknown")

newmods_matrix[, "unemployment_duration_months"] <- udm_seq
newmods_matrix[, "years_average"] <- mean(data$years_average)
newmods_matrix[, "unemployment_rate"] <- mean(data$unemployment_rate)

newmods_matrix[, "response_typePositive reaction"] <- rtable %>% filter(rtype == "Positive reaction") %>% pull()

newmods_matrix[, "genderFemale"] <- gtable %>% filter(gender == "Female") %>% pull() #data_pred$genderFemale

newmods_matrix[, "countryChina"] <- ctable %>% filter(country == "China") %>% pull() #data_pred$countryChina
newmods_matrix[, "countryFrance"] <- ctable %>% filter(country == "France") %>% pull() #data_pred$countryFrance
newmods_matrix[, "countryNorway"] <- ctable %>% filter(country == "Norway") %>% pull() #data_pred$countryNorway
newmods_matrix[, "countrySweden"] <- ctable %>% filter(country == "Sweden") %>% pull() #data_pred$countrySweden
newmods_matrix[, "countrySwitzerland"] <- ctable %>% filter(country == "Switzerland") %>% pull() #data_pred$countrySwitzerland
newmods_matrix[, "countryUK"] <- ctable %>% filter(country == "UK") %>% pull() #data_pred$countryUK

newmods_matrix[, "age_categoricalYoung (21-30)"] <- atable %>% filter(acat == "Young (21-30)") %>% pull() #data_pred$`age_categoricalYoung (21-30)`
newmods_matrix[, "age_categoricalOld (51+)"] <- atable %>% filter(acat == "Old (51+)") %>% pull() #data_pred$`age_categoricalOld (51+)`
newmods_matrix[, "age_categoricalUnknown"] <- atable %>% filter(acat == "Unknown") %>% pull() #data_pred$`age_categoricalUnknown`

# Step 4: Use predict function with newmods_matrix
mreg.ud.pred <-
  predict(mreg %>%
            robust(cluster = cluster,
                   adjust = FALSE),
          newmods = newmods_matrix,
          transf = exp) %>%
  as_tibble()

mreg.ud.pred <- mreg.ud.pred %>%
  mutate(x = as.numeric(rownames(mreg.ud.pred)),
         ud = data$unemployment_duration_months)
