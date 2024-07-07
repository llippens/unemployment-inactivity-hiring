# Step 1: Generate sequence for unemployment_duration_months
udm_seq <- seq(1, 36, 1)

# Step 2: Set constant values for other moderators
constants <- c(response_type = "Interview invitation",
               country = "USA",
               years_average = mean(data$years_average),
               gender = "Male",
               age_categorical = "Prime-aged (31-50",
               unemployment_rate = mean(data$unemployment_rate))

# Step 3: Map the constant moderator values to the model encoding
newmods_matrix <- matrix(0, nrow = length(udm_seq), ncol = 14)
colnames(newmods_matrix) <- c("unemployment_duration_months",
                              "response_typePositive reaction",
                              "countryChina", "countryFrance", "countryNorway", "countrySweden", "countrySwitzerland", "countryUK",
                              "years_average",
                              "unemployment_rate",
                              "genderFemale",
                              "age_categoricalYoung (21-30)", "age_categoricalOld (51+)", "age_categoricalUnknown")

newmods_matrix[, "unemployment_duration_months"] <- udm_seq
newmods_matrix[, "years_average"] <- mean(data$years_average)
newmods_matrix[, "unemployment_rate"] <- mean(data$unemployment_rate)

newmods_matrix[, "age_categoricalYoung (21-30)"] <- ifelse(constants["age_categorical"] == "Young (21-30)", 1, 0)
newmods_matrix[, "age_categoricalOld (51+)"] <- ifelse(constants["age_categorical"] == "Old (51+)", 1, 0)
newmods_matrix[, "age_categoricalUnknown"] <- ifelse(constants["age_categorical"] == "Unknown", 1, 0)

# Step 4: Use predict function with newmods_matrix
mreg.emm.pred <-
  predict(mreg, newmods = newmods_matrix, transf = exp) %>%
  as_tibble()

mreg.emm.pred <- mreg.emm.pred %>%
  mutate(x = as.numeric(rownames(mreg.emm.pred)))
