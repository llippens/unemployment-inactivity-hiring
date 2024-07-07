# Step 1: Generate sequence for unemployment_duration_months
udm_seq <- seq(1, 36, 1)
ur_seq <- seq(1, 15)

# Step 2: Set constant values for other moderators
constants <- c(response_type = "Interview invitation",
               region = "America (Northern)",
               years_average = mean(data$years_average),
               gender = "Male",
               age_categorical = "Prime-aged (31-50)")

# Step 3: Map the constant moderator values to the model encoding
newmods_matrix <- matrix(0, nrow = length(udm_seq)*length(ur_seq), ncol = 14)
colnames(newmods_matrix) <- c("unemployment_duration_months",
                              "unemployment_rate",
                              "unemployment_duration_months:unemployment_rate",
                              "response_typePositive reaction",
                              "regionEurope (Western)", "regionEurope (Northern)", "regionEurope (Central)", "regionAsia (Eastern)",
                              "years_average",
                              "genderFemale",
                              "genderMixed",
                              "age_categoricalYoung (21-30)", "age_categoricalOld (51+)", "age_categoricalUnknown")

newmods_matrix[, "unemployment_duration_months"] <- udm_seq
newmods_matrix[, "unemployment_rate"] <- rep(ur_seq, length(udm_seq)) %>% sort()
newmods_matrix[, "years_average"] <- mean(data$years_average)

newmods_matrix[, "age_categoricalYoung (21-30)"] <- ifelse(constants["age_categorical"] == "Young (21-30)", 1, 0)
newmods_matrix[, "age_categoricalOld (51+)"] <- ifelse(constants["age_categorical"] == "Old (51+)", 1, 0)
newmods_matrix[, "age_categoricalUnknown"] <- ifelse(constants["age_categorical"] == "Unknown", 1, 0)

# Step 4: Use predict function with newmods_matrix
mreg.ur.pred <-
  predict(mreg.ur, newmods = newmods_matrix, transf = exp) %>%
  as_tibble()

mreg.ur.pred <- mreg.ur.pred %>%
  mutate(x1 = rep(udm_seq, length(ur_seq)),
         x2 = rep(ur_seq, length(udm_seq)) %>% sort(),
         y_pred = pred-1)

# (Step 5: visualise)
ggplot(mreg.ur.pred, aes(x = x1, y = x2, z = pred-1)) +
  geom_contour_filled() +
  labs(title = "Contour Plot of Interaction",
       x = "UD",
       y = "UR",
       fill = "Predicted Response")
