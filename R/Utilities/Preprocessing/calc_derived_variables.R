calc_derived_variables <- function(data) {
  data %>%
    # Calculate net_kcal (sum of kcal across all weeks) and place after kcal_8w
    mutate(
      net_kcal = rowSums(across(starts_with("kcal_")), na.rm = TRUE),
      .after = kcal_8w
    ) %>%
    # Calculate delta_BW (change in body weight) and place after BW_8w
    mutate(
      delta_BW = BW_8w - BW_0w,
      .after = BW_8w
    ) %>%
    # Calculate food_efficiency (ratio of body weight change to net energy intake)
    # place after net_kcal
    mutate(
      food_efficiency = if_else(net_kcal == 0, 0, delta_BW / net_kcal),
      .after = net_kcal
    )
}
