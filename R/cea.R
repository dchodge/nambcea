
cal_impact_standard <- function(list_outputs, list_names, threshold) {

  icer_output_A <- calc_impact(list_outputs[[1]], list_outputs[[2]],  threshold, "base_mab_p1_pal", pal_base = FALSE, pal_compare = TRUE) %>%
    mutate(names = list_names[[1]])
  icer_output_B <- calc_impact(list_outputs[[2]], list_outputs[[3]], threshold, "base_mab_p1_vhr_icer", pal_base = TRUE) %>%
    mutate(names = list_names[[2]])
  icer_output_C <- calc_impact(list_outputs[[3]], list_outputs[[4]],  threshold, "base_mab_p1_icer", pal_base = FALSE) %>%
    mutate(names = list_names[[3]])
  icer_output_D <- calc_impact(list_outputs[[4]], list_outputs[[5]],  threshold, "base_mab_p1_catchup_icer", pal_base = FALSE) %>%
    mutate(names = list_names[[4]])
  icer_output_E <- calc_impact(list_outputs[[5]], list_outputs[[6]],  threshold, "base_mab_p1_catchup_nip_icer", pal_base = FALSE) %>%
    mutate(names = list_names[[5]])
  icer_output <- bind_rows(icer_output_A, icer_output_B, icer_output_C, icer_output_D, icer_output_E)
  icer_output
}

cal_impact_trim <- function(list_outputs, list_names, threshold) {

  icer_output_A <- calc_impact(list_outputs[[1]], list_outputs[[2]],  threshold, "base_mab_p1_vhr_icer", pal_base = FALSE, pal_compare = FALSE) %>%
    mutate(names = list_names[[1]])
  icer_output_B <- calc_impact(list_outputs[[2]], list_outputs[[3]], threshold, "base_mab_p1_icer", pal_base = FALSE) %>%
    mutate(names = list_names[[2]])
  icer_output_C <- calc_impact(list_outputs[[3]], list_outputs[[4]],  threshold, "base_mab_p1_catchup_icer", pal_base = FALSE) %>%
    mutate(names = list_names[[3]])
  icer_output_D <- calc_impact(list_outputs[[4]], list_outputs[[5]],  threshold, "base_mab_p1_catchup_nip_icer", pal_base = FALSE) %>%
    mutate(names = list_names[[4]])

  icer_output <- bind_rows(icer_output_A, icer_output_B, icer_output_C, icer_output_D)
  icer_output
}

calc_prop_ce_A <- function(list_outputs, list_names, threshold) {

  list_outputs[[1]]$QALY <- list_outputs[[1]]$QALY %>% rename(qaly_p1 = value)
  list_outputs[[2]]$QALY <- list_outputs[[2]]$QALY %>% rename(qaly_p2 = value)
  list_outputs[[3]]$QALY <- list_outputs[[3]]$QALY %>% rename(qaly_p3 = value)
  list_outputs[[4]]$QALY <- list_outputs[[4]]$QALY %>% rename(qaly_p4 = value)
  list_outputs[[5]]$QALY <- list_outputs[[5]]$QALY %>% rename(qaly_p5 = value)

  list_outputs[[1]]$cost <- list_outputs[[1]]$cost %>% rename(cost_p1 = value)
  list_outputs[[2]]$cost <- list_outputs[[2]]$cost %>% rename(cost_p2 = value)
  list_outputs[[3]]$cost <- list_outputs[[3]]$cost %>% rename(cost_p3 = value)
  list_outputs[[4]]$cost <- list_outputs[[4]]$cost %>% rename(cost_p4 = value)
  list_outputs[[5]]$cost <- list_outputs[[5]]$cost %>% rename(cost_p5 = value)

  qaly_vals <- Reduce(function(x, y) 
    unique(left_join(x, y, by = c("seed", "type", "metric"))),
    map(list_outputs, ~.x$QALY %>% filter(type == "discounted", metric == "total"))
  ) %>% mutate(
    qaly_diff_1 = qaly_p1 - qaly_p2,
    qaly_diff_2 = qaly_p2 - qaly_p3,
    qaly_diff_3 = qaly_p3 - qaly_p4,
    qaly_diff_4 = qaly_p4 - qaly_p5, .keep = "unused")

  cost_inter_vals <- Reduce(function(x, y) 
    unique(left_join(x, y, by = c("seed", "type", "metric"))),
    map(list_outputs, ~.x$cost %>% filter(type == "discounted", metric == "total"))
  ) %>% mutate(
      cost_diff_1 = cost_p2 - cost_p1,
      cost_diff_2 = cost_p3 - cost_p2,
      cost_diff_3 = cost_p4 - cost_p3,
      cost_diff_4 = cost_p5 - cost_p4, .keep = "unused")

 # pal_cost <- c(4035.50 * sum(list_outputs[[1]]$vac_cal[, 1] * exp(-0.035 / 52 * (1:521))), rep(0, 4))
  tot_doses <- unlist(map(list_outputs, ~sum(.x$vac_cal[, 2] * exp(-0.035 / 52 * (1:521)))))

  df_posterior_list <- list()
  j <- 1
  for (ppd in 36:100) {
    df_ppd <-  data.frame(
      icer_ppd_1 = (cost_inter_vals$cost_diff_1 + ppd * (tot_doses[2] - tot_doses[1])) / qaly_vals$qaly_diff_1,
      icer_ppd_2 = (cost_inter_vals$cost_diff_2 + ppd * (tot_doses[3] - tot_doses[2])) / qaly_vals$qaly_diff_2,
      icer_ppd_3 = (cost_inter_vals$cost_diff_3 + ppd * (tot_doses[4] - tot_doses[3])) / qaly_vals$qaly_diff_3,
      icer_ppd_4 = (cost_inter_vals$cost_diff_4 + ppd * (tot_doses[5] - tot_doses[4])) / qaly_vals$qaly_diff_4
    )
    uncertainty_ppd <- vector()
    for (i in c(1:7, 9:100)) {
      df_ppd[i, ][df_ppd[i, ] > threshold] <- NA
      uncertainty_ppd[i] <- as.numeric(which.max(df_ppd[i, ]))
    }
    uncertainty_ppd <- factor(uncertainty_ppd, levels = 1:4)
    df_posterior_list[[j]] <- data.frame(
      ppd = ppd,
      post = uncertainty_ppd
    )
    j <- j + 1
  }
  df_posterior <- df_posterior_list %>% bind_rows
}

calc_prop_ce_B <- function(list_outputs, list_names, threshold) {

  list_outputs[[1]]$QALY <- list_outputs[[1]]$QALY %>% rename(qaly_p1 = value)
  list_outputs[[2]]$QALY <- list_outputs[[2]]$QALY %>% rename(qaly_p2 = value)
  list_outputs[[3]]$QALY <- list_outputs[[3]]$QALY %>% rename(qaly_p3 = value)
  list_outputs[[4]]$QALY <- list_outputs[[4]]$QALY %>% rename(qaly_p4 = value)

  list_outputs[[1]]$cost <- list_outputs[[1]]$cost %>% rename(cost_p1 = value)
  list_outputs[[2]]$cost <- list_outputs[[2]]$cost %>% rename(cost_p2 = value)
  list_outputs[[3]]$cost <- list_outputs[[3]]$cost %>% rename(cost_p3 = value)
  list_outputs[[4]]$cost <- list_outputs[[4]]$cost %>% rename(cost_p4 = value)

  qaly_vals <- Reduce(function(x, y) 
    unique(left_join(x, y, by = c("seed", "type", "metric"))),
    map(list_outputs, ~.x$QALY %>% filter(type == "discounted", metric == "total"))
  ) %>% mutate(
    qaly_diff_1 = qaly_p1 - qaly_p2,
    qaly_diff_2 = qaly_p2 - qaly_p3,
    qaly_diff_3 = qaly_p3 - qaly_p4, .keep = "unused")

  cost_inter_vals <- Reduce(function(x, y) 
    unique(left_join(x, y, by = c("seed", "type", "metric"))),
    map(list_outputs, ~.x$cost %>% filter(type == "discounted", metric == "total"))
  ) %>% mutate(
      cost_diff_1 = cost_p2 - cost_p1,
      cost_diff_2 = cost_p3 - cost_p2,
      cost_diff_3 = cost_p4 - cost_p3, .keep = "unused")

 # pal_cost <- c(4035.50 * sum(list_outputs[[1]]$vac_cal[, 1] * exp(-0.035 / 52 * (1:521))), rep(0, 4))
  tot_doses <- unlist(map(list_outputs, ~sum(.x$vac_cal[, 2] * exp(-0.035 / 52 * (1:521)))))

  df_posterior_list <- list()
  j <- 1
  for (ppd in 1:35) {
    df_ppd <-  data.frame(
      icer_ppd_1 = (cost_inter_vals$cost_diff_1 + ppd * (tot_doses[2] - tot_doses[1])) / qaly_vals$qaly_diff_1,
      icer_ppd_2 = (cost_inter_vals$cost_diff_2 + ppd * (tot_doses[3] - tot_doses[2])) / qaly_vals$qaly_diff_2,
      icer_ppd_3 = (cost_inter_vals$cost_diff_3 + ppd * (tot_doses[4] - tot_doses[3])) / qaly_vals$qaly_diff_3
    )
    uncertainty_ppd <- vector()
    for (i in c(1:7, 9:100)) {
      df_ppd[i, ][df_ppd[i, ] > threshold] <- NA
      uncertainty_ppd[i] <- as.numeric(which.max(df_ppd[i, ]))
    }
    uncertainty_ppd <- factor(uncertainty_ppd + 1, levels = 2:4)
    df_posterior_list[[j]] <- data.frame(
      ppd = ppd,
      post = uncertainty_ppd
    )
    j <- j + 1
  }
  df_posterior <- df_posterior_list %>% bind_rows
}

# chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://www.bmj.com/bmj/section-pdf/186255?path=/bmj/338/7709/Analysis.full.pdf
calc_sea_implement <- function(base, programme, threshold, ppd) {

     base_sum <- base$outcomes_week_age %>% 
        group_by(outcome, seed, age_group) %>%
        summarise(incidence_base = sum(incidence))

    programme_sum <- programme$outcomes_week_age %>% 
        group_by(outcome, seed, age_group) %>%
        summarise(incidence_programme = sum(incidence))
      
    tot_doses_base <- sum(base$vac_cal[, 2] * exp(-0.035 / 52 * (1:521)))
    tot_doses_prog <- sum(programme$vac_cal[, 2] * exp(-0.035 / 52 * (1:521)))
     # Total QALY loss needed
    df_qaly <- left_join(
        base$QALY %>% filter(type == "discounted", metric == "total"),
        programme$QALY %>% filter(type == "discounted", metric == "total"),
        by = c("seed", "type", "metric")
    ) %>% rename(base_qaly = value.x, inter_qaly = value.y) %>% unique

  
       # Need total cost different ignoring number of doses given 
    df_cost <- left_join(
        base$cost %>% filter(type == "discounted", metric == "total"),
        programme$cost %>% filter(type == "discounted", metric == "total"),
        by = c("seed", "type", "metric")
    ) %>% rename(base_cost = value.x, inter_cost = value.y) %>% unique

    ppd <- ppd
    left_join(df_qaly, df_cost, by = c("seed", "type", "metric"))  %>% 
      mutate(threshold_season = (threshold * (inter_qaly - base_qaly) - (inter_cost - base_cost) - ppd * (tot_doses_prog - tot_doses_base)))

}

calc_impact <- function(base, programme, threshold, filename, pal_base, pal_compare = FALSE) {
    base_sum <- base$outcomes_week_age %>% 
        group_by(outcome, seed, age_group) %>%
        summarise(incidence_base = sum(incidence))

    programme_sum <- programme$outcomes_week_age %>% 
        group_by(outcome, seed, age_group) %>%
        summarise(incidence_programme = sum(incidence))

    df_averted <- base_sum %>% left_join(programme_sum, by = c("outcome", "seed", "age_group")) %>%
        mutate(cases_averted = incidence_base - incidence_programme)

    # Total QALY loss needed
    df_qaly <- left_join(
        base$QALY %>% filter(type == "discounted", metric == "total"),
        programme$QALY %>% filter(type == "discounted", metric == "total"),
        by = c("seed", "type", "metric")
    ) %>% rename(base_qaly = value.x, inter_qaly = value.y) %>% unique

    # Need total cost different ignoring number of doses given 
    df_cost <- left_join(
        base$cost %>% filter(type == "discounted", metric == "total"),
        programme$cost %>% filter(type == "discounted", metric == "total"),
        by = c("seed", "type", "metric")
    ) %>% rename(base_cost = value.x, inter_cost = value.y) %>% unique
    if (pal_compare) {
        pal_cost <- 3000.00 * sum(programme$vac_cal[, 1] * exp(-0.035 / 52 * (1:521)))
        df_icer <- left_join(df_qaly, df_cost, by = c("seed", "type", "metric")) %>%
              mutate(ppd = NA, inter_cost = inter_cost + pal_cost)
    } else {
      if (pal_base) {
          pal_cost <- 3000.00 * sum(base$vac_cal[, 1] * exp(-0.035 / 52 * (1:521)))
          tot_doses <- sum(programme$vac_cal[, 2] * exp(-0.035 / 52 * (1:521)))
          df_icer <- left_join(df_qaly, df_cost, by = c("seed", "type", "metric")) %>%
              mutate(ppd = (threshold * (base_qaly - inter_qaly) - (inter_cost - base_cost) + pal_cost) / tot_doses)
      } else{
          tot_doses <- sum(programme$vac_cal[, 2] * exp(-0.035 / 52 * (1:521))) - sum(base$vac_cal[, 2] * exp(-0.035 / 52 * (1:521)))
          df_icer <- left_join(df_qaly, df_cost, by = c("seed", "type", "metric")) %>%
              mutate(ppd = (threshold * (base_qaly - inter_qaly) - (inter_cost - base_cost)) / tot_doses)
      }
    }
    df_icer
}