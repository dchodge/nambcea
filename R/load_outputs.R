
get_cea_plane_info_mean <- function(none, output, name) {
  pal_cost <- 4035.50

  mean_QALY_none <- none$QALY %>% filter(type == "discounted", metric == "total") %>% unique %>% pull(value) # qaly loss
  mean_cost_treat_none <- none$cost %>% filter(type == "discounted", metric == "total") %>% unique %>% pull(value)  # cost of treatment

  mean_QALY <- output$QALY %>% filter(type == "discounted", metric == "total") %>% unique %>% pull(value) # qaly loss
  mean_cost_treat <- output$cost %>% filter(type == "discounted", metric == "total") %>% unique %>% pull(value) # cost of treatment
  mean_cost_ad <- (output$vac_cal[, 1] * pal_cost * exp(-(1:521 - 1) * 0.035 / 52.0)) %>% sum # cost of pal programmes
  doses <- (output$vac_cal[, 2] * exp(-(1:521 - 1) * 0.035 / 52.0)) %>% sum # number of doses of mabs given

  data.frame(
    strategy = name,
    sample = 1:100,
    QALY = mean_QALY_none - mean_QALY,
    cost = (mean_cost_treat + mean_cost_ad),
    doses = doses
  ) %>% group_by(strategy) %>%
    summarise(QALY = mean(QALY, na.rm = TRUE), cost = mean(cost, na.rm = TRUE), doses = mean(doses, na.rm = TRUE)) %>% arrange(doses)

}

get_cea_plane_info <- function(none, output, name) {
  pal_cost <- 4035.50

  mean_QALY_none <- none$QALY %>% filter(type == "discounted", metric == "total") %>% unique %>% pull(value) # qaly loss
  mean_cost_treat_none <- none$cost %>% filter(type == "discounted", metric == "total") %>% unique %>% pull(value)  # cost of treatment

  mean_QALY <- output$QALY %>% filter(type == "discounted", metric == "total") %>% unique %>% pull(value) # qaly loss
  mean_cost_treat <- output$cost %>% filter(type == "discounted", metric == "total") %>% unique %>% pull(value) # cost of treatment
  mean_cost_ad <- (output$vac_cal[, 1] * pal_cost * exp(-(1:521 - 1) * 0.035 / 52.0)) %>% sum # cost of pal programmes
  doses <- (output$vac_cal[, 2] * exp(-(1:521 - 1) * 0.035 / 52.0)) %>% sum # number of doses of mabs given

  data.frame(
    strategy = name,
    sample = 1:100,
    QALY = mean_QALY_none - mean_QALY,
    cost = (mean_cost_treat + mean_cost_ad),
    doses = doses
  )
}

get_icer_table_template <- function(run_type = "base", mean = TRUE) {
  load(file = here("outputs",  "impact", "base", "none.RData")) # output_default_none
  if (run_type == "base") {
    load(file = here("outputs",  "impact",  "base_sum.RData")) # output_season_vhr_base
  } else if (run_type == "d_100") {
    load(file = here("outputs",  "impact",  "d_100_sum.RData")) # output_season_vhr_base
  } else if (run_type == "d_250") {
    load(file = here("outputs",  "impact",  "d_250_sum.RData")) # output_season_vhr_base
  } else if (run_type == "d_360") {
    load(file = here("outputs",  "impact",  "d_360_sum.RData")) # output_season_vhr_base
  } else if (run_type == "low_cov") {
    load(file = here("outputs",  "impact",  "low_cov_sum.RData")) # output_season_vhr_base
  } else if (run_type == "lower_icer") {
    load(file = here("outputs",  "impact",  "base_sum.RData")) # output_season_vhr_base
  } else if (run_type == "2mo") {
    load(file = here("outputs",  "impact",  "2mo_sum.RData")) # output_season_vhr_base
  } else if (run_type == "age_eff") {
    load(file = here("outputs",  "impact",  "age_eff_sum.RData")) # output_season_vhr_base
  } else {
    cat("Select one of base, d_100, d_250, d_360, low_cov, lower_icer, 2mo")
  }
  if(mean) {
    icer_table_plot <- map2(list_outputs, names(list_outputs), ~get_cea_plane_info_mean(output_default_none, .x, .y)) %>% bind_rows
  } else {
    icer_table_plot <- map2(list_outputs, names(list_outputs),~get_cea_plane_info(output_default_none, .x, .y)) %>% bind_rows
  }
  icer_table_plot
}
