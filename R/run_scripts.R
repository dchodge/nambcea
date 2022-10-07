
make_data_list <- function() {
  source("R/vac_cal.R") # generates the vaccination calendars
  source("R/calc_outcomes.R")  # calculate the outcomes
  # Load posteriors
  load(here("data", "inter_model_output", "posteriors.Rda"))  # posteriors from fitting in Hodgson et al. 2020
  # Load seeds
  seeds <- read.csv(here("data", "inter_model_input", "seed_samples.csv"), header = FALSE)[, 1] + 1

  # Ex.1 Long-acting monoclonal antibodies at HR, LR, and VHR, given seasonally with 90% coverage.
  ind_pal <- c(rep(1, 9), rep(0, 16)) # VHR (<8 months)
  ind_mabs <- c(rep(1, 1), rep(0, 24)) # Birth only (0 months only)
  G_plus <- c(1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

  require(triangle)

  pal_eff <- rweibull(1250, 12.4311, 0.772251)
  mab_eff <- rtriangle(1250, 0.496, 0.871, 0.745) # efficacy of long-acting mabs
  mab_0_3_eff <- rtriangle(1250, 0.03, 0.828, 0.588) # efficacy of long-acting mabs
  mab_3p_eff <- rtriangle(1250, 0.696, 0.988, 0.922) # efficacy of long-acting mabs

  # VHR, HR and LR at birth during the winter
  make_vac_program_info_none <- function(seed) {
    list(
        pal = list(id = TRUE, age_id = ind_pal, t_start = 0, t_end = 0, eff = pal_eff[seed], cov = 0.0)
      )
  }

  make_vac_program_info_pal <- function(seed) {
    list(
        pal = list(id = TRUE, age_id = ind_pal, t_start = 15 * 7, t_end = 32 * 7, eff = pal_eff[seed], cov = 0.9)
      )
  }

  make_vac_program_info_mabs_vhr_seasonal <- function(seed) {
    list(
        mAB_VHR = list(id = TRUE, age_id = ind_pal, t_start = 15 * 7, t_end = 32 * 7, eff = mab_eff[seed], cov = 0.9)
      )
  }

  make_vac_program_info_mabs_seasonal <- function(seed) {
    list(
        mAB_VHR = list(id = TRUE, age_id = ind_pal, t_start = 13 * 7, t_end = 34 * 7, eff = mab_eff[seed], cov = 0.9),
        mAB_HR =  list(id = TRUE, catchup = FALSE, catchupnip = FALSE, age_id = ind_mabs, t_start = 13 * 7, t_end = 34 * 7, eff = mab_eff[seed], cov = 0.9),
        mAB_LR =  list(id = TRUE, catchup = FALSE, catchupnip = FALSE, age_id = ind_mabs, t_start = 13 * 7, t_end = 34 * 7, eff = mab_eff[seed], cov = 0.9)
      )
  }

  make_vac_program_info_mabs_year_round <- function(seed) {
    list(
        mAB_VHR = list(id = TRUE, age_id = ind_pal, t_start = 0 * 7, t_end = 52 * 7, eff = mab_eff[seed], cov = 0.9),
        mAB_HR =  list(id = TRUE, catchup = FALSE, catchupnip = FALSE, age_id = ind_mabs, t_start = 0, t_end = 52 * 7, eff = mab_eff[seed], cov = 0.9),
        mAB_LR =  list(id = TRUE, catchup = FALSE, catchupnip = FALSE, age_id = ind_mabs, t_start = 0, t_end = 52 * 7, eff = mab_eff[seed], cov = 0.9)
      )
  }

  make_vac_program_info_mabs_seasonal_catchup <- function(seed) {
    list(
        mAB_VHR = list(id = TRUE, age_id = ind_pal, t_start = 13 * 7, t_end = 34 * 7, eff = mab_eff[seed], cov = 0.9),
        mAB_HR =  list(id = TRUE, catchup = TRUE, catchupnip = FALSE, age_id_catchup = G_plus, age_id = ind_mabs, t_start = 13 * 7, t_end = 34 * 7, eff = mab_eff[seed], cov = 0.9),
        mAB_LR =  list(id = TRUE, catchup = TRUE, catchupnip = FALSE, age_id_catchup = G_plus, age_id = ind_mabs, t_start = 13 * 7, t_end = 34 * 7, eff = mab_eff[seed], cov = 0.9)
      )
  }


  make_vac_program_info_mabs_seasonal_catchup_nip <- function(seed) {
    list(
        mAB_VHR = list(id = TRUE, age_id = ind_pal, t_start = 9 * 7, t_end = 34 * 7, eff = mab_eff[seed], cov = 0.9),
        mAB_HR =  list(id = TRUE, catchup = FALSE, catchupnip = TRUE, age_id = ind_mabs, t_start = 9 * 7, t_end = 34 * 7, eff = mab_eff[seed], cov = 0.9),
        mAB_LR =  list(id = TRUE, catchup = FALSE, catchupnip = TRUE, age_id = ind_mabs, t_start = 9 * 7, t_end = 34 * 7, eff = mab_eff[seed], cov = 0.9)
      )
  }

  ## Different assumptions about the duration of protection
  vac_par_info <- list(om_mab = 1 / 150, direct = FALSE, xi_boost = 1)

  data_list <- 
    list(
      none = make_vac_program_info_none, 
      pal = make_vac_program_info_pal, 
      vhr_seasonal = make_vac_program_info_mabs_vhr_seasonal,
      seasonal = make_vac_program_info_mabs_seasonal,
      year_round = make_vac_program_info_mabs_year_round, 
      seasonal_catchup = make_vac_program_info_mabs_seasonal_catchup, 
      seasonal_catchup_nip = make_vac_program_info_mabs_seasonal_catchup_nip,
      vac_par_info = vac_par_info,
      pal_eff = pal_eff,
      mab_eff = mab_eff,
      mab_0_3_eff = mab_0_3_eff,
      mab_3p_eff = mab_3p_eff,
      post = post,
      seeds = seeds,
      S = 100
  )
  save(data_list, file = here::here("data", "nmab", "data_list.RData"))
}


run_icer_base <- function(data_list, rerun = FALSE) {

  source("R/cea.R")

  if (rerun) {
    seeds <- data_list$seeds
    post <- data_list$post
    vac_par_info <- data_list$vac_par_info
    S <- data_list$S

    output_default_none <- run_sample_custom(seeds[1:S], data_list$none, vac_par_info, 0, post)
    output_default_base <- run_sample_custom(seeds[1:S], data_list$pal, vac_par_info, 0, post)
    output_season_vhr_base <- run_sample_custom(seeds[1:S], data_list$vhr_seasonal, vac_par_info, 0, post)
    output_season_base <- run_sample_custom(seeds[1:S], data_list$seasonal, vac_par_info, 0, post)
    output_yr_base <- run_sample_custom(seeds[1:S], data_list$year_round, vac_par_info, 0, post)
    output_season_catchup_base <- run_sample_custom(seeds[1:S], data_list$seasonal_catchup, vac_par_info, 0, post)
    output_season_catchup_nip_base <- run_sample_custom(seeds[1:S], data_list$seasonal_catchup_nip, vac_par_info, 0, post)

    save(output_default_none, file = here("outputs",  "impact", "base", "none.RData"))
    save(output_default_base, file = here("outputs",  "impact", "base", "status_quo_base.RData"))
    save(output_season_vhr_base, file = here("outputs", "impact", "base", "output_season_vhr_base.RData"))
    save(output_season_base, file = here("outputs",  "impact", "base", "output_season_base.RData"))
    save(output_yr_base, file = here("outputs", "impact", "base", "output_yr_base.RData"))
    save(output_season_catchup_base, file = here("outputs",  "impact", "base",  "output_season_catchup_base.RData"))
    save(output_season_catchup_nip_base, file = here("outputs",  "impact", "base", "output_season_catchup_nip_base.RData"))

    list_outputs <- list(output_default_base, output_season_vhr_base, output_season_base,
      output_season_catchup_base, output_season_catchup_nip_base, output_yr_base)
    list_names <- list("pal", "vhr", "seasonal",
      "seasonal_and_catchup", "seasonal_and_catchup_nip", "year_round")
    names(list_outputs) <- list_names
    save(list_outputs, file = here::here("outputs", "impact", paste0("base", "_sum.RData")))
    
  } else {
    load(file = here("outputs", "impact", "base", "none.RData")) # output_default_none
    load(file = here("outputs", "impact", "base", "status_quo_base.RData")) # output_default_base
    load(file = here("outputs",  "impact", "base", "output_season_vhr_base.RData")) # output_season_vhr_base
    load(file = here("outputs",  "impact", "base", "output_season_base.RData")) # output_season_base
    load(file = here("outputs",  "impact", "base", "output_yr_base.RData")) # output_yr_base
    load(file = here("outputs",  "impact", "base", "output_season_catchup_base.RData")) # output_season_catchup_base
    load(file = here("outputs",  "impact", "base", "output_season_catchup_nip_base.RData")) # output_season_catchup_nip_base

    ## Look at estimating the dominance from these programmes
    list_outputs <- list(output_default_base, output_season_vhr_base, output_season_base,
      output_season_catchup_base, output_season_catchup_nip_base, output_yr_base)
    list_names <- list("pal", "vhr", "seasonal",
      "seasonal_and_catchup", "seasonal_and_catchup_nip", "year_round")
    names(list_outputs) <- list_names
    save(list_outputs, file = here::here("outputs",  "impact", paste0("base", "_sum.RData")))
  }

}

run_icer_base_direct <- function(data_list, rerun = FALSE) {

  if (rerun) {
    seeds <- data_list$seeds
    post <- data_list$post
    vac_par_info <- data_list$vac_par_info
    S <- data_list$S
    vac_par_info$direct <- TRUE 

    output_default_none_d <- run_sample_custom(seeds[1:S], data_list$none, vac_par_info, 0, post)
    output_default_base_d <- run_sample_custom(seeds[1:S], data_list$pal, vac_par_info, 0, post)
    output_season_vhr_base_d <- run_sample_custom(seeds[1:S], data_list$vhr_seasonal, vac_par_info, 0, post)
    output_season_base_d <- run_sample_custom(seeds[1:S], data_list$seasonal, vac_par_info, 0, post)
    output_yr_base_d <- run_sample_custom(seeds[1:S], data_list$year_round, vac_par_info, 0, post)
    output_season_catchup_base_d <- run_sample_custom(seeds[1:S], data_list$seasonal_catchup, vac_par_info, 0, post)
    output_season_catchup_nip_base_d <- run_sample_custom(seeds[1:S], data_list$seasonal_catchup_nip, vac_par_info, 0, post)

    save(output_default_none_d, file = here("outputs",  "impact", "base", "none_d.RData"))
    save(output_default_base_d, file = here("outputs",  "impact", "base", "status_quo_base_d.RData"))
    save(output_season_vhr_base_d, file = here("outputs",  "impact", "base", "output_season_vhr_base_d.RData"))
    save(output_season_base_d, file = here("outputs",  "impact", "base", "output_season_base_d.RData"))
    save(output_yr_base_d, file = here("outputs",  "impact", "base", "output_yr_base_d.RData"))
    save(output_season_catchup_base_d, file = here("outputs",  "impact", "base",  "output_season_catchup_base_d.RData"))
    save(output_season_catchup_nip_base_d, file = here("outputs",  "impact", "base", "output_season_catchup_nip_base_d.RData"))
  
    list_outputs <- list(output_default_base_d, output_season_vhr_base_d, output_season_base_d,
      output_season_catchup_base_d, output_season_catchup_nip_base_d, output_yr_base_d)
    list_names <- list("pal", "vhr", "seasonal",
      "seasonal_and_catchup", "seasonal_and_catchup_nip", "year_round")
    names(list_outputs) <- list_names
    save(list_outputs, file = here::here("outputs",  "impact", paste0("base_d", "_sum.RData")))
  
  } else {
    load(output_default_base_d, file = here("outputs",  "impact", "base", "status_quo_base_d.RData"))
    load(output_season_vhr_base_d, file = here("outputs",  "impact", "base", "output_season_vhr_base_d.RData"))
    load(output_season_base_d, file = here("outputs",  "impact", "base", "output_season_base_d.RData"))
    load(output_yr_base_d, file = here("outputs",  "impact", "base", "output_yr_base_d.RData"))
    load(output_season_catchup_base_d, file = here("outputs",  "impact", "base",  "output_season_catchup_base_d.RData"))
    load(output_season_catchup_nip_base_d, file = here("outputs",  "impact", "base", "output_season_catchup_nip_base_d.RData"))
  
    list_outputs <- list(output_default_base_d, output_season_vhr_base_d, output_season_base_d,
      output_season_catchup_base_d, output_season_catchup_nip_base_d, output_yr_base_d)
    list_names <- list("pal", "vhr", "seasonal",
      "seasonal_and_catchup", "seasonal_and_catchup_nip", "year_round")
    names(list_outputs) <- list_names
    save(list_outputs, file = here::here("outputs",  "impact", paste0("base_d", "_sum.RData")))
  }
}

run_icer_mabs_dur <- function(datalist, rerun = FALSE) {
  source("R/cea.R")
  load(file = here("outputs",  "impact", "base", "none.RData")) # output_default_none
  load(file = here("outputs",  "impact", "base", "status_quo_base.RData")) # output_default_base

  if (rerun) {

    seeds <- datalist$seeds
    post <- datalist$post
    S <- datalist$S

    vac_par_info_d_100 <- list(om_mab = 1 / 100, direct = FALSE, xi_boost = 1)

    output_season_vhr_d_100 <- run_sample_custom(seeds[1:S], datalist$vhr_seasonal, vac_par_info_d_100, 0, post)
    output_season_d_100 <- run_sample_custom(seeds[1:S], datalist$seasonal, vac_par_info_d_100, 0, post)
    output_yr_d_100 <- run_sample_custom(seeds[1:S], datalist$year_round, vac_par_info_d_100, 0, post)
    output_season_catchup_d_100 <- run_sample_custom(seeds[1:S], datalist$seasonal_catchup, vac_par_info_d_100, 0, post)
    output_season_catchup_nip_d_100 <- run_sample_custom(seeds[1:S], datalist$seasonal_catchup_nip, vac_par_info_d_100, 0, post)

    save(output_season_vhr_d_100, file = here("outputs",  "impact", "mabs_dur", "output_season_vhr_d_100.RData"))
    save(output_season_d_100, file = here("outputs",  "impact", "mabs_dur", "output_season_d_100.RData"))
    save(output_yr_d_100, file = here("outputs",  "impact", "mabs_dur", "output_yr_d_100.RData"))
    save(output_season_catchup_d_100, file = here("outputs",  "impact", "mabs_dur", "output_season_catchup_d_100.RData"))
    save(output_season_catchup_nip_d_100, file = here("outputs",  "impact", "mabs_dur", "output_season_catchup_nip_d_100.RData"))

    list_outputs <- list(output_default_base, output_season_vhr_d_100, output_season_d_100,
    output_season_catchup_d_100, output_season_catchup_nip_d_100, output_yr_d_100)
    list_names <- list("pal", "vhr", "seasonal",
      "seasonal_and_catchup", "seasonal_and_catchup_nip", "year_round")
    names(list_outputs) <- list_names
    save(list_outputs, file = here::here("outputs",  "impact", paste0("d_100", "_sum.RData")))


    seeds <- datalist$seeds
    post <- datalist$post
    S <- datalist$S

    vac_par_info_d_250 <- list(om_mab = 1 / 250, direct = FALSE, xi_boost = 1)

    output_season_vhr_d_250 <- run_sample_custom(seeds[1:S], datalist$vhr_seasonal, vac_par_info_d_250, 0, post)
    output_season_d_250 <- run_sample_custom(seeds[1:S], datalist$seasonal, vac_par_info_d_250, 0, post)
    output_yr_d_250 <- run_sample_custom(seeds[1:S], datalist$year_round, vac_par_info_d_250, 0, post)
    output_season_catchup_d_250 <- run_sample_custom(seeds[1:S], datalist$seasonal_catchup, vac_par_info_d_250, 0, post)
    output_season_catchup_nip_d_250 <- run_sample_custom(seeds[1:S], datalist$seasonal_catchup_nip, vac_par_info_d_250, 0, post)

    save(output_season_vhr_d_250, file = here("outputs",  "impact", "mabs_dur", "output_season_vhr_d_250.RData"))
    save(output_season_d_250, file = here("outputs",  "impact", "mabs_dur", "output_season_d_250.RData"))
    save(output_yr_d_250, file = here("outputs",  "impact", "mabs_dur", "output_yr_d_250.RData"))
    save(output_season_catchup_d_250, file = here("outputs",  "impact", "mabs_dur", "output_season_catchup_d_250.RData"))
    save(output_season_catchup_nip_d_250, file = here("outputs",  "impact", "mabs_dur", "output_season_catchup_nip_d_250.RData"))
      
    list_outputs <- list(output_default_base, output_season_vhr_d_250, output_season_d_250,
    output_season_catchup_d_250, output_season_catchup_nip_d_250, output_yr_d_250)
    list_names <- list("pal", "vhr", "seasonal",
      "seasonal_and_catchup", "seasonal_and_catchup_nip", "year_round")
    names(list_outputs) <- list_names
    save(list_outputs, file = here::here("outputs",  "impact", paste0("d_250", "_sum.RData")))

    vac_par_info_d_360 <- list(om_mab = 1 / 360, direct = FALSE, xi_boost = 1)

    output_season_vhr_d_360 <- run_sample_custom(seeds[1:S],  datalist$vhr_seasonal, vac_par_info_d_360, 0, post)
    output_season_d_360 <- run_sample_custom(seeds[1:S],  datalist$seasonal, vac_par_info_d_360, 0, post)
    output_yr_d_360 <- run_sample_custom(seeds[1:S], datalist$year_round, vac_par_info_d_360, 0, post)
    output_season_catchup_d_360 <- run_sample_custom(seeds[1:S], datalist$seasonal_catchup, vac_par_info_d_360, 0, post)
    output_season_catchup_nip_d_360 <- run_sample_custom(seeds[1:S],  datalist$seasonal_catchup_nip, vac_par_info_d_360, 0, post)

    save(output_season_vhr_d_360, file = here("outputs",  "impact", "mabs_dur","output_season_vhr_d_360.RData"))
    save(output_season_d_360, file = here("outputs",  "impact", "mabs_dur","output_season_d_360.RData"))
    save(output_yr_d_360, file = here("outputs",  "impact", "mabs_dur","output_yr_d_360.RData"))
    save(output_season_catchup_d_360, file = here("outputs",  "impact", "mabs_dur", "output_season_catchup_d_360.RData"))
    save(output_season_catchup_nip_d_360, file = here("outputs",  "impact", "mabs_dur","output_season_catchup_nip_d_360.RData"))

    list_outputs <- list(output_default_base, output_season_vhr_d_360, output_season_d_360,
    output_season_catchup_d_360, output_season_catchup_nip_d_360, output_yr_d_360)
    list_names <-  list("pal", "vhr", "seasonal",
      "seasonal_and_catchup", "seasonal_and_catchup_nip", "year_round")
    names(list_outputs) <- list_names
    save(list_outputs, file = here::here("outputs",  "impact", paste0("d_360", "_sum.RData")))
    
  } else {
    load(file = here("outputs",  "impact", "mabs_dur", "output_season_vhr_d_250.RData"))
    load(file = here("outputs",  "impact", "mabs_dur", "output_season_d_250.RData"))
    load(file = here("outputs",  "impact", "mabs_dur", "output_yr_d_250.RData"))
    load(file = here("outputs",  "impact", "mabs_dur", "output_season_catchup_d_250.RData"))
    load(file = here("outputs",  "impact", "mabs_dur", "output_season_catchup_nip_d_250.RData"))

    list_outputs <- list(output_default_base, output_season_vhr_d_250, output_season_d_250,
    output_season_catchup_d_250, output_season_catchup_nip_d_250, output_yr_d_250)
    list_names <- list("pal", "vhr", "seasonal",
      "seasonal_and_catchup", "seasonal_and_catchup_nip", "year_round")
    names(list_outputs) <- list_names
    save(list_outputs, file = here::here("outputs",  "impact", paste0("d_250", "_sum.RData")))

    load(file = here("outputs",  "impact", "mabs_dur","output_season_vhr_d_360.RData"))
    load(file = here("outputs",  "impact", "mabs_dur","output_season_d_360.RData"))
    load(file = here("outputs",  "impact", "mabs_dur","output_yr_d_360.RData"))
    load(file = here("outputs",  "impact", "mabs_dur", "output_season_catchup_d_360.RData"))
    load(file = here("outputs",  "impact", "mabs_dur","output_season_catchup_nip_d_360.RData"))

    list_outputs <- list(output_default_base, output_season_vhr_d_360, output_season_d_360,
    output_season_catchup_d_360, output_season_catchup_nip_d_360, output_yr_d_360)
    list_names <-  list("pal", "vhr", "seasonal",
      "seasonal_and_catchup", "seasonal_and_catchup_nip", "year_round")
    names(list_outputs) <- list_names
    save(list_outputs, file = here::here("outputs",  "impact", paste0("d_360", "_sum.RData")))
  }
}

run_icer_coverage <- function(datalist, coverage, rerun = FALSE) {

    source("R/cea.R")
    load(file = here("outputs",  "impact", "base", "none.RData")) # output_default_none
    load(file = here("outputs",  "impact", "base", "status_quo_base.RData")) # output_default_base
    if (rerun) {
      seeds <- datalist$seeds
      post <- datalist$post
      S <- datalist$S
      vac_par_info <- datalist$vac_par_info

      pal_eff <- datalist$pal_eff
      mab_eff <- datalist$mab_eff

      ind_pal <- c(rep(1, 9), rep(0, 16)) # VHR (<8 months)
      ind_mabs <- c(rep(1, 1), rep(0, 24)) # Birth only (0 months only)
      G_plus <- c(1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    
  
      make_vac_program_info_mabs_vhr_seasonal <- function(seed) {
          list(
              mAB_VHR = list(id = TRUE, age_id = ind_pal, t_start = 15 * 7, t_end = 32 * 7, eff = mab_eff[seed], cov = coverage)
          )
      }

      make_vac_program_info_mabs_seasonal_low_cov <- function(seed) {
          list(
              mAB_VHR = list(id = TRUE, age_id = ind_pal, t_start = 13 * 7, t_end = 34 * 7, eff = mab_eff[seed], cov = coverage),
              mAB_HR =  list(id = TRUE, catchup = FALSE, catchupnip = FALSE, age_id = ind_mabs, t_start = 13 * 7, t_end = 34 * 7, eff = mab_eff[seed], cov = coverage),
              mAB_LR =  list(id = TRUE, catchup = FALSE, catchupnip = FALSE, age_id = ind_mabs, t_start = 13 * 7, t_end = 34 * 7, eff = mab_eff[seed], cov = coverage)
          )
      }

      make_vac_program_info_mabs_year_round_low_cov <- function(seed) {
          list(
              mAB_VHR = list(id = TRUE, age_id = ind_pal, t_start = 0 * 7, t_end = 52 * 7, eff = mab_eff[seed], cov = 0.9),
              mAB_HR =  list(id = TRUE, catchup = FALSE, catchupnip = FALSE, age_id = ind_mabs, t_start = 0, t_end = 52 * 7, eff = mab_eff[seed], cov = coverage),
              mAB_LR =  list(id = TRUE, catchup = FALSE, catchupnip = FALSE, age_id = ind_mabs, t_start = 0, t_end = 52 * 7, eff = mab_eff[seed], cov = coverage)
          )
      }

      make_vac_program_info_mabs_seasonal_catchup_low_cov <- function(seed) {
          list(
              mAB_VHR = list(id = TRUE, age_id = ind_pal, t_start = 13 * 7, t_end = 34 * 7, eff = mab_eff[seed], cov = 0.9),
              mAB_HR =  list(id = TRUE, catchup = TRUE, catchupnip = FALSE, age_id_catchup = G_plus, age_id = ind_mabs, t_start = 13 * 7, t_end = 34 * 7, eff = mab_eff[seed], cov = coverage),
              mAB_LR =  list(id = TRUE, catchup = TRUE, catchupnip = FALSE, age_id_catchup = G_plus, age_id = ind_mabs, t_start = 13 * 7, t_end = 34 * 7, eff = mab_eff[seed], cov = coverage)
          )
      }


      make_vac_program_info_mabs_seasonal_catchup_nip_low_cov <- function(seed) {
          list(
              mAB_VHR = list(id = TRUE, age_id = ind_pal, t_start = 9 * 7, t_end = 34 * 7, eff = mab_eff[seed], cov = 0.9),
              mAB_HR =  list(id = TRUE, catchup = FALSE, catchupnip = TRUE, age_id = ind_mabs, t_start = 9 * 7, t_end = 34 * 7, eff = mab_eff[seed], cov = coverage),
              mAB_LR =  list(id = TRUE, catchup = FALSE, catchupnip = TRUE, age_id = ind_mabs, t_start = 9 * 7, t_end = 34 * 7, eff = mab_eff[seed], cov = coverage)
          )
      }

      output_season_vhr_low_cov <- run_sample_custom(seeds[1:S], make_vac_program_info_mabs_vhr_seasonal, vac_par_info, 0, post)
      output_season_low_cov <- run_sample_custom(seeds[1:S], make_vac_program_info_mabs_seasonal_low_cov, vac_par_info, 0, post)
      output_yr_low_cov <- run_sample_custom(seeds[1:S], make_vac_program_info_mabs_year_round_low_cov, vac_par_info, 0, post)
      output_season_catchup_low_cov <- run_sample_custom(seeds[1:S], make_vac_program_info_mabs_seasonal_catchup_low_cov, vac_par_info, 0, post)
      output_season_catchup_nip_low_cov <- run_sample_custom(seeds[1:S], make_vac_program_info_mabs_seasonal_catchup_nip_low_cov, vac_par_info, 0, post)

      save(output_season_vhr_low_cov, file = here("outputs",  "impact", "coverage", "output_season_vhr_low_cov.RData"))
      save(output_season_low_cov, file = here("outputs",  "impact","coverage",  "output_season_low_cov.RData"))
      save(output_yr_low_cov, file = here("outputs",  "impact", "coverage", "output_yr_low_cov.RData"))
      save(output_season_catchup_low_cov, file = here("outputs",  "impact", "coverage", "output_season_catchup_low_cov.RData"))
      save(output_season_catchup_nip_low_cov, file = here("outputs",  "impact", "coverage", "output_season_catchup_nip_low_cov.RData"))

      list_outputs <- list(output_default_base, output_season_vhr_low_cov, output_season_low_cov,
          output_season_catchup_low_cov, output_season_catchup_nip_low_cov, output_yr_low_cov)
      list_names <-  list("pal", "vhr", "seasonal",
      "seasonal_and_catchup", "seasonal_and_catchup_nip", "year_round")
      names(list_outputs) <- list_names
      save(list_outputs, file = here::here("outputs",  "impact", paste0("low_cov", "_sum.RData")))

    } else {
      load(file = here("outputs",  "impact", "coverage", "output_season_vhr_low_cov.RData"))
      load(file = here("outputs",  "impact","coverage",  "output_season_low_cov.RData"))
      load(file = here("outputs",  "impact", "coverage", "output_yr_low_cov.RData"))
      load(file = here("outputs",  "impact", "coverage", "output_season_catchup_low_cov.RData"))
      load(file = here("outputs",  "impact", "coverage", "output_season_catchup_nip_low_cov.RData"))

      list_outputs <- list(output_default_base, output_season_vhr_low_cov, output_season_low_cov,
          output_season_catchup_low_cov, output_season_catchup_nip_low_cov, output_yr_low_cov)
      list_names <-  list("pal", "vhr", "seasonal",
      "seasonal_and_catchup", "seasonal_and_catchup_nip", "year_round")
      names(list_outputs) <- list_names
      save(list_outputs, file = here::here("outputs",  "impact", paste0("low_cov", "_sum.RData")))
    }
}

run_icer_admin_year <- function(datalist, ind_mabs, rerun = FALSE) {

  source("R/cea.R")
  load(file = here("outputs",  "impact", "base", "none.RData")) # output_default_none
  load(file = here("outputs",  "impact", "base", "status_quo_base.RData")) # output_default_base

  if (rerun) {
    seeds <- datalist$seeds
    post <- datalist$post
    S <- datalist$S
    vac_par_info <- datalist$vac_par_info

    pal_eff <- datalist$pal_eff
    mab_eff <- datalist$mab_eff

    ind_pal <- c(rep(1, 9), rep(0, 16)) # VHR (<8 months)
    G_plus <- c(1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)


    make_vac_program_info_mabs_vhr_seasonal <- function(seed) {
      list(
          mAB_VHR = list(id = TRUE, age_id = ind_pal, t_start = 15 * 7, t_end = 32 * 7, eff = mab_eff[seed], cov = 0.9)
        )
    }

    make_vac_program_info_mabs_seasonal_2mo <- function(seed) {
      list(
          mAB_VHR = list(id = TRUE, age_id = ind_pal, t_start = 13 * 7, t_end = 34 * 7, eff = mab_eff[seed], cov = 0.9),
          mAB_HR =  list(id = TRUE, catchup = FALSE, catchupnip = FALSE, age_id = ind_mabs, t_start = 13 * 7, t_end = 34 * 7, eff = mab_eff[seed], cov = 0.9),
          mAB_LR =  list(id = TRUE, catchup = FALSE, catchupnip = FALSE, age_id = ind_mabs, t_start = 13 * 7, t_end = 34 * 7, eff = mab_eff[seed], cov = 0.9)
          )
    }

    make_vac_program_info_mabs_year_round_2mo <- function(seed) {
      list(
          mAB_VHR = list(id = TRUE, age_id = ind_pal, t_start = 0 * 7, t_end = 52 * 7, eff = mab_eff[seed], cov = 0.9),
          mAB_HR =  list(id = TRUE, catchup = FALSE, catchupnip = FALSE, age_id = ind_mabs, t_start = 0, t_end = 52 * 7, eff = mab_eff[seed], cov = 0.9),
          mAB_LR =  list(id = TRUE, catchup = FALSE, catchupnip = FALSE, age_id = ind_mabs, t_start = 0, t_end = 52 * 7, eff = mab_eff[seed], cov = 0.9)
        )
    }

    make_vac_program_info_mabs_seasonal_catchup_2mo <- function(seed) {
      list(
          mAB_VHR = list(id = TRUE, age_id = ind_pal, t_start = 13 * 7, t_end = 34 * 7, eff = mab_eff[seed], cov = 0.9),
          mAB_HR =  list(id = TRUE, catchup = TRUE, catchupnip = FALSE, age_id_catchup = G_plus, age_id = ind_mabs, t_start = 13 * 7, t_end = 34 * 7, eff = mab_eff[seed], cov = 0.9),
          mAB_LR =  list(id = TRUE, catchup = TRUE, catchupnip = FALSE, age_id_catchup = G_plus, age_id = ind_mabs, t_start = 13 * 7, t_end = 34 * 7, eff = mab_eff[seed], cov = 0.9)
        )
    }


    make_vac_program_info_mabs_seasonal_catchup_nip_2mo <- function(seed) {
      list(
          mAB_VHR = list(id = TRUE, age_id = ind_pal, t_start = 9 * 7, t_end = 34 * 7, eff = mab_eff[seed], cov = 0.9),
          mAB_HR =  list(id = TRUE, catchup = FALSE, catchupnip = TRUE, age_id = ind_mabs, t_start = 9 * 7, t_end = 34 * 7, eff = mab_eff[seed], cov = 0.9),
          mAB_LR =  list(id = TRUE, catchup = FALSE, catchupnip = TRUE, age_id = ind_mabs, t_start = 9 * 7, t_end = 34 * 7, eff = mab_eff[seed], cov = 0.9)
        )
    }


    output_season_vhr_2mo <- run_sample_custom(seeds[1:S], make_vac_program_info_mabs_vhr_seasonal, vac_par_info, 0, post)
    output_season_2mo <- run_sample_custom(seeds[1:S], make_vac_program_info_mabs_seasonal_2mo, vac_par_info, 0, post)
    output_yr_2mo <- run_sample_custom(seeds[1:S], make_vac_program_info_mabs_year_round_2mo, vac_par_info, 0, post)
    output_season_catchup_2mo <- run_sample_custom(seeds[1:S], make_vac_program_info_mabs_seasonal_catchup_2mo, vac_par_info, 0, post)
    output_season_catchup_nip_2mo <- run_sample_custom(seeds[1:S], make_vac_program_info_mabs_seasonal_catchup_nip_2mo, vac_par_info, 0, post)

    save(output_season_vhr_2mo, file = here("outputs",  "impact", "admin_age", "output_season_vhr_2mov.RData"))
    save(output_season_2mo, file = here("outputs",  "impact","admin_age",  "output_season_2mo.RData"))
    save(output_yr_2mo, file = here("outputs",  "impact", "admin_age", "output_yr_2mo.RData"))
    save(output_season_catchup_2mo, file = here("outputs",  "impact", "admin_age", "output_season_catchup_2mo.RData"))
    save(output_season_catchup_nip_2mo, file = here("outputs",  "impact", "admin_age", "output_season_catchup_nip_2mo.RData"))

    list_outputs <- list(output_default_base, output_season_vhr_2mo, output_season_2mo,
      output_season_catchup_2mo, output_season_catchup_nip_2mo, output_yr_2mo)
    list_names <-  list("pal", "vhr", "seasonal",
      "seasonal_and_catchup", "seasonal_and_catchup_nip", "year_round")
    names(list_outputs) <- list_names
    save(list_outputs, file = here::here("outputs",  "impact", paste0("2mo", "_sum.RData")))

  } else {
    load(file = here("outputs",  "impact", "admin_age", "output_season_vhr_2mov.RData"))
    load(file = here("outputs",  "impact","admin_age",  "output_season_2mo.RData"))
    load(file = here("outputs",  "impact", "admin_age", "output_yr_2mo.RData"))
    load(file = here("outputs",  "impact", "admin_age", "output_season_catchup_2mo.RData"))
    load(file = here("outputs",  "impact", "admin_age", "output_season_catchup_nip_2mo.RData"))

    list_outputs <- list(output_default_base, output_season_vhr_2mo, output_season_2mo,
      output_season_catchup_2mo, output_season_catchup_nip_2mo, output_yr_2mo)
    list_names <-  list("pal", "vhr", "seasonal",
      "seasonal_and_catchup", "seasonal_and_catchup_nip", "year_round")
    names(list_outputs) <- list_names
    save(list_outputs, file = here::here("outputs",  "impact", paste0("2mo", "_sum.RData")))
  }
}



run_icer_age_eff <- function(datalist, rerun = FALSE) {

  source("R/cea.R")
  load(file = here("outputs",  "impact", "base", "none.RData")) # output_default_none
  load(file = here("outputs",  "impact", "base", "status_quo_base.RData")) # output_default_base

  if (rerun) {
    seeds <- datalist$seeds
    post <- datalist$post
    S <- datalist$S
    vac_par_info <- datalist$vac_par_info
    ind_mabs <- c(rep(1, 1), rep(0, 24)) # Birth only (0 months only)

    pal_eff <- datalist$pal_eff
    mab_0_3_eff <- datalist$mab_0_3_eff
    mab_3p_eff <- datalist$mab_3p_eff

    ind_pal <- c(rep(1, 9), rep(0, 16)) # VHR (<8 months)
    G_plus <- c(1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)


    make_vac_program_info_mabs_vhr_age_eff <- function(seed) {
      eff_vec <- c(mab_0_3_eff[seed],  mab_3p_eff[seed])
      list(
          mAB_VHR = list(id = TRUE, age_id = ind_pal, t_start = 15 * 7, t_end = 32 * 7, age_eff_ind = TRUE, eff = eff_vec, cov = 0.9)
        )
    }

    make_vac_program_info_mabs_seasonal_age_eff <- function(seed) {
      eff_vec <- c(mab_0_3_eff[seed],  mab_3p_eff[seed])
      list(
          mAB_VHR = list(id = TRUE, age_id = ind_pal, t_start = 13 * 7, t_end = 34 * 7, age_eff_ind = TRUE, eff = eff_vec, cov = 0.9),
          mAB_HR =  list(id = TRUE, catchup = FALSE, catchupnip = FALSE, age_id = ind_mabs, t_start = 13 * 7, t_end = 34 * 7, age_eff_ind = TRUE, eff = eff_vec, cov = 0.9),
          mAB_LR =  list(id = TRUE, catchup = FALSE, catchupnip = FALSE, age_id = ind_mabs, t_start = 13 * 7, t_end = 34 * 7, age_eff_ind = TRUE, eff = eff_vec, cov = 0.9)
          )
    }

    make_vac_program_info_mabs_year_round_age_eff <- function(seed) {
      eff_vec <- c(mab_0_3_eff[seed],  mab_3p_eff[seed])
      list(
          mAB_VHR = list(id = TRUE, age_id = ind_pal, t_start = 0 * 7, t_end = 52 * 7, eff = eff_vec, cov = 0.9),
          mAB_HR =  list(id = TRUE, catchup = FALSE, catchupnip = FALSE, age_id = ind_mabs, t_start = 0, t_end = 52 * 7, age_eff_ind = TRUE, eff = eff_vec, cov = 0.9),
          mAB_LR =  list(id = TRUE, catchup = FALSE, catchupnip = FALSE, age_id = ind_mabs, t_start = 0, t_end = 52 * 7, age_eff_ind = TRUE, eff = eff_vec, cov = 0.9)
        )
    }

    make_vac_program_info_mabs_seasonal_catchup_age_eff <- function(seed) {
      eff_vec <- c(mab_0_3_eff[seed],  mab_3p_eff[seed])

      list(
          mAB_VHR = list(id = TRUE, age_id = ind_pal, t_start = 13 * 7, t_end = 34 * 7, eff = eff_vec, cov = 0.9),
          mAB_HR =  list(id = TRUE, catchup = TRUE, catchupnip = FALSE, age_id_catchup = G_plus, age_id = ind_mabs, t_start = 13 * 7, t_end = 34 * 7, age_eff_ind = TRUE, eff = eff_vec, cov = 0.9),
          mAB_LR =  list(id = TRUE, catchup = TRUE, catchupnip = FALSE, age_id_catchup = G_plus, age_id = ind_mabs, t_start = 13 * 7, t_end = 34 * 7, age_eff_ind = TRUE, eff = eff_vec, cov = 0.9)
        )
    }


    make_vac_program_info_mabs_seasonal_catchup_nip_age_eff <- function(seed) {
      eff_vec <- c(mab_0_3_eff[seed],  mab_3p_eff[seed])

      list(
          mAB_VHR = list(id = TRUE, age_id = ind_pal, t_start = 9 * 7, t_end = 34 * 7, eff = eff_vec, cov = 0.9),
          mAB_HR =  list(id = TRUE, catchup = FALSE, catchupnip = TRUE, age_id = ind_mabs, t_start = 9 * 7, t_end = 34 * 7, age_eff_ind = TRUE, eff = eff_vec, cov = 0.9),
          mAB_LR =  list(id = TRUE, catchup = FALSE, catchupnip = TRUE, age_id = ind_mabs, t_start = 9 * 7, t_end = 34 * 7, age_eff_ind = TRUE, eff = eff_vec, cov = 0.9)
        )
    }

    output_season_vhr_age_eff <- run_sample_custom(seeds[1:S], make_vac_program_info_mabs_vhr_age_eff, vac_par_info, 0, post)
    output_season_age_eff <- run_sample_custom(seeds[1:S], make_vac_program_info_mabs_seasonal_age_eff, vac_par_info, 0, post)
    output_yr_age_eff <- run_sample_custom(seeds[1:S], make_vac_program_info_mabs_year_round_age_eff, vac_par_info, 0, post)
    output_season_catchup_age_eff <- run_sample_custom(seeds[1:S], make_vac_program_info_mabs_seasonal_catchup_age_eff, vac_par_info, 0, post)
    output_season_catchup_nip_age_eff <- run_sample_custom(seeds[1:S], make_vac_program_info_mabs_seasonal_catchup_nip_age_eff, vac_par_info, 0, post)

    save(output_season_vhr_age_eff, file = here("outputs",  "impact", "admin_age", "output_season_vhr_age_eff.RData"))
    save(output_season_age_eff, file = here("outputs",  "impact","admin_age",  "output_season_age_eff.RData"))
    save(output_yr_age_eff, file = here("outputs",  "impact", "admin_age", "output_yr_age_eff.RData"))
    save(output_season_catchup_age_eff, file = here("outputs",  "impact", "admin_age", "output_season_catchup_age_eff.RData"))
    save(output_season_catchup_nip_age_eff, file = here("outputs",  "impact", "admin_age", "output_season_catchup_nip_age_eff.RData"))

    list_outputs <- list(output_default_base, output_season_vhr_age_eff, output_season_age_eff,
      output_season_catchup_age_eff, output_season_catchup_nip_age_eff, output_yr_age_eff)
    list_names <-  list("pal", "vhr", "seasonal",
      "seasonal_and_catchup", "seasonal_and_catchup_nip", "year_round")
    names(list_outputs) <- list_names
    save(list_outputs, file = here::here("outputs",  "impact", paste0("age_eff", "_sum.RData")))

  #  ppd_2mo <- cal_impact_trim(list_outputs, list_names, threshold)
  #  save(ppd_2mo, file = here::here("outputs",  "icer", "2mo_icer.RData"))
  } else {
    load(file = here("outputs",  "impact", "admin_age", "output_season_vhr_age_eff.RData"))
    load(file = here("outputs",  "impact","admin_age",  "output_season_age_eff.RData"))
    load(file = here("outputs",  "impact", "admin_age", "output_yr_age_eff.RData"))
    load(file = here("outputs",  "impact", "admin_age", "output_season_catchup_age_eff.RData"))
    load(file = here("outputs",  "impact", "admin_age", "output_season_catchup_nip_age_eff.RData"))

    list_outputs <- list(output_default_base, output_season_vhr_age_eff, output_season_age_eff,
      output_season_catchup_age_eff, output_season_catchup_nip_age_eff, output_yr_age_eff)
    list_names <-  list("pal", "vhr", "seasonal",
      "seasonal_and_catchup", "seasonal_and_catchup_nip", "year_round")
    names(list_outputs) <- list_names
    save(list_outputs, file = here::here("outputs",  "impact", paste0("age_eff", "_sum.RData")))
  }
}