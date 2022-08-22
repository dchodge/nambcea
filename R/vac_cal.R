#' Generate a daily vaccination calendar for Palivizumab administration
#'
#' @param pal_info A list of parameters assocaited with Palivizumab administration
#' @return A list, first element is the when the doses are given, the second is when individuals seroconvert.
get_pal_calendar <- function(pal_info) { 
    calendar <- matrix(0, 365, 25)
    sero <- matrix(0, 365, 25)

    for (i in (pal_info$t_start):(pal_info$t_end)) {
        for (j in 1:25) {
        calendar[(i - 1) %% 365 + 1, j] <- pal_info$age_id[j] * pal_info$cov / 30.0
        sero[(i - 1) %% 365 + 1, j] <- calendar[(i - 1) %% 365 + 1, j] * pal_info$eff;
        }
    }
    list(calendar = calendar, sero = sero)

}

#' Generate a daily vaccination calendar for long-acting monnoclonal ab administration for VHR infants
#'
#' @param mab_vhr_info A list of parameters associated with long-acting monoclonal administration
#' @return A list, first element is the when the doses are given, the second is when individuals seroconvert.
get_vhr_mAB_calendar <- function(mab_vhr_info) { 

    if(is.null(mab_vhr_info$age_eff_ind)) {
        eff_age <- rep(mab_vhr_info$eff, 25)
    } else {
        eff_age <- c(rep(mab_vhr_info$eff[1], 3), rep(mab_vhr_info$eff[2], 22))
    }
    
    calendar <- matrix(0, 365, 25)
    sero <- matrix(0, 365, 25)

    for (i in (mab_vhr_info$t_start):(mab_vhr_info$t_end)) {
        for (j in 1:25) {
           calendar[(i - 1) %% 365 + 1, j] <- mab_vhr_info$age_id[j] * mab_vhr_info$cov / 30.0
           sero[(i - 1) %% 365 + 1, j] <- calendar[(i - 1) %% 365 + 1, j] * eff_age[j];
        }
    }
    list(calendar = calendar, sero = sero)
}

#' Generate a daily vaccination calendar for long-acting monnoclonal ab administration for HR or LR infants
#'
#' @param mab_info A list of parameters associated with long-acting monoclonal administration
#' @return A list, first element is the when the doses are given, the second is when individuals seroconvert.
get_mAB_calendar <- function(mab_info) { 

    if(is.null(mab_info$age_eff_ind)) {
        eff_age <- rep(mab_info$eff, 25)
    } else {
        eff_age <- c(rep(mab_info$eff[1], 3), rep(mab_info$eff[2], 22))
    }

    calendar <- matrix(0.0, 365, 25)
    sero <- matrix(0.0, 365, 25)

    for (i in (mab_info$t_start):(mab_info$t_end)) {
        for (j in 1:25) {
           calendar[(i - 1) %% 365 + 1, j] <- mab_info$age_id[j] * mab_info$cov / 30.0
        }
    }
    if (!is.null(mab_info$catchup)) {
        if (mab_info$catchup) {
            for (i in (mab_info$t_start):(mab_info$t_start + 28)) {
                for (j in 1:25) {
                    calendar[(i - 1) %% 365 + 1, j] <- mab_info$age_id_catchup[j] * mab_info$cov / 30.0
                }
            }
        }
    }
    if (!is.null(mab_info$catchupnip)) {
        if (mab_info$catchupnip) {
            for (i in (13 * 7):(13 * 7 + 30)) {
                for (j in 3:5) {
                    calendar[(i - 1) %% 365 + 1, j] <- 1 * mab_info$cov / 30.0
                }
            }
            for (i in (9 * 7):(9 * 7 + 30)) {
                calendar[(i - 1) %% 365 + 1, 5] <- 1 * mab_info$cov / 30.0
            }
            for (i in (4 * 7):(4 * 7 + 30)) {
                calendar[(i - 1) %% 365 + 1, 5] <- 1 * mab_info$cov / 30.0
            }        
            for (i in (0 * 7):(0 * 7 + 30)) {
                calendar[(i - 1) %% 365 + 1, 5] <- 1 * mab_info$cov / 30.0
            }
        }
    }
    for (i in 0:365) {
        for (j in 1:25) {
           sero[(i - 1) %% 365 + 1, j] <- calendar[(i - 1) %% 365 + 1, j] * eff_age[j];
        }
    }

    list(calendar = calendar, sero = sero)
}

#' Generate a daily vaccination calendar for normal vaccination administration for HR or LR infants
#'
#' @param lav_info A list of parameters associated with lav administration
#' @return A list, first element is the when the doses are given, the second is when individuals seroconvert.
get_LAV_calendar <- function(lav_info) { 
    
    calendar <- matrix(0, 365, 25)
    sero <- matrix(0, 365, 25)

    if (lav_info$uptake_type == 1)
    {
        for (i in 1:365)
            for (j in 1:25)
                calendar[(i - 1) %% 365 + 1, j] <- lav_info$uptake_daily[i] * lav_info$age_id[j] * lav_info$cov;
    }
    else if (lav_info$uptake_type == 2)
    {
        for (i in (lav_info$t_start):(lav_info$t_end))
            for (j in 1:25)
                calendar[(i - 1) %% 365 + 1, j] <- lav_info$age_id[j] * lav_info$cov / 30.0;
    } else {
        stop("Value for uptake_type not recognised.")
    }
    
    for (i in 1:365)
        for (j in 1:25)
            for(k in 1:30)
                sero[(i + k - 1) %% 365 + 1, j] <- sero[(i + k - 1) %% 365 + 1, j] +
                    dweibull(k, 2.42, 12.87) * calendar[(i - 1) %% 365 + 1, j] * lav_info$eff;
    
    list(calendar = calendar, sero = sero)

}

#' Generate a daily vaccination calendar for maternal vaccination administration.
#'
#' @param mat_info A list of parameters associated with maternal administration administration
#' @return A list, first element is the when the doses are given, the second is when individuals seroconvert.
get_mat_calendar <- function(mat_info) {

    calendar <- matrix(0, 365, 25)
    sero <- matrix(0, 365, 25)

    for (i in (mat_info$t_start):(mat_info$t_end))
        for (j in 1:25)
            calendar[(i - 1) %% 365 + 1, j] <- mat_info$age_id[j] / 365.0
    

    for(i in 1:365) {
        for(j in 1:25) {
            for(k in 1:30) {
                sero[(i + k - 1) %% 365 + 1, j] <- sero[(i + k - 1) %% 365 + 1, j] + dweibull(k, 2.42, 12.87) * calendar[(i - 1) %% 365 + 1, j] * mat_info$eff_mat;
            }
        }
        for(k in (8*7):(12*7)) {
            sero[(i + k - 1) %% 365 + 1, 1] <- sero[(i + k - 1) %% 365 + 1, 1] + dunif(k, 8 * 7, 12 * 7) * calendar[(i - 1) %% 365 + 1, 20] * mat_info$eff_inf * 365;
        }
    }
    list(calendar = calendar, sero = sero)
}

#' Function to convert a custom weekly uptake vector into a daily uptake calendar
#'
#' @param up_week_raw A vector of weekly uptake proportions
#' @param start_time Start week when administration occurs
#' @return A vector of the daily proportion of a target group which are vaccinated.
get_daily_uptake <- function(up_week_raw, start_time)
{
    up_week <- vector("numeric", length = 52)
    up_day <- vector("numeric", length = 365)
    for (k in (start_time):(start_time + 20)) { 
        pos_week <- (k - 1) %% 52 + 1
        pos_ist <- (k - start_time) + 1
        up_week[pos_week] <- up_week_raw[pos_ist]
    }
            
    for (i in 1:365) {
        pos_day <- (i - 1 + start_time) %% 365 + 1;
        up_day[pos_day] <- (up_week[((i / 7 + start_time) %% 52) + 1] - up_week[((i / 7 - 1 + start_time) %% 52) + 1]) / 7.0;
        if (up_day[pos_day] < 0)
            up_day[pos_day] <- 0
    }
    up_day
}

#' Function to check the custom entries of the vaccination calendars.
#'
#' @param info A list of paramter values
check_all_entries <- function(info) {
    if (is.null(info$age_id))
        stop("PARAMETER MISSING: Binary vector indicating target age groups not provided. (age_id)")
    if (is.null(info$t_start))
        stop("PARAMETER MISSING: Start time of vaccination programme not provided. (t_start)")
    if (is.null(info$t_end))
        stop("PARAMETER MISSING: End time of vaccination programme not provided. (t_end)")
    if (is.null(info$eff))
        stop("PARAMETER MISSING: Efficacy not provided. (eff)")
    if (is.null(info$cov))
        stop("PARAMETER MISSING: Coverage not provided. (cov)")
}

#' Function to check the custom entries, specific to maternal vaccination calendars.
#'
#' @param info A list of paramter values
check_mat_entries <- function(info) {
    if (is.null(info$age_id))
        stop("PARAMETER MISSING: Binary vector indicating target age groups not provided. (age_id)")
    if (is.null(info$t_start))
        stop("PARAMETER MISSING: Start time of vaccination programme not provided. (t_start)")
    if (is.null(info$t_end))
        stop("PARAMETER MISSING: End time of vaccination programme not provided. (t_end)")
    if (is.null(info$eff_inf))
        stop("PARAMETER MISSING: Efficacy for infants not provided. (eff_inf)")
    if (is.null(info$eff_mat))
        stop("PARAMETER MISSING: Efficacy for pregnant women not provided. (eff_mat)")
    if (is.null(info$cov))
        stop("PARAMETER MISSING: Coverage not provided. (cov)")
}

#' Function to check the custom entries, specific to long-acting monoclonal antibody calendars.
#'
#' @param info A list of paramter values
check_mab_entries <- function(info) {
    if (is.null(info$catchup))
        stop("PARAMETER MISSING: Must indicate boolean for including a catchup programme at start of season. (catchup)")
    else {
        if(info$catchup) {
            if (is.null(info$age_id_catchup))
                stop("PARAMETER MISSING: Must provide a binary vector indicating target age groups for the catchup programme. (age_id_catchup)")
        }
    }
}

#' Function to check the custom entries, specific to lav calendars.
#'
#' @param info A list of paramter value
check_lav_entries <- function(info) {
    if (is.null(info$age_id))
        stop("PARAMETER MISSING: Binary vector indicating target age groups not provided. (age_id)")
    if (is.null(info$uptake_daily))
        stop("PARAMETER MISSING: Daily uptake proportions missing. (uptake_daily)")
    if (is.null(info$eff))
        stop("PARAMETER MISSING: Efficacy not provided. (eff)")
    if (is.null(info$cov))
        stop("PARAMETER MISSING: Coverage not provided. (cov)")
}

#' Function to convert input parameters into a daily uptake calendar for each age group in the transmission model
#'
#' @param vac_program_info A list of details outlining the parmeters for each of the 7 calendars
#' @param metric The metric (default week)
#' @param cov_c The proportion of infants who are in the targeted group (equal to the coverage of maternal vaccination.)
#' @return Daily vaccination calendars for use in the dynamic transmission
create_calendar <- function(vac_program_info, metric = "week", cov_c) {
    # need to include week and month options
    all_cal <- list(
        pal = matrix(0, 365, 25),
        mAB_VHR = matrix(0, 365, 25),
        mAB_HR = matrix(0, 365, 25),
        mAB_LR = matrix(0, 365, 25),
        LAV_HR = matrix(0, 365, 25),
        LAV_LR = matrix(0, 365, 25),
        mat_LR = matrix(0, 365, 25)
    )   
    if(!is.null(vac_program_info$pal$id) && vac_program_info$pal$id) {
        check_all_entries(vac_program_info$pal)
        all_cal$pal <- get_pal_calendar(vac_program_info$pal)
    }
    if(!is.null(vac_program_info$mAB_VHR$id) && vac_program_info$mAB_VHR$id ) {
        check_all_entries(vac_program_info$mAB_VHR)
        all_cal$mAB_VHR <- get_vhr_mAB_calendar(vac_program_info$mAB_VHR)
    }
    if(!is.null(vac_program_info$mAB_HR$id) && vac_program_info$mAB_HR$id ) {
        check_all_entries(vac_program_info$mAB_HR)
        check_mab_entries(vac_program_info$mAB_HR)
        all_cal$mAB_HR <- get_mAB_calendar(vac_program_info$mAB_HR)
    }
    if(!is.null(vac_program_info$mAB_LR$id) && vac_program_info$mAB_LR$id ) {
        check_all_entries(vac_program_info$mAB_LR)
        check_mab_entries(vac_program_info$mAB_LR)
        all_cal$mAB_LR <- get_mAB_calendar(vac_program_info$mAB_LR)
    }
    if(!is.null(vac_program_info$LAV_HR$id) && vac_program_info$LAV_HR$id ) {
        if (vac_program_info$LAV_HR$uptake_type == 2) {
            check_all_entries(vac_program_info$LAV_HR)
        } else {
            check_lav_entries(vac_program_info$LAV_HR)
        }
        all_cal$LAV_HR <- get_LAV_calendar(vac_program_info$LAV_HR)
    }
    if(!is.null(vac_program_info$LAV_LR$id) && vac_program_info$LAV_LR$id) {
        if (vac_program_info$LAV_LR$uptake_type == 2) {
            check_all_entries(vac_program_info$LAV_LR)
        } else {
            check_lav_entries(vac_program_info$LAV_LR)
        }
        all_cal$LAV_LR <- get_LAV_calendar(vac_program_info$LAV_LR)
    }
    if(!is.null(vac_program_info$mat$id) && vac_program_info$mat$id ) { 
        check_mat_entries(vac_program_info$mat)
        all_cal$mat_LR <- get_mat_calendar(vac_program_info$mat)
    }


    list(all_cal = all_cal, cov_c = cov_c)
}

#' Function to get the serorevertion times given an output from create_calendar
#' 
#' @param output A list outputs from create_calendar
#' @return A list of matrices showing the seroconversion proportions for each of the seven vaccination calendars.
get_cal <- function(output) {
    names <- c("pal", "mAB_VHR", "mAB_HR", "mAB_LR", "LAV_HR", "LAV_LR", "mat_LR")
    out <- vector(mode = "list", length = length(output[[1]])) %>% setNames(names)
    j <- 1
    for (i in output[[1]]) {
        if (is.matrix(i))
            out[[j]] <- i
        else
            out[[j]] <- i[[2]]
        j <- j + 1
    }
    out
}

#' Function to get the dosing times given an output from create_calendar
#' 
#' @param output A list outputs from create_calendar
#' @return A list of matrices showing the dosing proportions for each of the seven vaccination calendars.
get_dose <- function(output) {
    names <- c("pal", "mAB_VHR", "mAB_HR", "mAB_LR", "LAV_HR", "LAV_LR", "mat_LR")
    out <- vector(mode = "list", length = length(output[[1]])) %>% setNames(names)
    j <- 1
    for (i in output[[1]]) {
        if (is.matrix(i))
            out[[j]] <- i
        else
            out[[j]] <- i[[1]]
        j <- j + 1
    }
    out
}

#' Function to run a custom vaccination calendar
#' 
#' @param seeds A vector of seed value (integers)
#' @param func_vac A function which generate the vaccination calendar given a seed value
#' @param vac_par_info A list of values which help define the intervention programmes
#' @param cov_c The proportion of infants who are in the targeted group (equal to the coverage of maternal vaccination.)
#' @return A list of two dataframes, the first shows the annual incidence for health outcomes, the second shows the economic metrics.
run_sample_custom <- function(seeds, func_vac, vac_par_info, cov_c, post, cost_imp, discount_rate = 0.03, disease_mult = 1) {
    QALY_list <- list(); cost_list <- list();
    outcomes_week_age_list <- list(); econmetric_list <- list(); outcomes_annual_list <- list(); i <- 1;
    for (seed in seeds) {
    
      vac_program_info_custom <- func_vac(seed)

      vac_cal <- create_calendar(vac_program_info_custom, "week", cov_c)
      cal <- get_cal(vac_cal)
      dose <- get_dose(vac_cal)
      out <- classRunInterventions$Sample(cal, dose, vac_cal[["cov_c"]],
        vac_par_info, as.matrix(post)[seed, ])
      outcomes_cea <- get_outcomes(out, as.matrix(post)[seed, ], cost_imp, discount_rate, seed, disease_mult)
      
      QALY_list[[i]] <- outcomes_cea$QALY %>% unique
      cost_list[[i]] <- outcomes_cea$cost %>% unique
      outcomes_week_age_list[[i]] <- outcomes_cea$outcomes_age_week

      cat("sample_no: ", i, "\n"); i <- i + 1;
    }
    list(
        QALY = bind_rows(QALY_list),
        cost = bind_rows(cost_list),
        outcomes_week_age = bind_rows(outcomes_week_age_list),
        vac_cal = out$doses,
        cal_pre = cal,
        dose_pre = dose
    )
}