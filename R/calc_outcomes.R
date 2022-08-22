#' Function to convert incidence to GP visits
#' 
#' @param inci A matrix of incidence in each age, social, and risk group.
#' @param seed An integer value
#' @param t_w Week no
#' @return The number of annual GP cases in each age group vector.
get_GP <- function(inci, t_w, seed) {
    set.seed(seed)

    # GET RISKS
    risk_lr <- vector(mode = "numeric", length = 25)

    risk_lr[1] <- rweibull(1, 788.190729, 0.065666)
    risk_lr[2] <- rweibull(1, 788.190729, 0.059147)
    risk_lr[3] <- rweibull(1, 788.190729, 0.058839)
    risk_lr[4] <- rweibull(1, 788.190729, 0.058752)
    risk_lr[5] <- rweibull(1, 788.190729, 0.058482)
    risk_lr[6] <- rweibull(1, 788.190729, 0.058244)

    risk_lr[7] <- rgamma(1, 383.596616, scale = 0.000017)
    risk_lr[8] <- rgamma(1, 383.596616, scale = 0.000017)
    risk_lr[9] <- rgamma(1, 383.596616, scale = 0.000018)
    risk_lr[10] <- rgamma(1, 383.596616, scale = 0.000018)
    risk_lr[11] <- rgamma(1, 383.596616, scale = 0.000019)
    risk_lr[12] <- rgamma(1, 383.596616, scale = 0.000019)
    risk_lr[13] <- rgamma(1, 383.600556, scale = 0.000017)
    risk_lr[14] <- rgamma(1, 383.600556, scale = 0.000021)
    risk_lr[15] <- rgamma(1, 383.600556, scale = 0.000023)
    risk_lr[16] <- rgamma(1, 383.600556, scale = 0.000025)

    risk_lr[17] <- rweibull(1, 5.159418, 0.018924)
    risk_lr[18] <- rweibull(1, 5.159418, 0.019388)

    risk_lr[19] <- rweibull(1, 7.620541, 0.014886)
    risk_lr[20] <- rweibull(1, 7.620541, 0.016555)
    risk_lr[21] <- rweibull(1, 7.620541, 0.015342)

    risk_lr[22] <- rweibull(1, 11.023472, 0.037348)
    risk_lr[23] <- rweibull(1, 11.023472, 0.055732)
    risk_lr[24] <- rweibull(1, 11.023472, 0.107717)
    risk_lr[25] <- rweibull(1, 11.023472, 0.137592)

    risk_vhr <- risk_hr <- risk_lr
    out_vhr <- (1:25 %>% map(~sum(inci[(c(1, 4, 7) + 9 * (.x - 1))])) %>% unlist) * risk_vhr
    out_hr <- (1:25 %>% map(~sum(inci[(c(2, 5, 8) + 9 * (.x - 1))])) %>% unlist) * risk_hr
    out_lr <- (1:25 %>% map(~sum(inci[(c(3, 6, 9) + 9 * (.x - 1))])) %>% unlist) * risk_lr

    data.frame(
        outcome = "gp_visits",
        seed = seed,
        time = t_w,
        age_group = 1:25,
        incidence = out_vhr + out_hr + out_lr
    )
}

#' Function to convert incidence to hospital visits
#' 
#' @param inci A matrix of incidence in each age, social, and risk group.
#' @param seed An integer value
#' @param t_w Week no
#' @return The number of annual hospital cases in each age group vector.
get_hosp <- function(inci, t_w, seed) {
    set.seed(seed)
    # GET RISKS
    risk_lr <- vector(mode = "numeric", length = 25)
    # 0-11 months
    risk_lr[1] <- rlnorm(1, -3.115625, 0.020926)
    risk_lr[2] <- rweibull(1, 62.227553, 0.094955)
    risk_lr[3] <- rlnorm(1, -2.639371, 0.021130)
    risk_lr[4] <- rlnorm(1, -2.956136, 0.027445)
    risk_lr[5] <- rlnorm(1, -3.103970, 0.034059)
    risk_lr[6] <- rlnorm(1, -3.300267, 0.033973)
    risk_lr[7] <- rlnorm(1, -3.501204, 0.040090)
    risk_lr[8] <- rlnorm(1, -3.683768, 0.039078)
    risk_lr[9] <- rlnorm(1, -3.874510, 0.051394)
    risk_lr[10] <- rlnorm(1, -4.018271, 0.058688)
    risk_lr[11] <- rlnorm(1, -4.274218, 0.056815)
    risk_lr[12] <- rlnorm(1, -4.405675, 0.090839)
    # 1 - 4 years
    risk_lr[13] <- rlnorm(1, -5.332327, 0.097721)
    risk_lr[14] <- rlnorm(1, -5.102881, 0.097721)
    risk_lr[15] <- rlnorm(1, -5.000518, 0.097721)
    risk_lr[16] <- rlnorm(1, -4.935537, 0.097721)

    for (a in 17:21) # 5-44 years
        risk_lr[a] <- rgamma(1, 60.924023, scale = 0.000001)

    # 45-64 years
    risk_lr[22] <- rweibull(1, 10.009425, 0.000849)
    risk_lr[23] <- rweibull(1, 10.009425, 0.001267)
    # 65 + years
    risk_lr[24] <- rgamma(1, 55.761551, scale = 0.000075)
    risk_lr[25] <- rgamma(1, 56.300550, scale = 0.000270)

    # 0–11 months
    risk_hr <- vector(mode = "numeric", length = 25)
    risk_hr[1] <- rlnorm(1, -2.828635, 0.020926)
    risk_hr[2] <- rweibull(1, 62.227553, 0.126519)
    risk_hr[3] <- rlnorm(1, -2.352380, 0.021130)
    risk_hr[4] <- rlnorm(1, -2.669146, 0.027445)
    risk_hr[5] <- rlnorm(1, -2.816981, 0.034059)
    risk_hr[6] <- rlnorm(1, -3.013278, 0.033973)
    risk_hr[7] <- rlnorm(1, -3.214214, 0.040090)
    risk_hr[8] <- rlnorm(1, -3.396781, 0.039078)
    risk_hr[9] <- rlnorm(1, -3.587518, 0.051394)
    risk_hr[10] <- rlnorm(1, -3.731282, 0.058688)
    risk_hr[11] <- rlnorm(1, -3.987226, 0.056815)
    risk_hr[12] <- rlnorm(1, -4.118684, 0.090839)

    risk_vhr <- vector(mode = "numeric", length = 25)
    risk_vhr[1] <- rgamma(1, 1707.23, scale = 0.000218917)
    risk_vhr[2] <- rgamma(1, 1707.23, scale = 0.000218917)
    risk_vhr[3] <- rgamma(1, 1707.23, scale = 0.000218917)
    risk_vhr[4] <- rgamma(1, 1097.73, scale = 0.000218973)
    risk_vhr[5] <- rgamma(1, 1097.73, scale = 0.000218973)
    risk_vhr[6] <- rgamma(1, 1097.73, scale = 0.000218973)
    risk_vhr[7] <- rgamma(1, 905.012, scale = 0.000156833)
    risk_vhr[8] <- rgamma(1, 905.012, scale = 0.000156833)
    risk_vhr[9] <- rgamma(1, 905.012, scale = 0.000156833)

    out_vhr <- (1:25 %>% map(~sum(inci[(c(1, 4, 7) + 9 * (.x - 1))])) %>% unlist) * risk_vhr
    out_hr <- (1:25 %>% map(~sum(inci[(c(2, 5, 8) + 9 * (.x - 1))])) %>% unlist) * risk_hr
    out_lr <- (1:25 %>% map(~sum(inci[(c(3, 6, 9) + 9 * (.x - 1))])) %>% unlist) * risk_lr

    data.frame(
        outcome = "hospital_cases",
        seed = seed,
        time = t_w,
        age_group = 1:25,
        incidence = out_vhr + out_hr + out_lr
    )

}

#' Function to convert incidence to the number of bed days
#' 
#' @param inci A matrix of incidence in each age, social, and risk group.
#' @param seed An integer value
#' @param t_w Week no
#' @return The number of annual bed days cases in each age group vector.
get_bd <- function(inci, t_w, seed) {
    set.seed(seed)

    risk_lr <- vector(mode = "numeric", length = 25)
    # 0-11 months
    risk_lr[1] <- rlnorm(1, -1.518030, 0.025948)
    risk_lr[2] <- rgamma(1, 3804.312368, scale = 0.000063)
    risk_lr[3] <- rlnorm(1, -1.815288, 0.015253)
    risk_lr[4] <- rlnorm(1, -2.235095, 0.019425)
    risk_lr[5] <- rweibull(1, 39.946103, 0.090392)
    risk_lr[6] <- rlnorm(1, -2.715636, 0.028779)
    risk_lr[7] <- rlnorm(1, -2.956000, 0.036121)
    risk_lr[8] <- rlnorm(1, -3.026522, 0.027760)
    risk_lr[9] <- rlnorm(1, -3.246425, 0.031848)
    risk_lr[10] <- rlnorm(1, -3.377888, 0.043004)
    risk_lr[11] <- rlnorm(1, -3.503136, 0.034244)
    risk_lr[12] <- rlnorm(1, -3.838841, 0.067924)
    # 1 - 4 years
    risk_lr[13] <- rlnorm(1, -4.639179, 0.097721)
    risk_lr[14] <- rlnorm(1, -4.409734, 0.097721)
    risk_lr[15] <- rlnorm(1, -4.307371, 0.097721)
    risk_lr[16] <- rlnorm(1, -4.242390, 0.097721)

    risk_lr[17] <- rgamma(1, 60.923051, scale = 0.000002)
    risk_lr[18] <- rgamma(1, 60.923051, scale = 0.000002)
    risk_lr[19] <- rgamma(1, 60.922448, scale = 0.000004)
    risk_lr[20] <- rgamma(1, 60.922448, scale = 0.000005)
    risk_lr[21] <- rgamma(1, 60.922448, scale = 0.000004)

    # 45-64 years
    risk_lr[22] <- rweibull(1, 10.009442, 0.002548)
    risk_lr[23] <- rweibull(1, 10.009442, 0.003802)
    # 65 + years
    risk_lr[24] <- rgamma(1, 55.759807, scale = 0.000226)
    risk_lr[25] <- rgamma(1, 56.300150, scale = 0.000809)

    # 0–11 months
    risk_hr <- vector(mode = "numeric", length = 25)
    risk_hr[1] <- rlnorm(1, 0.388474,0.025948)
    risk_hr[2] <- rgamma(1, 3804.674598, scale = 0.000422)
    risk_hr[3] <- rlnorm(1, 0.091217, 0.015253)
    risk_hr[4] <- rlnorm(1, -0.328591, 0.019425)
    risk_hr[5] <- rweibull(1, 39.946103, 0.608296)
    risk_hr[6] <- rlnorm(1, -0.809133,0.028779)
    risk_hr[7] <- rlnorm(1, -1.049497, 0.036121)
    risk_hr[8] <- rlnorm(1, -1.120021, 0.027760)
    risk_hr[9] <- rlnorm(1, -1.339918, 0.031848)
    risk_hr[10] <- rlnorm(1, -1.471385, 0.043004)
    risk_hr[11] <- rlnorm(1, -1.596630, 0.034244)
    risk_hr[12] <- rlnorm(1, -1.932337, 0.067924)

    risk_vhr <- vector(mode = "numeric", length = 25)
    risk_vhr[1] <- rgamma(1, 91.7501, scale = 0.0254)
    risk_vhr[2] <- rgamma(1, 91.7501, scale = 0.0254)
    risk_vhr[3] <- rgamma(1, 91.7501, scale = 0.0254)
    risk_vhr[4] <- rgamma(1, 87.2681, scale = 0.01718)
    risk_vhr[5] <- rgamma(1, 87.2681, scale = 0.01718)
    risk_vhr[6] <- rgamma(1, 87.2681, scale = 0.01718)
    risk_vhr[7] <- rgamma(1, 87.4392, scale = 0.01014)
    risk_vhr[8] <- rgamma(1, 87.4392, scale = 0.01014)
    risk_vhr[9] <- rgamma(1, 87.4392, scale = 0.01014)

    out_vhr <- (1:25 %>% map(~sum(inci[(c(1, 4, 7) + 9 * (.x - 1))])) %>% unlist) * risk_vhr
    out_hr <- (1:25 %>% map(~sum(inci[(c(2, 5, 8) + 9 * (.x - 1))])) %>% unlist) * risk_hr
    out_lr <- (1:25 %>% map(~sum(inci[(c(3, 6, 9) + 9 * (.x - 1))])) %>% unlist) * risk_lr

    data.frame(
        outcome = "bed_days",
        seed = seed,
        time = t_w,
        age_group = 1:25,
        incidence = out_vhr + out_hr + out_lr
    )

}

#' Function to convert incidence to the number of deaths
#' 
#' @param inci A matrix of incidence in each age, social, and risk group.
#' @param seed An integer value
#' @param t_w Week no
#' @return The number of annual death cases in each age group vector.
get_death <- function(inci, t_w, seed) {
    set.seed(seed)
    risk_lr <- vector(mode = "numeric", length = 25)
    for (a in 1:6) # < 6 months
        risk_lr[a] <- rgamma(1, 2362.125146, scale = 0.00000001)
    for (a in 7:16) # 7 mo – 4 years
        risk_lr[a] <- rweibull(1, 9999.999999, scale = 0.000008)
    risk_lr[17] <- rlnorm(1, -11.908821, 1.3120344)
    risk_lr[18] <- rweibull(1, 1.309704, 0.000007)

    risk_lr[19] <- rlnorm(1, -12.275800, 1.077579)
    risk_lr[20] <- rlnorm(1, -12.169540, 1.077579)
    risk_lr[21] <- rlnorm(1, -12.245614, 1.077579)

    risk_lr[22] <- rweibull(1, 2.058097, 0.000130)
    risk_lr[23] <- rweibull(1, 2.058097, 0.000194)
    risk_lr[24] <- rweibull(1, 2.058097, 0.000286)
    risk_lr[25] <- rweibull(1, 2.908690, 0.002443)

    risk_hr <- risk_lr

    risk_vhr <- vector(mode = "numeric", length = 25)
    risk_vhr[1] <- rgamma(1, 8.31507, scale = 0.00166481)
    risk_vhr[2] <- rgamma(1, 8.31507, scale = 0.00166481)
    risk_vhr[3] <- rgamma(1, 8.31507, scale = 0.00166481)
    risk_vhr[4] <- rgamma(1, 8.18185, scale = 0.00108933)
    risk_vhr[5] <- rgamma(1, 8.18185, scale = 0.00108933)
    risk_vhr[6] <- rgamma(1, 8.18185, scale = 0.00108933)
    risk_vhr[7] <- rgamma(1, 8.27439, scale = 0.00063814)
    risk_vhr[8] <- rgamma(1, 8.27439, scale = 0.00063814)
    risk_vhr[9] <- rgamma(1, 8.27439, scale = 0.00063814)

    out_vhr <- (1:25 %>% map(~sum(inci[(c(1, 4, 7) + 9 * (.x - 1))])) %>% unlist) * risk_vhr
    out_hr <- (1:25 %>% map(~sum(inci[(c(2, 5, 8) + 9 * (.x - 1))])) %>% unlist) * risk_hr
    out_lr <- (1:25 %>% map(~sum(inci[(c(3, 6, 9) + 9 * (.x - 1))])) %>% unlist) * risk_lr

    data.frame(
        outcome = "deaths",
        seed = seed,
        time = t_w,
        age_group = 1:25,
        incidence = out_vhr + out_hr + out_lr
    )
}

get_Inc <- function(inci, t_w, seed) {

    out_vhr <- (1:25 %>% map(~sum(inci[(c(1, 4, 7) + 9 * (.x - 1))])) %>% unlist)
    out_hr <- (1:25 %>% map(~sum(inci[(c(2, 5, 8) + 9 * (.x - 1))])) %>% unlist) 
    out_lr <- (1:25 %>% map(~sum(inci[(c(3, 6, 9) + 9 * (.x - 1))])) %>% unlist)

    data.frame(
        outcome = "all_cases",
        seed = seed,
        time = t_w,
        age_group = 1:25,
        incidence = out_vhr + out_hr + out_lr
    )
}


#' Function to convert incidence to the number of symptomatic cases
#' 
#' @param inci A matrix of incidence in each age, social, and risk group.
#' @param seed An integer value
#' @param t_w Week no
#' @return The number of annual symptomatic cases in each age group vector.
get_S <- function(inci, posterior, t_w, seed) {
    set.seed(seed)
    risk_lr <- vector(mode = "numeric", length = 25)
    for (a in 1:12)
        risk_lr[a] <- (1 - posterior[["pA1"]])
    for (a in 13:16)
        risk_lr[a] <- (1 - posterior[["pA2"]])
    for (a in 17:18)
        risk_lr[a] <- (1 - posterior[["pA3"]])
    for (a in 19:25)
        risk_lr[a] <- (1 - posterior[["pA4"]])
    
    risk_vhr <- risk_hr <- risk_lr
    
    out_vhr <- (1:25 %>% map(~sum(inci[(c(1, 4, 7) + 9 * (.x - 1))])) %>% unlist) * risk_vhr
    out_hr <- (1:25 %>% map(~sum(inci[(c(2, 5, 8) + 9 * (.x - 1))])) %>% unlist) * risk_hr
    out_lr <- (1:25 %>% map(~sum(inci[(c(3, 6, 9) + 9 * (.x - 1))])) %>% unlist) * risk_lr

    data.frame(
        outcome = "symptomatic_cases",
        seed = seed,
        time = t_w,
        age_group = 1:25,
        incidence = out_vhr + out_hr + out_lr
    )
}

#' Function to convert incidence of outcomes into the QALY loss
#' 
#' @param symp A vector of number of symptomatic cases per age group
#' @param gp A vector of number of GP cases per age group
#' @param hosp A vector of number of hospital cases per age group
#' @param bd A vector of number of bed days per age group
#' @param death A vector of number of deaths per age group
#' @param seed An integer value
#' @return The total number of QALYs lost
get_QALY <- function(symp, gp, hosp, bd, death, seed) {
    set.seed(seed)
    nhseek_QALY <-  vector(mode = "numeric", length = 25)
    hseek_QALY <-  vector(mode = "numeric", length = 25)
    death_QALY <-  vector(mode = "numeric", length = 25)

    for (a in 1:12) 
        death_QALY[a] <- rnorm(1, 23.32, 23.32 * 0.1)

    death_QALY[13] <- rnorm(1, 23.2186, 23.2186 * 0.1)
    death_QALY[14] <- rnorm(1, 23.1135, 23.1135 * 0.1)
    death_QALY[15] <- rnorm(1, 23.0048, 23.0048 * 0.1)
    death_QALY[16] <- rnorm(1, 22.8921, 22.8921 * 0.1)
    death_QALY[17] <- rnorm(1, 22.5251, 22.5251 * 0.1)
    death_QALY[18] <- rnorm(1, 21.8287, 21.8287 * 0.1)
    death_QALY[19] <- rnorm(1, 20.7512, 20.7512 * 0.1)
    death_QALY[20] <- rnorm(1, 19.3996, 19.3996 * 0.1)
    death_QALY[21] <- rnorm(1, 17.616, 17.616 * 0.1)
    death_QALY[22] <- rnorm(1, 15.2231, 15.2231 * 0.1)
    death_QALY[23] <- rnorm(1, 12.038, 12.038 * 0.1)
    death_QALY[24] <- rnorm(1, 7.63045, 7.63045 * 0.1)
    death_QALY[25] <- rnorm(1, 3.04609, 3.04609 * 0.1)

    tot_Q_c <- 0
    tot_Q_d <- 0

    for (a in 1:16) {
        nhseek_QALY[a] <- rgamma(1, 1.6578, scale = 0.0018241) # non-healthcare seeking QALY loss (3.024 × 10−3)
        hseek_QALY[a] <- rgamma(1, 1.7927, scale = 0.00213254) # healthcare seeking QALY loss (3.823 × 10−3)
        tot_Q_c <- tot_Q_c + (symp[a] - gp[a] - hosp[a]) * nhseek_QALY[a]
        tot_Q_c <- tot_Q_c + (gp[a] + hosp[a]) * hseek_QALY[a]
        tot_Q_d <- tot_Q_d + death[a] * death_QALY[a];
    }
    for (a in 17:25) {
        nhseek_QALY[a] <- rgamma(1, 1.36973, scale = 0.0011265)  # non-healthcare seeking QALY loss (1.543 × 10−3)
        hseek_QALY[a] <- rlnorm(1, -6.23993, 0.933905) # healthcare seeking QALY loss (1.950 × 10−3)
        tot_Q_c <- tot_Q_c + (symp[a] - gp[a] - hosp[a]) * nhseek_QALY[a]
        tot_Q_c <- tot_Q_c + (gp[a] + hosp[a]) * hseek_QALY[a]
        tot_Q_d <- tot_Q_d + death[a] * death_QALY[a];
    }

    list(qaly_cases = tot_Q_c,
        qaly_death = tot_Q_d,
        qaly_total = tot_Q_c + tot_Q_d
    )
}


#' Function to convert incidence of outcomes into the cost of treatment
#' 
#' @param gp A vector of number of GP cases per age group
#' @param bd A vector of number of bed days per age group
#' @param seed An integer value
#' @return The total cost of treatment
get_costT <- function(gp, bd, seed) {
    set.seed(seed)

    tot_CT <- 0
    GP_c <- 37.4
    C1 <- rnorm(1, 725.293, 4.12643)
    C2 <- rnorm(1, 425.242, 5.27808)

    for (a in 1:25)
        tot_CT <- tot_CT + gp[a] * GP_c;
    
    for (a in 1:16)
        tot_CT <- tot_CT + bd[a] * C1
    
    for (a in 17:25)
        tot_CT <- tot_CT + bd[a] * C2
    tot_CT
}

#' Function to convert dose schedule into the cost of administration
#' 
#' @param doses_w A matrix of doses
#' @return The total cost of administration
get_costA <- function(doses_w) {
    costA <- (doses_w * c(57.5, 11.5, 9, 9)) %>% sum
    costA
}


get_Costs <- function(gp, bd, doses_w, seed) {
    costT <- get_costT(gp, bd, seed)
    costA <- get_costA(doses_w)

    cost_direct <- costT
    cost_administration <- costA
    cost_total <- cost_direct + cost_administration

    list(direct = cost_direct,
        intervention = cost_administration,
        total = cost_total
    )
}

#' Function to convert outputs form RunInterventions model into outcomes
#' 
#' @param outputs Output from the RunInterventions model
#' @param posterior Posterior distributions from calibration
#' @param seed integer value
#' @param r discount rate (default is 0.035)
#' @return list of two dataframes, once detailing the health outcomes, one the economic outcomes
get_outcomes <- function(outputs, posterior, cost_imp, discount_rate, seed, disease_mult) {
    inci <- outputs$inci
    doses <- outputs$doses

    undiscount_qaly <- undiscount_cost <- list(rep(0, 25), rep(0, 25), rep(0, 25), rep(0, 25))
    discount_qaly <- discount_cost <- list(rep(0, 25), rep(0, 25), rep(0, 25), rep(0, 25))

    QALY <- 0
    costP <- 0
    costA <- 0
    costT <- 0
    
    death_tot <- hosp_tot <- bd_tot <- gp_tot <- symp_tot <- cases_tot <- 0
    outcomes_age_week <- data.frame()

    for (t_w in 1:nrow(inci)) {
        inci_tw <- inci[t_w, ]
        cases <- get_Inc(inci_tw, t_w, seed)
        symp <- get_S(inci_tw, posterior, t_w, seed)
        gp <- get_GP(inci_tw, t_w, seed)
        bd <- get_bd(inci_tw, t_w, seed)
        hosp <- get_hosp(inci_tw, t_w, seed)
        death <- get_death(inci_tw, t_w, seed)
        if ((t_w >= 52*3) & (t_w < (52*3 + 52))) {
            cases_tot <- cases_tot + sum(cases$incidence)
            symp_tot <- symp_tot + sum(symp$incidence)
            gp_tot <- gp_tot + sum(gp$incidence)
            bd_tot <- bd_tot + sum(bd$incidence)
            hosp_tot <- hosp_tot + sum(hosp$incidence)
            death_tot <- death_tot + sum(death$incidence)
            outcomes_age <- bind_rows(cases, symp, gp, bd, hosp, death)
            outcomes_age_week <- bind_rows(outcomes_age_week, outcomes_age)
        }

        undiscount_qaly_tw <- get_QALY(symp$incidence, gp$incidence, hosp$incidence, bd$incidence, death$incidence, seed)
        discount_qaly_tw <- undiscount_qaly_tw %>% map(~.x * exp(-(t_w - 1) * discount_rate / 52.0))
        undiscount_qaly <- ((1:3 %>% map(~undiscount_qaly[[.x]] + undiscount_qaly_tw[[.x]])) %>% setNames(c("qaly_cases", "qaly_death", "qaly_total")))
        discount_qaly <- ((1:3 %>% map(~discount_qaly[[.x]] + discount_qaly_tw[[.x]])) %>% setNames(c("qaly_cases", "qaly_death", "qaly_total")))
        
        undiscount_cost_tw <- get_Costs(gp$incidence, bd$incidence, doses[t_w, ], seed)
        discount_cost_tw <- undiscount_cost_tw %>% map(~.x * exp(-(t_w - 1) * discount_rate / 52.0))
        undiscount_cost <- ((1:3 %>% map(~undiscount_cost[[.x]] + undiscount_cost_tw[[.x]])) %>% setNames(c("cost_direct", "cost_inter", "cost_total")))
        discount_cost <- ((1:3 %>% map(~discount_cost[[.x]] + discount_cost_tw[[.x]])) %>% setNames(c("cost_direct", "cost_inter", "cost_total")))

    }
    QALY <- data.frame(
        seed = seed,
        type = c(rep("undiscounted", 75), rep("discounted", 75)),
        metric = c(rep("cases", 25), rep("deaths", 25), rep("total", 25), rep("cases", 25), rep("deaths", 25), rep("total", 25)),
        value = c(undiscount_qaly$qaly_cases, undiscount_qaly$qaly_death, undiscount_qaly$qaly_total,
            discount_qaly$qaly_cases, discount_qaly$qaly_death, discount_qaly$qaly_total)
    )
    
    cost <- data.frame(
        seed = seed,
        type = c(rep("undiscounted", 75), rep("discounted", 75)),
        metric = c(rep("direct", 25), rep("inter", 25), rep("total", 25), rep("direct", 25), rep("inter", 25), rep("total", 25)),
        value = c(undiscount_cost$cost_direct, undiscount_cost$cost_inter, undiscount_cost$cost_total,
            discount_cost$cost_direct,  discount_cost$cost_inter, discount_cost$cost_total)
    )
    list(
        QALY = QALY,
        cost = cost,
        outcomes_age_week = outcomes_age_week
    )
}