require(dplyr)
require(tidyr)

require(ggplot2)

labels_month <- c("Jul", "Sep", "Nov", "Jan", "Mar", "May")
ggplot() +  
  scale_x_continuous(breaks = seq(1, 12, 2), labels = labels_month, limits = c(0, 12)) + 
  scale_y_continuous(limits = c(-0.4, 0.7)) +
  geom_function(fun = "dnorm", args = list(mean = 6, sd = 1), size = 3) + 
  theme_bw() + theme(axis.text.y = element_blank(), text = element_text(size = 15)) +
  labs(y = "Transmission Rate")


# Load pre-made datasets
load(file = here::here("data", "inter_model_input", "rsv_data_uk.RData")) # loads ukdata
load(file = here::here("data", "inter_model_input", "inter_data_uk.RData")) # loads data_inter_uk

age_groups_vec <- c("Time", rep("<6mo", 6), rep("6–11mo", 6), rep("1yr", 1), rep("2-4yrs", 3), rep("5–64yrs", 7), rep("65+yrs", 2))
df_incidence <- ukdata$observationalData %>% t %>% as.data.frame %>%
  mutate(details = age_groups_vec) %>% group_by(details) %>% summarise(across(c("V1":"V364"), ~mean(.x))) %>% 
  pivot_longer("V1":"V364", values_to = "weekly_incidence", names_to = "week") %>%
  mutate(week = as.numeric(substr(week, 2, 4))) %>% filter(details != "Time") %>% 
  mutate(details = factor(details, levels = c("<6mo", "6–11mo", "1yr", "2-4yrs", "5–64yrs", "65+yrs")))



df_incidence %>% 
  ggplot() + 
    geom_point(aes(week, weekly_incidence), size = 0.1, shape = 2) + facet_wrap(vars(details)) + 
    theme_bw() + 
    labs(x = "Year", y = "Weekly reported samples in RDMS") +
    scale_x_continuous(breaks = c(seq(1, 400, 52)), labels = 2010:2017) + theme(axis.text.x = element_text(angle = 90))

plot_plane <- function(icer_table_plot, ppd) {
  icer_table_plot <- icer_table_plot_mean
  icer_table_cal <- icer_table_plot %>% mutate(total_cost = cost + doses * ppd)
  icer_table_cal[1, 5] <- icer_table_cal[1, 5] + 58320000

  icer_table_cal_dom <- recal_icer_table(icer_table_cal)
  icer_table_cal %>% 
    ggplot() +
      geom_line(data = icer_table_cal_dom, aes(x = QALY, y = total_cost), color = "black", size = 1) + 
      geom_point(aes(x = QALY, y = total_cost, fill = strategy), shape = 21, color = "black", size = 3) + 
      labs(x = "QALY gain", y = "Total discounted cost over 10 years", title = paste0("ICER plane with price per dose of £", ppd)) +
      theme_bw()
}


create_dominance_df <- function(icer_table_plot) {

  df_dominance_ppd <- list()
  i <- 1
  for (ppd in 1:100) {
    icer_table_cal <- icer_table_plot %>% mutate(total_cost = cost + doses * ppd)
    icer_table_cal_dom1 <- icer_table_cal %>% arrange(total_cost) %>%
      mutate(dom = QALY < lag(QALY), rank = 1:6, ppd = ppd) 

    icer_table_cal_dom2 <- icer_table_cal_dom1 %>% mutate(QALY_change = QALY - lag(QALY),
      cost_change = total_cost - lag(total_cost), icer = cost_change / QALY_change) %>%
      mutate(ext_dom = (icer > lead(icer)) & (lead(icer) > 0) ) 

    icer_table_cal_dom <- left_join(icer_table_cal_dom1, icer_table_cal_dom2, 
      by = c("strategy", "QALY", "cost", "doses", "total_cost", "dom", "rank", "ppd")) %>%
      mutate(dom_type = case_when(dom~"Dominated", ext_dom~"Extended dominated", TRUE~"None"))

    df_dominance_ppd[[i]] <- icer_table_cal_dom %>% dplyr::select(strategy, QALY, cost, doses, total_cost, rank, ppd, dom_type)     
    i <- i + 1
  }
  df_dominance_ppd
}
