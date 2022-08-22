
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
