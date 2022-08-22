
get_new_data_frame <- function(df_outputs) {
  df_outputs$outcomes_week_age %>%
  group_by(outcome, seed) %>%
  summarise(tot_inci = sum(incidence))
}

get_change <- function(base, inter, string) {

  left_join(
    get_new_data_frame(base) %>% rename(tot_inci_base = tot_inci),
    get_new_data_frame(inter) %>% rename(tot_inci_output = tot_inci)
  )  %>% mutate(inci_averted = (tot_inci_base - tot_inci_output) / tot_inci_base, type = string) %>%
    filter(!is.na(inci_averted)) %>% as.data.frame
}

get_new_data_frame_age <- function(df_outputs) {
  df_outputs$outcomes_week_age %>%
  group_by(outcome, seed, age_group) %>%
  summarise(tot_inci = sum(incidence))
}


get_change_age <- function(base, inter, string) {

  left_join(
    get_new_data_frame_age(base) %>% rename(tot_inci_base = tot_inci),
    get_new_data_frame_age(inter) %>% rename(tot_inci_output = tot_inci)
  )  %>% mutate(inci_averted = (tot_inci_base - tot_inci_output) / tot_inci_base, type = string) %>%
    filter(!is.na(inci_averted)) %>% as.data.frame
}


get_new_data_frame_age <- function(df_outputs) {
  df_outputs$outcomes_week_age %>%
    mutate(age_group_cat = case_when(
      (age_group >= 1) & (age_group <= 6)~"<6 mo",
      (age_group >= 7) & (age_group <= 12)~"6-11 mo",
      (age_group >= 13) & (age_group <= 16)~"1-4 yrs",
      (age_group >= 17) & (age_group <= 23)~"5-64 yrs",
      (age_group >= 24) & (age_group <= 25)~"65+ yrs"
      )) %>%
    group_by(outcome, seed, age_group_cat) %>%
    summarise(tot_inci = sum(incidence))
}

get_prob_direct_df <- function(base, inter, base_d, inter_d, string) {
    left_join(
      left_join(
        left_join(
          get_new_data_frame_age(base) %>% rename(tot_inci_base = tot_inci),
          get_new_data_frame_age(base_d) %>% rename(tot_inci_d_base = tot_inci)
        ),
        get_new_data_frame_age(inter) %>% rename(tot_inci_inter = tot_inci)
      ),
      get_new_data_frame_age(inter_d) %>% rename(tot_inci_d_inter = tot_inci)
    ) %>%  mutate(cases_averted_direct =  tot_inci_base - tot_inci_d_inter, cases_averted_indirect = tot_inci_d_inter - tot_inci_inter) %>%
    mutate(cases_averted_direct_prop = cases_averted_direct / tot_inci_base, cases_averted_indirect_prop = cases_averted_indirect / tot_inci_base) %>% 
  #  mutate(cases_averted_direct_prop = cases_averted_direct / sum(tot_inci_base), cases_averted_indirect_prop = cases_averted_indirect / sum(tot_inci_base)) %>% 
    mutate(name = string)
}