#' Joins primary and subaward data from USASPENDING
#' 
#' This function joins primary and subaward data together from USASpending
#' contracts dataframes.  Subaward data is grouped and summed
#'
#' @param prim_df df primary contracts from usaspending
#' @param sub_df df subawards from usaspending
#' @param df_name character name for df
#'
#' @return primary df with sum of subaward count and total money. Assigned to 
#' df_name
#' @export
#'
#' @examples
#' prim_df <- read.csv("./data/Contracts_PrimeAwardSummaries_2023-10-01_H04M40S14_1.csv")
#' subaward_df <- read.csv("./data/Contracts_Subawards_2023-10-01_H04M45S53_1.csv")
#' subaward_join(prim_df, subaward_df, "test")
#' 
subaward_df <- function(prim_df, sub_df, df_name) {
  stopifnot("input must be df" = is.data.frame(prim_df) & is.data.frame(sub_df),
            "df must haven 1 row" = nrow(prim_df) > 0,
            "df must haven 1 row" = nrow(sub_df) > 0,
            "df_name must be string" = is.character(df_name))
  
  prim_df %>%
    select(contract_award_unique_key, award_id_piid, total_obligated_amount, 
           total_outlayed_amount, period_of_performance_start_date, 
           period_of_performance_current_end_date, recipient_name, 
           recipient_uei, recipient_parent_name, 
           recipient_parent_uei, awarding_office_code, awarding_office_name, 
           primary_place_of_performance_country_code, award_base_action_date_fiscal_year) -> filtered_prim
  sub_df %>%
    select(prime_award_unique_key, prime_award_piid, subaward_amount, subawardee_name, subawardee_parent_name) %>%
    group_by(prime_award_unique_key, prime_award_piid) %>%
    summarize(total_subaward = sum(subaward_amount), n_subawards = length(prime_award_unique_key)) -> grouped_sub
  
  joined_df <- left_join(filtered_prim, 
                         grouped_sub, 
                         by = join_by(contract_award_unique_key == prime_award_unique_key)) %>%
    mutate(period_of_performance_start_date = lubridate::ymd(period_of_performance_start_date)) %>%
    mutate(period_of_performance_current_end_date = lubridate::ymd(period_of_performance_current_end_date))
  
  joined_df <- joined_df %>%
    select(!(prime_award_piid))
  assign(df_name, joined_df, envir = .GlobalEnv)
  
}


