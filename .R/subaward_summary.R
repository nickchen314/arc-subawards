#' Summarizes joined df from subaward_df function
#'
#' @param joined_df joined_df from subaward_df
#' @param start_date yymmdd.  All contracts before are excluded
#' @param contract_threshold numeric.  All contracts with obligated amounts below excluded
#' @param country_code character. Filters for primary_place_of_performance_country_code
#' @param award_office_code character. Filters for awarding_office_code
#' @param by_year logical. If true, divides final summary table by year
#' @param df_name character. Name which you want df saved to
#'
#' @return summary data for subaward and award obligations by recieving parent
#' @export
#'
#' @examples
#' subaward_summary(JoinedData, start_date = "2010-08-01", by_year = F, df_name = "test")
subaward_summary <- function(joined_df, 
                             start_date = "", 
                             contract_threshold = "", 
                             country_code = "", 
                             award_office_code = "", 
                             by_year = F,
                             df_name){
  
  ##apply filters
  stopifnot("input must be df" = is.data.frame(joined_df),
            "df_name must be string" = is.character(df_name),
            "by year must be logical" = is.logical(by_year))
  
  if (start_date != ""){
    stopifnot("start_date must be yymmdd" = !is.na(lubridate::ymd(start_date)))
    joined_df %>%
      filter(period_of_performance_start_date > lubridate::ymd(start_date)) -> joined_df
  }
  
  if (contract_threshold != "") {
    stopifnot("contract threshold must be numeric" = is.double(contract_threshold))
    joined_df %>%
      filter(total_obligated_amount >= contract_threshold) -> joined_df
  }
  
  if (country_code != "") {
    stopifnot("invalid country code" = country_code %in% unique(joined_df$primary_place_of_performance_country_code))
    joined_df %>%
      filter(primary_place_of_performance_country_code == country_code) -> joined_df
  }
  
  if (award_office_code != ""){
    stopifnot("invalid office code" = award_office_code %in% unique(joined_df$awarding_office_code))
    joined_df %>%
      filter(awarding_office_code == award_office_code ) -> joined_df
  }
  
  
  
  ##make summary table
  
  if (by_year == F) {
    joined_df %>%
      group_by(recipient_parent_name) %>%
      summarize(total_ob = sum(total_obligated_amount), 
                num_contracts = length(award_id_piid), 
                num_sa = sum(!is.na(n_subawards))) %>%
      arrange(desc(total_ob)) %>%
      mutate(prop_sa = num_sa/num_contracts) %>%
      mutate(prop_sa = scales::percent(prop_sa)) %>%
      mutate(recipient_parent_name = case_when(recipient_parent_name == "" ~ "NO PARENT", 
                                               TRUE ~ recipient_parent_name))-> summary_df
    
    joined_df %>%
      filter(!is.na(n_subawards)) %>%
      group_by(recipient_parent_name) %>%
      summarize(subawardedprojects_ob = sum(total_obligated_amount)) %>%
      mutate(recipient_parent_name = case_when(recipient_parent_name == "" ~ "NO PARENT", 
                                               TRUE ~ recipient_parent_name))-> obligations_projectswsa
    
    final_df <- left_join(summary_df, obligations_projectswsa, 
                          by = join_by(recipient_parent_name == recipient_parent_name)) %>%
      mutate(subawardedprojects_ob = case_when(is.na(subawardedprojects_ob) ~ 0, 
                                               TRUE ~ subawardedprojects_ob)) %>%
      mutate(prop_obs_sa = subawardedprojects_ob / total_ob) %>%
      mutate(prop_obs_sa = scales::percent(prop_obs_sa)) 
  }
  
  if (by_year == T){
    joined_df %>%
      group_by(recipient_parent_name, award_base_action_date_fiscal_year) %>%
      summarize(total_ob = sum(total_obligated_amount), 
                num_contracts = length(award_id_piid), 
                num_sa = sum(!is.na(n_subawards))) %>%
      arrange(desc(total_ob)) %>%
      mutate(prop_sa = num_sa/num_contracts) %>%
      mutate(prop_sa = scales::percent(prop_sa)) %>%
      mutate(recipient_parent_name = case_when(recipient_parent_name == "" ~ "NO PARENT", 
                                               TRUE ~ recipient_parent_name))-> summary_df
    joined_df %>%
      filter(!is.na(n_subawards)) %>%
      group_by(recipient_parent_name, award_base_action_date_fiscal_year) %>%
      summarize(subawardedprojects_ob = sum(total_obligated_amount)) %>%
      mutate(recipient_parent_name = case_when(recipient_parent_name == "" ~ "NO PARENT", 
                                               TRUE ~ recipient_parent_name))-> obligations_projectswsa
    final_df <- left_join(summary_df, obligations_projectswsa, 
                          by = join_by(recipient_parent_name == recipient_parent_name,
                                       award_base_action_date_fiscal_year == award_base_action_date_fiscal_year)) %>%
      mutate(subawardedprojects_ob = case_when(is.na(subawardedprojects_ob) ~ 0, 
                                               TRUE ~ subawardedprojects_ob)) %>%
      mutate(prop_obs_sa = subawardedprojects_ob / total_ob) %>%
      group_by(recipient_parent_name) %>%
      mutate(aggregate_obs = sum(total_ob)) %>%
      arrange(desc(aggregate_obs)) %>%
      mutate(prop_obs_sa = scales::percent(prop_obs_sa)) 
  }
  assign(df_name, final_df, envir = .GlobalEnv)
}



