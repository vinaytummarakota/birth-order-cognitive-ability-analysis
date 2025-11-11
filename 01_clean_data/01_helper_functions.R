library(tidyverse)
library(NlsyLinks)
library(docstring)

clean_df <- function(df, old_cols, new_cols, dropped_cols) {
  #' @description: Clean the input NLSY data 
  #' 
  #' @param df: The input NLSY data
  #' @param old_cols: The column names in the original NLSY data obtained via the NLS Investigator
  #' @param new_cols: The new column names that should be used in place of the old column names
  #' @param dropped_cols: The columns that should be dropped because they won't be used in this analysis, but are automatically added by the NLS Investigator
  cleaned_df <- df %>%
    rename(
      !!!set_names(old_cols, new_cols)
    ) %>%
    select(
      -all_of(dropped_cols)
    ) %>%
    mutate(
      birth_order = ifelse(
        between(birth_order, -5, -1), 
        NA, 
        birth_order
      ),
      month_of_birth = ifelse(
        between(month_of_birth, 1, 12), 
        month_of_birth, 
        NA
      ), 
      year_of_birth = ifelse(
        between(year_of_birth, 55, 65), 
        year_of_birth, 
        NA
      ), 
      month_of_1981_interview = ifelse(
        between(month_of_1981_interview, 1, 12), 
        month_of_1981_interview, 
        NA
      ), 
      sampling_weight = ifelse(
        between(sampling_weight, 1, 9999999), 
        sampling_weight, 
        NA
      ), 
      asvab_section_1_general_science = ifelse(
        between(asvab_section_1_general_science, -5, -1), 
        NA, 
        asvab_section_1_general_science
      ), 
      asvab_section_2_arithmetic_reasoning = ifelse(
        between(asvab_section_2_arithmetic_reasoning, -5, -1), 
        NA, 
        asvab_section_2_arithmetic_reasoning
      ),
      asvab_section_3_word_knowledge = ifelse(
        between(asvab_section_3_word_knowledge, -5, -1), 
        NA, 
        asvab_section_3_word_knowledge
      ),
      asvab_section_4_paragraph_comprehension = ifelse(
        between(asvab_section_4_paragraph_comprehension, -5, -1), 
        NA, 
        asvab_section_4_paragraph_comprehension
      ),
      asvab_section_5_numerical_operations = ifelse(
        between(asvab_section_5_numerical_operations, -5, -1), 
        NA, 
        asvab_section_5_numerical_operations
      ),
      asvab_section_6_coding_speed = ifelse(
        between(asvab_section_6_coding_speed, -5, -1), 
        NA, 
        asvab_section_6_coding_speed
      ),
      asvab_section_7_auto_and_shop_info = ifelse(
        between(asvab_section_7_auto_and_shop_info, -5, -1), 
        NA, 
        asvab_section_7_auto_and_shop_info
      ),
      asvab_section_8_mathematics_knowledge = ifelse(
        between(asvab_section_8_mathematics_knowledge, -5, -1), 
        NA, 
        asvab_section_8_mathematics_knowledge
      ),
      asvab_section_9_mechanical_comp = ifelse(
        between(asvab_section_9_mechanical_comp, -5, -1), 
        NA, 
        asvab_section_9_mechanical_comp
      ),
      asvab_section_10_electronics_info = ifelse(
        between(asvab_section_10_electronics_info, -5, -1), 
        NA, 
        asvab_section_10_electronics_info
      ), 
      age_at_interview = ifelse(
        between(age_at_interview, 16, 24), 
        age_at_interview, 
        NA
      )
    ) %>%
    # because the NLSY79 did not report the exact age at which respondents completed the ASVAB, I approximated age using the respondents' birth month and the month they completed the 1981 interview (the interview which took place the year after the ASVAB was administered)
    mutate(
      estimated_age = estimate_age(
        month_of_birth, 
        year_of_birth, 
        month_of_1981_interview
      )
    ) %>%
    # I remove observations which do not have an ASVAB sampling weight (i.e. they did not complete the ASVAB)
    filter(
      !is.na(sampling_weight)
    )
  
  # I use the NlsyLinks package to identify full biological siblings
  family_linkages_df <- Links79Pair %>%
    filter(
      R == 0.5, 
      RelationshipPath == 'Gen1Housemates'
    ) %>%
    pivot_longer(
      cols = c(SubjectTag_S1, SubjectTag_S2), 
      names_to = c("SubjectTag"), 
      values_to = c("case_id")
    ) %>%
    mutate(
      # the NlsyLinks package appends two zeroes to the {case_id} variable, we remove those additional zeroes here
      case_id = case_id / 100
    ) %>%
    select(
      -c(SubjectTag, R, RelationshipPath)
    ) %>%
    rename(
      sibling_id = ExtendedID
    ) %>%
    distinct() 
  
  # While the {sibling_id} is technically equal to the {family_id} (and thus, I could join by {sibling_id} = {family_id}), I don't do this because I want to distinguish between two non-full-sibling respondents who live in the same household vs two full-sibling respondents who live in the same household. In the former case, the {sibling_id} would be NA; in the latter case, it would not.
  cleaned_df <- merge(cleaned_df, family_linkages_df, by = c("case_id"), all.x = TRUE)
  
  cleaned_df <- estimate_num_full_sibling_respondents(cleaned_df)
  
  cleaned_df <- identify_dz_twins(cleaned_df)
  
  cleaned_df <- identify_analytic_sample(cleaned_df)
  
  return(cleaned_df)
}

estimate_age <- function(month_of_birth, year_of_birth, month_of_1981_interview) {
  #' @description: Estimate the age of the respondent at the time of their 1981 interview
  #' 
  #' @param month_of_birth: An integer indicating the month of the year in which the respondent was born
  #' @param year_of_birth: A two-digit integer indicating the year in which the respondent was born
  #' @param month_of_1981_interview: An integer indicating the month in which the respondent completed the 1981 interview
  birth_month <- year_of_birth * 12 + month_of_birth
  
  # because the {year_of_birth} variable is stored as a two-digit integer, we use '81' when calculating the interview month instead of '1981'
  interview_month <- 81 * 12 + month_of_1981_interview 
  
  return((interview_month - birth_month) / 12)
}

estimate_num_full_sibling_respondents <- function(df) {
  #' @description: Estimate the number of full-sibling NLSY respondents in each family 
  #' 
  #' @param df: The input NLSY data
  grouped_df <- df %>%
    group_by(sibling_id) %>%
    summarise(
      num_full_sibling_respondents = n()
    )
  
  sibling_size_df <- merge(df, grouped_df, by = "sibling_id", all.x = TRUE) %>%
    mutate(
      num_sibling_respondents = coalesce(
        num_full_sibling_respondents, 
        1
      )
    )
  
  return(sibling_size_df)
}

identify_dz_twins <- function(df) {
  #' @description: Identify likely dizygotic twins in the NLSY data and randomly select one to be included in any birth order analyses
  #' 
  #' @param df: The input NLSY data
  age_df <- df %>%
    filter(
      !is.na(sibling_id)
    ) %>%
    select(
      case_id, 
      sibling_id, 
      estimated_age, 
      birth_order
    )
  
  # use dplyr::inner_join instead of merge due to faster performance
  age_df_joined <- age_df %>%
    inner_join(
      age_df, 
      by = "sibling_id", 
      suffix = c("", "_sibling2")
    ) %>%
    filter(
      case_id != case_id_sibling2
    )
  
  grouped_df <- age_df_joined %>%
    # when the two siblings' ages differ by <= 1 month, they are likely dizygotic twins
    mutate(
      is_likely_dz_twin = abs(estimated_age - estimated_age_sibling2) <= 1/12
    ) %>%
    # in families with 3+ siblings, the same {case_id} will be duplicated by the self-join, so we use group_by to de-duplicate
    group_by(
      case_id, 
      sibling_id, 
      birth_order
    ) %>% 
    summarise(
      is_likely_dz_twin = as.logical(max(is_likely_dz_twin))
    )
    
  # for each set of DZ twins, select the one with the lowest {birth_order} to be included in the birth order analysis; if the DZ twins have the same birth order, select the one with the lowest {case_id}
  selected_twin_df <- grouped_df %>%
    filter(
      is_likely_dz_twin == TRUE
    ) %>%
    group_by(
      sibling_id
    ) %>%
    arrange(
      birth_order, 
      case_id, 
      .by_group = TRUE
    ) %>%
    slice_head(
      n = 1
    ) %>%
    select(
      case_id
    ) %>%
    mutate(
      is_selected_twin = TRUE
    )
  
  grouped_df_with_selected_twins <- merge(grouped_df, selected_twin_df, by = "case_id", all.x = TRUE) %>%
    mutate(
      is_selected_twin = coalesce(is_selected_twin, FALSE)
    ) %>% 
    select(
      case_id, 
      is_likely_dz_twin, 
      is_selected_twin
    )
  
  dz_identified_df <- merge(df, grouped_df_with_selected_twins, by = "case_id", all.x = TRUE)
  
  return(dz_identified_df)
}

identify_analytic_sample <- function(df) {
  #' @description: Identify the respondents who will be included in the two-sample and three-sample birth order analyses, respectively. 
  #' 
  #' @param df: The input NLSY data
  grouped_df <- df %>%
    filter(
      num_full_sibling_respondents >= 2, 
      # remove the non-selected DZ twins to ensure they aren't used to identify the set of families that would be included in the analytic sample
      ifelse(
        is_likely_dz_twin, 
        is_selected_twin, 
        TRUE
      )
    ) %>%
    group_by(
      family_id
    ) %>%
    summarise(
      has_first_born_child = max(birth_order == 0), 
      has_second_born_child = max(birth_order == 1), 
      has_third_born_child = max(birth_order == 2), 
      # unfortunately, there are a couple families where multiple non-DZ twin siblings report having the same birth order (which should be impossible), so I need to use these columns to remove them
      has_anomalous_birth_order_two_sibling_sample = (sum(birth_order %in% c(0, 1)) != 2), 
      has_anomalous_birth_order_three_sibling_sample = (sum(birth_order %in% c(0, 1, 2)) != 3)
    ) %>%
    # identify the set of families which have the first-born, second-born, and third-born children in the NLSY data
    mutate(
      is_two_sibling_sample_family = (
        has_first_born_child & 
        has_second_born_child & 
        !has_anomalous_birth_order_two_sibling_sample
      ), 
      is_three_sibling_sample_family = (
        has_first_born_child & 
        has_second_born_child & 
        has_third_born_child & 
        !has_anomalous_birth_order_three_sibling_sample
      )
    ) %>%
    select(
      family_id, 
      is_two_sibling_sample_family, 
      is_three_sibling_sample_family
    )
  
  analytic_sample_df <- merge(df, grouped_df, by = "family_id", all.x = TRUE) %>%
    mutate(
      # If the respondent ... 
      # 1. Belongs to an eligible family
      # 2. Has an eligible birth order
      # 3. Is a selected DZ twin or a non-DZ twin
      # ... then they should be included in the analytic sample
      is_in_two_sibling_sample = 
        is_two_sibling_sample_family &  
        birth_order %in% c(0, 1) & 
        ifelse(
          is_likely_dz_twin, 
          is_selected_twin, 
          TRUE
        ), 
      is_in_three_sibling_sample = 
        is_three_sibling_sample_family &  
        birth_order %in% c(0, 1, 2) & 
        ifelse(
          is_likely_dz_twin, 
          is_selected_twin, 
          TRUE
        )
      )
  
  # in rare cases, siblings' birth orders don't align with their actual ages; the next steps ensure that those siblings are not included in the analytic sample
  two_sibling_sample_eligible_families <- analytic_sample_df %>%
    filter(
      is_in_two_sibling_sample == TRUE
    ) %>%
    select(
      family_id, 
      estimated_age, 
      birth_order
    ) %>%
    pivot_wider(
      names_from = birth_order, 
      values_from = estimated_age, 
      names_prefix = 'estimated_age_'
    ) %>%
    # require that the first-born child has a higher age than the second-born
    filter(
      estimated_age_0 > estimated_age_1 
    ) %>%
    pull(
      family_id
    )
  
  three_sibling_sample_eligible_families <- analytic_sample_df %>%
    filter(
      is_in_three_sibling_sample == TRUE
    ) %>%
    select(
      family_id, 
      estimated_age, 
      birth_order
    ) %>%
    pivot_wider(
      names_from = birth_order, 
      values_from = estimated_age, 
      names_prefix = 'estimated_age_'
    ) %>%
    # require that the first-born child has a higher age than the second-born and third-born, second-born child has a higher age than the third-born
    filter(
      estimated_age_0 > estimated_age_1, 
      estimated_age_0 > estimated_age_2, 
      estimated_age_1 > estimated_age_2
    ) %>%
    pull(
      family_id
    )
  
  analytic_sample_df <- analytic_sample_df %>%
    mutate(
      is_in_two_sibling_sample = is_in_two_sibling_sample & family_id %in% two_sibling_sample_eligible_families, 
      is_in_three_sibling_sample = is_in_three_sibling_sample & family_id %in% three_sibling_sample_eligible_families
    )
  
  return(analytic_sample_df)
}



