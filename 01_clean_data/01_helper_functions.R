library(tidyverse)
library(NlsyLinks)

clean_df <- function(df, old_cols, new_cols, dropped_cols) {
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
    mutate(
      estimated_age = estimate_age(
        month_of_birth, 
        year_of_birth, 
        month_of_1981_interview
      )
    )
  
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
  
  # While the {sibling_id} is technically equal to the {family_id} (and thus, I could join by {sibling_id} = {family_id}), I don't do this because I want to distinguish between two non-full-sibling respondents who live in the same household vs two full-sibling respondents who live in the same household. 
  
  # In the former case, the {sibling_id} would be NA; in the latter case, it would not. Thus, I only join by {case_id} 
  cleaned_df <- merge(cleaned_df, family_linkages_df, by = c("case_id"), all.x = TRUE)
  
  cleaned_df <- estimate_num_full_sibling_respondents(cleaned_df)
  
  cleaned_df <- identify_dz_twins(cleaned_df)
  
  cleaned_df <- identify_analytic_sample(cleaned_df)
  
  return(cleaned_df)
}

estimate_age <- function(month_of_birth, year_of_birth, month_of_1981_interview) {
  birth_month <- year_of_birth * 12 + month_of_birth
  
  # because the {year_of_birth} variable is stored as a two-digit integer, we use '81' when calculating the interview month instead of '1981'
  interview_month <- 81 * 12 + month_of_1981_interview 
  
  return((interview_month - birth_month) / 12)
}

estimate_num_full_sibling_respondents <- function(df) {
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
    
  # for each set of DZ twins, select the one with the lowest {birth_order}; if the DZ twins have the same birth order, select the one with the lowest {case_id}
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
      has_third_born_child = max(birth_order == 2)
    ) %>%
    mutate(
      is_two_sibling_sample_family = (
        has_first_born_child & 
        has_second_born_child
      ), 
      is_three_sibling_sample_family = (
        has_first_born_child & 
        has_second_born_child & 
        has_third_born_child
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
      # 3. Is either a non-DZ twin or a selected DZ twin
      # ... then they are included in the analytic sample
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
  
  return(analytic_sample_df)
}



