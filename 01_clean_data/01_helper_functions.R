library(tidyverse)
library(NlsyLinks)

clean_df <- function(df, old_cols, new_cols, dropped_cols) {
  cleaned_df <- df %>%
    rename(!!!set_names(old_cols, new_cols)) %>%
    select(-all_of(dropped_cols)) %>%
    mutate(
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
    return(cleaned_df)
}

estimate_age <- function(month_of_birth, year_of_birth, month_of_1981_interview) {
  birth_month <- year_of_birth * 12 + month_of_birth
  
  # because the {year_of_birth} variable is stored as a two-digit integer, we use '81' when calculating the interview month instead of '1981'
  interview_month <- 81 * 12 + month_of_1981_interview 
  
  return((interview_month - birth_month) / 12)
}



