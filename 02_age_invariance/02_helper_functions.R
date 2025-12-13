library(tidyverse)
library(lavaan)
library(semPlot)
library(sirt)
library(ggthemes)

estimate_factor_scores <- function(df, parameters, moderator.grid, first_order_factors, items) {
  
  scores_df <- data.frame(
    case_id = as.integer(), 
    age_group = as.integer(), 
    g_factor_score = as.numeric()
  )
  
  for(mod in moderator.grid) {
    age_parameters <- parameters %>%
      filter(moderator == mod)
    
    age_df <- df %>%
      filter(
        round(estimated_age) == mod | 
          estimated_age < 16 & mod == 16 | 
          estimated_age > 23 & mod == 23
      )
    
    age_scores_df <- estimate_factor_scores_by_age(age_df, age_parameters, first_order_factors, items) %>%
      mutate(
        age_group = mod
      )
    
    scores_df <- rbind(
      age_scores_df, 
      scores_df
    )
  }
  
  return(scores_df)
}


estimate_factor_scores_by_age <- function(df, parameters, first_order_factors, items) {
  # assume p items and b broad ability factors
  
  # p x b matrix describing the loading of items onto broad ability factors
  first_order_loadings_matrix <- create_first_order_loading_matrix(parameters, first_order_factors, items)
  # b x 1 vector describing the loading of broad ability factors on the general factor
  second_order_loadings_matrix <- create_second_order_loading_matrix(parameters, first_order_factors)
  # p x 1 matrix describing the  loading of items directly onto the general factor
  direct_loadings_matrix <- first_order_loadings_matrix %*% second_order_loadings_matrix
  
  # p x n matrix of scores on all items, removing the intercepts for those items
  residualized_items <- remove_factor_intercepts(df, parameters, direct_loadings_matrix, first_order_loadings_matrix, first_order_factors, items)
  
  # p x p matrix describing the residual variances of the items
  item_residual_variance_matrix <- create_residual_variance_matrix(parameters, items)
  # b x b matrix describing the residual variances of the broad ability factors
  first_order_factor_residual_variance_matrix <- create_residual_variance_matrix(parameters, first_order_factors)
  # p x p matrix describing the total residual variance (variance not explained by the general factor)
  total_residual_variance_matrix <- item_residual_variance_matrix + first_order_loadings_matrix %*% first_order_factor_residual_variance_matrix %*% t(first_order_loadings_matrix) 
  
  # 1 x n matrix describing the g-factor scores of each respondent
  g_factor_scores_matrix <- solve(t(direct_loadings_matrix) %*% solve(total_residual_variance_matrix) %*% direct_loadings_matrix) %*%
    t(direct_loadings_matrix) %*% solve(total_residual_variance_matrix) %*% residualized_items
    
  g_factor_scores_df <- data.frame(g_factor_scores_matrix) %>%
    pivot_longer(
      cols = everything(), 
      names_to = "case_id", 
      values_to = "g_factor_score"
    ) %>%
    mutate(
      case_id = as.integer(substring(case_id, 2))
    )
  return(g_factor_scores_df)
}

create_first_order_loading_matrix <- function(parameters, first_order_factors, items) {
  first_order_loadings_df <- parameters %>%
    # filter to first-order factor loadings
    filter(
      lhs %in% first_order_factors, 
      op == "=~"
    ) %>%
    select(
      lhs,
      rhs, 
      est
    ) %>%
    # convert to matrix form
    pivot_wider(
      id_cols = "lhs",
      names_from = "rhs", 
      values_from = "est", 
      values_fill = 0
    ) %>%
    # order the matrix rows and columns so that this matrix will be compatible with the matrices produced by other functions 
    arrange(
      lhs
    ) %>%
    select(
      all_of(items)
    )
    
    first_order_loadings_matrix <- t(as.matrix(first_order_loadings_df))
    colnames(first_order_loadings_matrix) <- sort(first_order_factors)
    return(first_order_loadings_matrix)
}

create_second_order_loading_matrix <- function(parameters, first_order_factors) {
  second_order_loadings_df <- parameters %>%
    # filter to second-order factor loadings
    filter(
      lhs == "g", 
      rhs %in% first_order_factors, 
      op == "=~"
    ) %>%
    arrange(
      rhs
    ) %>%
    select(
      est
    )
  
  second_order_loadings_matrix <- as.matrix(second_order_loadings_df)
  colnames(second_order_loadings_matrix) <- c("g")
  rownames(second_order_loadings_matrix) <- sort(first_order_factors)
  return(second_order_loadings_matrix)
}

remove_factor_intercepts <- function(df, parameters, direct_loadings_matrix, first_order_loadings_matrix, first_order_factors, items) {
  
  # retrieve second-order factor mean
  second_order_factor_mean <- parameters %>%
    filter(
      lhs == "g", 
      op == "~1"
    ) %>%
    select(
      est
    )

  # multiply the second-order factor mean by the item loadings to yield intercepts at the item-level
  second_order_factor_mean_vector <- as.matrix(second_order_factor_mean)
  item_intercepts_from_second_order_factor_df <- data.frame(direct_loadings_matrix %*% second_order_factor_mean_vector) %>%
    rownames_to_column(var = "item")

  # retrieve first-order factor means  
  first_order_factor_means <- parameters %>%
    filter(
      lhs %in% first_order_factors, 
      op == "~1"
    ) %>%
    # sort so that subsequent matrix operations are compatible
    arrange(
      lhs
    ) %>%
    select(
      est
    )
  
  # multiply the first-order factor means by the item loadings to yield intercepts at the item-level
  first_order_factor_means_vector <- as.matrix(first_order_factor_means)
  item_intercepts_from_first_order_factors_df <- data.frame(first_order_loadings_matrix %*% first_order_factor_means_vector) %>%
    rownames_to_column(var = "item")
  
  # retrieve intercepts of the items themselves
  item_intercepts_from_items <- parameters %>%
    filter(
      lhs %in% items, 
      op == "~1"
    ) %>%
    select(
      lhs, 
      est
    ) %>%
    rename(
      item = "lhs"
    )
  
  # calculate the total intercept for each item (sum up item intercepts derived from second-order factor means, first-order factor means, and the items themselves)  
  intercepts <- item_intercepts_from_second_order_factor_df %>%
    merge(
      item_intercepts_from_first_order_factors_df, 
      by = "item", 
      suffixes = c("_second_order_factor_means", "_first_order_factor_means")
    ) %>%
    merge(
      item_intercepts_from_items, 
      by = "item"
    ) %>%
    mutate(
      intercept = est_second_order_factor_means + est_first_order_factor_means + est
    ) %>%
    select(
      item,
      intercept
    )
  
  # create sorted list of case IDs so that the resulting matrix is sorted by case ID as well
  respondents <- as.character(sort(df$case_id))
  
  # subtract the total intercept from each of the item scores
  residualized_df <- df %>%
    select(
      any_of(c(items, "case_id"))
    ) %>%
    pivot_longer(
      cols = !case_id, 
      names_to = "item", 
      values_to = "score"
    ) %>%
    merge(
      intercepts, 
      by = "item"
    ) %>%
    mutate(
      item = factor(item, levels = items),
      residualized_score = score - intercept
    ) %>%
    pivot_wider(
      id_cols = "item",
      names_from = "case_id",
      values_from = "residualized_score"
    ) %>%
    # sort the items so that they are compatible with other matrices
    arrange(
      item
    ) %>%
    select(
      all_of(respondents)
    )
  
  residualized_matrix <- as.matrix(residualized_df)
  rownames(residualized_matrix) <- items
  
  return(residualized_matrix)
}

create_residual_variance_matrix <- function(parameters, desired_parameters) {
  residual_variances_df <- parameters %>%
    filter(
      lhs %in% desired_parameters, 
      rhs %in% desired_parameters,  
      op == "~~"
    ) %>%
    select(
      lhs, 
      rhs, 
      est
    ) %>%
    pivot_wider(
      id_cols = "lhs", 
      names_from = "rhs", 
      values_from = "est", 
      values_fill = 0
    ) %>%
    # sort rows and columns so that they are compatible with other matrices
    mutate(
      lhs = factor(lhs, levels = desired_parameters)
    ) %>%
    arrange(
      lhs
    ) %>%
    select(
      all_of(desired_parameters)
    )
  
  residual_variances_matrix <- as.matrix(residual_variances_df)
  rownames(residual_variances_matrix) <- desired_parameters
  
  return(residual_variances_matrix)
}
