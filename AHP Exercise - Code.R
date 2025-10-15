
# ============================== Code for AHP Exercise ==============================

# Auxiliary Function - calculating the principal eigenvector
principal_eigenvector <- function(A) {
  eig <- eigen(A)
  i_max <- which.max(Re(eig$values))
  w <- Re(eig$vectors[, i_max])
  w <- abs(w)
  w <- w / sum(w)
  list(lambda_max = Re(eig$values[i_max]), w = w)
}

conduct_AHP <- function(crit_mat, alter_mat_list) {
  
  # Check input validity
  if (nrow(crit_mat) != ncol(crit_mat))
    stop("Criteria Matrix must be square.")
  
  n_criteria <- nrow(crit_mat)
  n_alternatives <- nrow(alter_mat_list[[1]])
  
  if (length(alter_mat_list) != n_criteria)
    stop("Number of alternative matrices must equal number of criteria.")
  
  for (i in seq_len(n_criteria)) {
    if (nrow(alter_mat_list[[i]]) != ncol(alter_mat_list[[i]]))
      stop(paste("Alternative matrix", i, "is not square."))
    if (nrow(alter_mat_list[[i]]) != n_alternatives)
      stop("All alternative matrices must have the same order.")
  }
  
  # Eigenvector for each criterion
  alternative_weights <- list()
  for (i in seq_len(n_criteria)) {
    alternative_weights[[i]] <- principal_eigenvector(alter_mat_list[[i]])$w
  }
  alternative_weights_mat <- do.call(cbind, alternative_weights)
  colnames(alternative_weights_mat) <- paste0("Criterion_", 1:n_criteria)
  rownames(alternative_weights_mat) <- paste0("Alternative_", 1:n_alternatives)
  
  # Criteria weights
  crit_weights <- principal_eigenvector(crit_mat)$w
  names(crit_weights) <- paste0("Criterion_", 1:n_criteria)
  
  # Weighted sum: total score
  total_scores <- alternative_weights_mat %*% crit_weights
  total_scores <- as.vector(total_scores)

  result <- list(
    criteria_weights = crit_weights,
    alternative_scores_by_criterion = alternative_weights_mat,
    total_scores = data.frame(
      Alternative = paste0("Alternative_", 1:n_alternatives),
      Score = round(total_scores, 4),
      Rank = rank(-total_scores, ties.method = "first")
    )
  )
  
  return(result)
}

# Testing
crit_mat <- matrix(c(1, 1/5, 3,
                     5, 1, 2,
                     1/3, 1/2, 1), nrow = 3, byrow = TRUE)

alter_mat_list <- list(
  # Criterion 1: 
  matrix(c(1, 1/9, 1/7,
           9, 1, 1/3,
           7, 3, 1), nrow = 3, byrow = TRUE),
  
  # Criterion 2: 
  matrix(c(1, 1/5, 1/3,
           5, 1, 1/2,
           3, 2, 1), nrow = 3, byrow = TRUE),
  
  # Criterion 3: 
  matrix(c(1, 9, 1,
           1/9, 1, 1/7,
           1, 7, 1), nrow = 3, byrow = TRUE)
)

result <- conduct_AHP(crit_mat, alter_mat_list)

cat("\nCriteria Weights \n")
print(round(result$criteria_weights, 4))

cat("\nAlternative Weights per Criterion \n")
print(round(result$alternative_scores_by_criterion, 4))

cat("\nFinal Total Scores and Ranking \n")
print(result$total_scores)










