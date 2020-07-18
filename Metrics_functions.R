#' traj_class <- class_trajectory(traj, traj_pval)
class_trajectory <- function(traj, pval) {
  out <- ifelse(traj <= 0.9 & pval < 0.025, 1,
                ifelse(traj >=  1.1 & pval < 0.025, 3, 2))
  
  out <- factor(out,
                levels = 1:3,
                labels = c("Shrinking", "No significant change", "Growing"),
                ordered = TRUE)
  
  return(out)
}

traj<-.85
traj_pval<-.02
class_trajectory(traj, traj_pval)

### Burden ########

score_burden <- function(curr, prev, pop) {
  # requires: pair of non-negative integers
  curr <- check_nonneg(curr, "curr")
  prev <- check_nonneg(prev, "prev")
  pop <- check_nonneg(pop, "pop")
  
  #Currently not using pseudocount/offset for burden
  alpha <- 0
  
  burden <- (1e5 / pop) * (curr + prev + alpha)
  
  burden
}