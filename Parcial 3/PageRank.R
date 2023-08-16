pagerank2 <- function(H, n, pi0) {
  # Initialize transition matrix S
  S <- (H + 1e-10) / rowSums(H + 1e-10)
  
  # Initialize teleport matrix
  alpha <- 0.15
  G <- matrix(0,n,n)
  diag(G) <- (1-alpha) /n
  G <- G + (alpha) * pi0
  
  # Calculate PageRank
  pi <- pi0
  for (i in 1:n) {
    pi <- G %*% pi
  }
  
  # Return result
  return(list(S = S, G = G, pi = pi))
}