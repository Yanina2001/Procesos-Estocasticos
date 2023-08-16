### Problema 3

cards <- function(n){
  ncards <- c()
  for(i in 1:n){
    x <- 5
    counter <- 0
    while(x>4){
      x <- floor(runif(1, 0, 53))
      counter <- counter + 1
    }
    ncards <- c(ncards, counter)
  }
  print(mean(ncards))
  print(var(ncards))
}
cards(1000)



### Problema 4

### 2.26

mat_attacked_ports <- matrix(c(0, 0, 0, 0, 1,
                               0, 8/13, 3/13, 1/13, 1/13,
                               1/16, 3/16, 3/8, 1/4, 1/8,
                               0, 1/11, 4/11, 5/11, 1/11,
                               0, 1/8, 1/2, 1/8, 1/4), ncol = 5, byrow = TRUE)

init = c(0, 0, 0, 0, 1)

### (a)

library(ggplot2)
n <- 2
labels <- c('Port 80', 'Port 135', 'Port 139', 'Port 445', 'No attack')
trials <- 1000
res <- replicate(trials, markov(mat = mat_attacked_ports,init = init,
                                n = n, labels = labels))
states_2_weeks <- res[3,]
counts <- as.data.frame(table(states_2_weeks))
ggplot(counts, aes(x=states_2_weeks, y=Freq, fill=states_2_weeks)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = Freq), vjust = 1.5, color = "white") +
  labs(cat("Attacked ports after 2 weeks (", trials, " trials )"))


### (b) Find the long-term distribution of attacked ports.


error <- 10 ** -8
i <- 1
A_n_minus_1 <- mat_attacked_ports
number_entries <- dim(mat_attacked_ports)[1] * dim(mat_attacked_ports)[2]
flag <- TRUE
while (flag) {
  A_n <- A_n_minus_1 %*% mat_attacked_ports
  aux_mat <- A_n - A_n_minus_1
  aux_count <- length(aux_mat[abs(aux_mat) < error])
  if (aux_count == number_entries) {
    flag <- FALSE
  }
  else {
    i <- i + 1
    A_n_minus_1 <- A_n
  }
}
cat("In", i, "iterations, the matrix transition converges to the following matrix:\n")
print(A_n)


