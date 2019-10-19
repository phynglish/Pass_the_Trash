prob_lose <- function(N=10,n=13,m=6) {
  pl <- 1
  for (k in 1:N) {
      pl <- pl*(4*(n - m + 1) - k)/(4*n - k)  
  }
  return(pl)
}

lose_per_player <- function(N=52,n=13) {
  pl <- 0
  for (m in 1:13) {
    pl <- pl + (1/13)*prob_lose(N,n,m)
  }
  return(pl)
}

N <- 51
players <- 1:N
lose_prob <- numeric(length(players))
for (i in 1:length(players)) {
  lose_prob[i] = lose_per_player(players[i],13)
}

cards <- rep(1:13,4)

lose_prob2 <- numeric(length(players))
for (i in 1:length(players)) {
  lose_prob2[i] <- mean(replicate(1e5,which.min(sample(cards,players[i] + 1,FALSE))==1))
}

library(ggplot2)

df1 <- data.frame(players,lose_prob,lose_prob2)
ggplot(df1, aes(players)) +
  geom_hline(yintercept=1/13,linetype = "dashed",size=1.35,color="grey45") +
  geom_line(aes(y=lose_prob), colour="dodgerblue4",size=2) +
  geom_point(aes(y=lose_prob2),colour="violet",size=2) +
  labs(x = "Number of other players") +
  labs(y = "Probability of losing") +
  xlim(1,51) + ylim(0.01, 0.59)

##3d scatter plot of num_players vs. card drawn vs. lose probability
my_cards <- 1:13
my_players <- 1:15
library(colorRamps)
library(scales)
lose_probs = numeric(length(my_cards)*length(my_players))
for (j in my_cards){
  for (i in my_players){
    lose_probs[(j - 1)*length(my_players) + i] = prob_lose(i,n=13,j)
  }
}

df2 <- expand.grid(x=my_players,y=my_cards)
df2$lose_probs <- lose_probs
ggplot(df2, aes(x,y))+
  geom_tile(aes(fill=lose_probs))+
  scale_fill_gradient(low = 'dodgerblue4', high = 'violet')