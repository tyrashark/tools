aaa <- matrix(data = NA, nrow = 4, ncol = 13)


rownames(aaa) <- c("Diamond", "Heart", "Spade", "Clover")
colnames(aaa) <- paste("N", 1:13)

aaa["Diamond", "N1"] = 1

carddeck <- paste(c("Diamond", "Heart", "Spade", "Clover"), rep(1:13, each = 4))

carddeck <- gsub("13", "King", carddeck)
carddeck <- gsub("12", "Queen", carddeck)
carddeck <- gsub("11", "Junior", carddeck)
carddeck <- gsub("1$", "Ace", carddeck)


cards <- sample(carddeck, 5, replace = F)


length(grep("Spade", cards))
for (i in 1:1000){
  cards <- sample(carddeck, 5, replace = F)
  temp = unlist(strsplit(cards, ' '))
  temp[temp %in% c("Diamond", "Heart", "Spade", "Clover")]
  if (length(unique(temp[temp %in% c("Diamond", "Heart", "Spade", "Clover")]))==1){
   cat("Flush! \n")
    print(cards)
  }
}

sample(carddeck, 5, replace = F)







