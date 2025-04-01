set.seed(5004)
sites <- 15
n <- 10
baby_mussels <- data.frame(site = sort(rep(letters[1:sites], n)),
                             mussel_length = rnorm(sites*n, 20, 7) +
                               sort(rep(rnorm(sites, 4, 2), n)))
write.csv(baby_mussels, "mussels.csv", row.names=FALSE)


baby_mussels_ragged <- baby_mussels[-sample(1:nrow(baby_mussels)/2, 50, replace=FALSE),]


qplot(site, mussel_length, data=baby_mussels)
qplot(site, mussel_length, data=baby_mussels_ragged)

write.csv(baby_mussels_ragged, "mussels_ragged.csv", row.names=FALSE)
