setwd("C:/Users/asus/Desktop/TP_Actuariat")

## Triangle des paiements cumulés, C = (Ci,j )

L1<-c(3209, 4372, 4411, 4428, 4435, 4456)
L2<-c(3367, 4659, 4696, 4720, 4730, NA)
L3<-c(3871, 5345, 5398, 5420, NA, NA)
L4<-c(4239, 5917, 6020, NA, NA, NA)
L5<-c(4929, 6794, NA, NA, NA, NA)
L6<-c(5217, NA, NA, NA, NA, NA)

PAID<-rbind(L1,L2,L3,L4,L5,L6); PAID

## Le triangle des incréments, I, se déduit facilement du triangle des cumulés

INCURRED<-PAID
nc <- ncol(PAID)
nl <- nrow(PAID)

INCURRED[,2:nc] <- PAID[,2:nc]-PAID[,1:(nc-1)]; I

## Vecteur des primes P

PREMIUM<-c(4591, 4672, 4863, 5175, 5673, 6431); PREMIUM

##  Triangle des nombres de sinistres, cumulés, en milliers

l1<-c( 1043.4, 1045.5, 1047.5, 1047.7, 1047.7, 1047.7)
l2<-c(1043.0, 1027.1, 1028.7, 1028.9, 1028.7, NA)
l3<-c(965.1, 967.9, 967.8, 970.1, NA, NA)
l4<-c(977.0, 984.7, 986.8, NA, NA, NA)
l5<-c( 1099.0, 1118.5,NA, NA, NA, NA)
l6<-c(1076.3, NA, NA, NA, NA, NA)

NUMBER<-rbind(l1,l2,l3,l4,l5,l6); NUMBER

