
# Génération d'une subdivision de l'intervalle [0, 5] de pas 0.01
x <- seq(0,5,.01); x 
# Densité de la loi log-normale de moyenne -1/2 et de variance 1
y <- dlnorm(x, -1/2, 1); y 

# Densité de la loi gamma de paramètres 2, 2
y2 <- dgamma(x, 2, 2); y2

y3 <- dweibull(x, 2, 2/sqrt(pi)); y3

leg.txt <- c("LN(-1/2,1)","G(2,2)","W(2,2/sqrt(pi))")
# Graphe de la densité de la loi normale
plot(x, y, xlab="x", ylab="f(x)", main="Comparaison des densit\'es", ylim=range(y, y2, y3), col="black", type="l")

lines(x,y2, lty=2)
lines(x,y3, lty=3)
legend("topright",leg=leg.txt, col="black",lty=1:3)

# Loi uniforme standard sur [0, 1]
x1<- runif(100); x1
y1=-log(x1)
# Loi uniforme sur [2,3]
x2 <-rnorm(100, 2,3); x2
hist(y1)

plot(ecdf(y1), do.points=FALSE)
curve(pexp, 0, max(y1), add=TRUE, col="grey50")


# Création d'une fonction de masse de probabilité
dpoism <- function(x, p0, l)
    ifelse(x == 0, p0, (1-p0)/(1-dpois(0, l))*dpois(x, l))

e<-seq(0,20,1)
E<-dpoism(e, 0.5, 2); E

x <- 0:10
y <- dpoism(x, dpois(0, 1), 1)
y2 <- dpoism(x, 1/2, 1)
y3 <- dpoism(x, dpois(0, 2), 2)
y4 <- dpoism(x, 1/3, 2)
leg.txt <- c("P(1)","P-0M(1)", "P(2)","P-0M(2)")
plot(x, y, xlab="x", ylab="f(x)", ylim=range(y, y2, y3, y4[-(1:15)]), col="black", type="b")
lines(x, y2, col="blue", type="b")
lines(x, y3, col="red", type="b")
lines(x, y4, col="green", type="b")
legend("topright",leg=leg.txt, col=c("black","blue","red","green"),lty=1)

### Comparaison des différentes méthodes

??actuar
library("actuar")
install.packages("actuar")

# COMPARAISON DE GRAPHES

fx.u <- discretize(pgamma(x, 2, 1), from = 0, to = 22, step = 0.5, method = "upper")
Fs.u <- aggregateDist("recursive", model.freq = "poisson", model.sev = fx.u, lambda = 10, x.scale = 0.5)
fx.l <- discretize(pgamma(x, 2, 1), from = 0, to = 22, step = 0.5, method = "lower")
Fs.l <- aggregateDist("recursive", model.freq = "poisson", model.sev = fx.l, lambda = 10, x.scale = 0.5)
Fs.n <- aggregateDist("normal", moments = c(20, 60))
Fs.np <- aggregateDist("npower", moments = c(20, 60, 4/sqrt(60)))
Fs.s <- aggregateDist("simulation", model.freq = expression(y = rpois(10)), model.sev = expression(y = rgamma(2, 1)), nb.simul = 10000)


x<-seq(20,50, 0.05)
leg.txt=c("FS.u", "Fs.np")
par(mfrow=c(1,2))
plot(x, Fs.u(x), type="l")
lines(x,Fs.np(x), lty=2)
legend("topright", leg=leg.txt, col="red", lty=1:2)

#plot(x,Fs.np(x), type="l")