

View(base5)

# CREATION DE VARIABLE

N1=base5$SINAP1
N2=base5$SINAP2
N3=base5$SINAP3
N4=base5$SINAP4
N5=base5$SINAP5
N6=base5$SINAP6

N=N1+N2+N3+N4+N5+N6

###Charges de sinistres

charge=base5$CHARGE

#### Ancienneté de permis

Permis=base5$PERMIS


##### Sexe du conducteur

S=base5$SEX

###Satut
st=base5$STATUT

#Catégorie socio-professionnelle
CSP=base5$CSP

### Usage
us=base5$USAGE

### Age du conducteur
age=base5$AGECOND


# ETUDE DESCRIPTIF

effN <-table(N)
freqN <-prop.table(effN)
barplot(freqN)
pie(freqN)
plot(freqN, type="l")


charge1<-charge[charge>0]
effC <-table(charge1)
freqC<-prop.table(effC)
barplot(freqC)


# Test d'adéquation

X=rpois(length(N), mean(N)) #Génératon d'une loi de poisson

## test de Kormogorov-Smernov

ks.test(N,X)

# Test de conformité
qqplot(N,X,main="Test Adeq")

## Loi de Chi-2
chisq.test(N,X)

# Les valeurs de la variable charge

Charge=base5$CHARGE
C1=Charge[Charge>0] # extraction des charges nulles

hist(C1, breaks=100)
m=density(C1)
plot(m)
hist(C1,col="yellow",proba=T)
lines(m)

## Test de conformité

xt=0:20000
moy=mean(C1)
s=sd(C1)
yt=dnorm(xt,moy,s)
points(xt,yt,col="lightslateblue",type="l",lwd==2)

qt=qnorm((1:length(C1))/length(C1),moy,s)
qqplot(C1,qt)
abline(a=0,b=1, col="lightslateblue")

hist(C1,col="yellow",proba=T)
xt=10000:20000
moy=mean(log(C1))
s=sd(log(C1))
yt=dlorm(xt,moy,s)
points(xt,yt,col="lightslateblue",type="l",lwd==2)

qt=qlnorm((1:length(C1))/length(C1),moy,s)
qqplot(C1,qt)
abline(a=0,b=1, col="lightslateblue")

m=length(C1)
E=rexp(m,1/m)
qtexp=qexp((1:m)/m,1/m)
qqplot(C1,qtexp)
abline(a=0,b=1, col="lightslateblue")

ks.test(C1,E)

### Conclusion: La exponentielle convient la mieux à la modélisation des Charges

zone=base5$ZONE
eff1<-table(zone)
freq1<-prop.table(eff1)
barplot(freq1)
pie(freq1)


sexe=base5$SEX
eff2<-table(sexe)
freq2<-prop.table(eff2)
barplot(freq2)
pie(freq2)


CSP=base5$CSP
eff3<-table(CSP)
freq3<-prop.table(eff3)
barplot(freq3)
pie(freq3)



age=base5$AGECOND
eff4<-table(age)
freq4<-prop.table(eff4)
barplot(freq4)
pie(freq4)

per=base5$PERMIS
eff5<-table(per)
freq5<-prop.table(eff5)
barplot(freq5)
pie(freq5)

St=base5$STATUT

### De deux variable fortement corrélées, l'on ne conservera que une seule dans l'étude


cor(Charge,N)
cor(zone,N)
cor(age,N)
cor(CSP,N)
cor(per,N)

### Création d'une base de données
 
base=data.frame(N,age,CSP,St,sexe,zone)
base 

### Courbes variables fréquences sinistres

n=max(age)-min(age)
fage=seq(0,n)
for(i in 1:n)
{
  x=19+i
  y=N[age==x]
  fage[i]=sum(y)
}
w=seq(0,n)+20
fage1=fage/sum(N)
plot(n, fage1,col="lightslateblue",type="l",lwd=2)

## GLM

reg1<-glm(N ~ age, data=base, family=poisson)
summary(reg1)

reg2<-glm(N ~ age+CSP, data=base, family=poisson)
summary(reg2)

reg3<-glm(N ~ age+CSP+St, data=base, family=poisson)
summary(reg3)


