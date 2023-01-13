rm(list=ls())

# Mjerene su kolicine srebra u Bizantskom kovanom novcu klasificiranog u 4 grupe
# 4 grupe = 4 razdoblja

# Ucitala sam podatke iz datoteke "ulazni_podaci" preko ikone import data

podaci <- ulazni_podaci
grupe <- list(podaci$V1[!is.na(podaci$V1)], podaci$V2[!is.na(podaci$V2)], podaci$V3[!is.na(podaci$V3)], podaci$V4[!is.na(podaci$V4)])
grupe_s_null <- list(podaci$V1, podaci$V2, podaci$V3, podaci$V4)
boje <- c("yellow", "orange", "green", "blue")
nazivi <- list("Grupa 1", "Grupa 2", "Grupa 3", "Grupa 4")

# a)

# Dijagrami pravokutnika za svaku grupu i usporedba
# -------------------------------------------------

# par(mfrow = c(2, 2)) # ako zelimo da su svi na jednoj slici
# par(mfrow = c(1, 1)) # za vratiti natrag na jedan graf po slici
for(i in 1:4){
  boxplot(grupe[[i]], col=boje[i], main=nazivi[[i]], horizontal=T)
  print(median(grupe[[i]]))
}



# ZAKLJUCAK: Dijagrami se međusobno razlikuju. Medijani grupa su 6.8, 8.6, 4.75 i 5.6
# pa vidimo da se medijani razlikuju. Na grafovima je medijan oznacen kao crna
# zadebljana crta unutar dijagrama pravokutnika. Razlika se vidi i u polozaju
# medijana unutar dijagrama pravokutnika i duljini dijagrama pravokutnika
# sto ovisi o tome kako su podaci grupirani oko medijana.


# b)
# Ispitujemo dolaze li podatci iz normalno distribuiranih populacija
# To radimo upotrebom dva kriterija (grafova norm. vjer. i Lillieforsove inacice)

# Upotreba grafova normalnih vjerojatnosti
# ----------------------------------------

# y1,...,yn sortirani uzorak. za i=1,...,n definiramo kvantile jed. norm. razd.
# qi=(fi)^(-1) * (i-3/8) / (n+1/4)
# graf s točkama (qi,yi) nazivamo normalni vjerojatnosni graf.
# ako uzorak dolazi i norm. razd s parametrima mi i sigma^2, tada tocke (qi, yi)
# aproksimativno budu na pravcu y = mi + sigma * q

# par(mfrow = c(2, 2)) # ako zelimo da su svi na jednoj slici
# par(mfrow = c(1, 1)) # za vratiti natrag na jedan graf po slici
for (j in 1:4){
  y <- sort(grupe[[j]])
  n <- length(grupe[[j]])
  i <- 1:n
  x <- qnorm((i-3/8)/(n+1/4))
  plot(x,y, ylim=c(floor(min(grupe[[j]])),ceiling(max(grupe[[j]]))), xlim=c(-2,2), col=boje[[j]])
  abline(mean(y),sd(y))
}

# ZAKLJUCAK: Ne mozemo odbaciti pretpostavku da su podaci normalno distriburiani.
# Podaci grupa 1 i 4 prate pravac koji bi trebao aproksimirati podatke u slucaju
# da su normalno distribuirani. Kod grupa 2 i 3 su podaci malo udaljeniji od
# pravca, ali su i dalje poredani oko njega, da ima malo vise podataka
# mozda bi se bolje uocile nekakve pravilnosti

# Lillieforsova inacica Kolmogorov-Smirovljevog testa
# ---------------------------------------------------

# Ho: Podaci pripadaju normalnoj distribuciji
# H1: Podaci ne pripadaju normalnoj distribuciji

mi <- list()
sigma <- list()
z <- matrix(0,4,9) # spremimo standardizirane podatke u varijablu z
# redak matrice z pripada pojedinoj grupi

for (i in 1:4){
  zz <- c()
  mi[[i]] <- mean(grupe[[i]])
  sigma[[i]] <- sd(grupe[[i]])
  duljina <- length(grupe[[i]])
  for (k in 1:duljina){
    zz[k] <- (grupe[[i]][k] - mi[[i]]) / sigma[[i]]
  }
  z[i,1:duljina] <- sort(zz)
}

Dn <- list(0,0,0,0)

for (i in 1:4){
  D1 <- c()
  D2 <- c()
  duljina <- length(grupe[[i]])
  for (k in 1:duljina){
    D1 <- abs(k/duljina - pnorm(q=z[i,k]))
    D2 <- abs((k-1)/n - pnorm(q=z[i,k]))
    Dn[[i]] <- max(Dn[[i]], max(D1, D2))
  }
}
Dn # 0.2009168, 0.1987394, 0.4889953, 0.1613733

p_vrijednosti <- list()
for (i in 1:4){
  p_vrijednosti[[i]] <- ks.test(x = z[[i]], y = "pnorm")$p.value
}
p_vrijednosti # 0.1201804, 0.1352278, 0.4046568, 0.156016

# ZAKLJUCAK: p vrijednosti za sve grupe su vece od svih standardnih razina
# znacajnosti 1%, 5% i 10% tako da u odnosu na svaku od njih ne možemo odbaciti
# Ho, to jest ne možemo odbaciti pretpostavku da su podaci normalno distribuirani


# c)
# Xi procenitelji od mii, a sigma^2 nepristrani procjenitelj zajednickog
# parametra varijance na osnovi sva cetiri uzorka zajedno.
# pokazite da (Xi-mii)*sqrt(ni)/sigma ~ t(n-4) ; n=n1+n2+n3+n4
# Nakon toga procjeniti 95% pouzdane intervale za svaku grupu za 
# populacijske srednje vrijednosti mii i graficki usporedite

Xi = list()
for (i in 1:4){
  Xi[[i]]=mean(grupe[[i]])
}
Xi # 6.74, 8.243, 4.875, 5.614

svi_podaci_skupa=c(grupe[[1]], grupe[[2]], grupe[[3]], grupe[[4]])
nn=length(svi_podaci_skupa)
sigma_kapa=var(svi_podaci_skupa)*nn/(nn-1) # 1.947633

# t.test(grupe[[2]], df=nn-4)

donja_granica = list()
gornja_granica = list()
for (i in 1:4){
  donja_granica[[i]]=Xi[[i]]-qt(p=0.975, df=nn-4)*sigma_kapa/sqrt(length(grupe[[i]]))
  gornja_granica[[i]]=Xi[[i]]+qt(p=0.975, df=nn-4)*sigma_kapa/sqrt(length(grupe[[i]]))
}
donja_granica 
gornja_granica 

plot(c(donja_granica[[1]], gornja_granica[[1]]), c(0,0), type="l", col=boje[[1]], xlim=c(2.5,10), ylim=c(-0.1,0.5),
     xlab="", ylab="")
segments(x0=donja_granica[[2]], y0=0.1, x1 = gornja_granica[[2]], y1 = 0.1, col = boje[[2]])
segments(x0=donja_granica[[3]], y0=0.2, x1 = gornja_granica[[3]], y1 = 0.2, col = boje[[3]])
segments(x0=donja_granica[[4]], y0=0.3, x1 = gornja_granica[[4]], y1 = 0.3, col = boje[[4]])


# d)
# Jednostranom analizom varijanci uspoređujem dane skupove podataka te
# odgovaram na pitanje da li svi podaci dolaze iz iste populacije.
# Diskutiram jesu li sve pretpostavke za taj test ispunjene?


# PRETPOSTAVKE ZA KORIŠTENJE JEDNOSTRANE ANALIZE VARIJANCI:
      # 1. Za svaku razinu faktora, podaci moraju biti normalno distribuirani
# i to imamo, pokazano u podzadatku b)


      # 2. Uzorci između dvije različite razine faktora moraju biti nezavisni:

# Buduci da svaka razina faktora ima uzorke istog obiljezja (kolicina srebra),
# samo pripadaju razlicitom razdoblju razumno je za pretpostaviti da su uzorci
# nezavisni

      # 3. Varijanca uzorka mora biti jednaka za svaki uzorak:

uzorak <- c(grupe[[1]], grupe[[2]], grupe[[3]], grupe[[4]])
ni <- c(length(grupe[[1]]), length(grupe[[2]]),length(grupe[[3]]), length(grupe[[4]]))
faktor_uzorka <- factor(rep(c("A","B","C","D"), ni))
matrica<-data.frame(faktor_uzorka,uzorak)

library(car)
leveneTest(uzorak~faktor_uzorka)
# p-vrijednost je 0.1052 sto je vece od svih standardnih razina znacajnosti 
# iz tog razloga ne mozemo odbaciti hipotezu Ho, da su varijance promatrana
# cetiri uzorka iste


# Sve su pretpostavke zadovoljene pa mozemo primjeniti ANOVA-u:

# Ho: nema razlike među populacijama
# H1: ima razlike među populacijama

m <- 4

vars <- tapply(uzorak, faktor_uzorka, var)
means <- tapply(uzorak, faktor_uzorka, mean)
xpotez <-  mean(uzorak)

sst <-  as.vector(crossprod(ni, (means - xpotez)^2))
sse <- as.vector(crossprod(ni - 1, vars))
mst <- sst/(m-1)
mse <- sse/(nn-m)

f <- mst/mse 
pvalue <- 1 - pf(f, df1 = m-1, df2 = nn-m)  
pvalue

# Kriticno podrucje je [ff,+oo> = [9.276628, +oo>
alfa <- 0.5
ff <- qf(alfa,m-1,n-m,lower.tail = FALSE) # 9.276628

# tockovni procjenitelj od sigme je MSE, a on iznosi 0.478932
# p vrijednost je 1.305986e-07, manja od svih standardnih razina znacajnosti 
# pa odbacujemo hipotezu Ho, da nema razlike među populacijama

