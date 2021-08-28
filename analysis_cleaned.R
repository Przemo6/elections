# libraries used (w pliku z 1 zajec sa opisy bibliotek do czego sluza jbc)

library(rgdal)
library(dplyr)
library(spdep)
library(lmtest)
library(spatialreg)
library(texreg) # for table comparison between different models

# wczytanie danych

# data from BDL:
unempl<-read.csv("data/bezrobocie.csv", header=TRUE, dec=",", sep=";") # bezrobocie w roku 2019 w procentach
inc<-read.csv("data/dochody na mieszk 2016-2019.csv", header=TRUE, dec=",", sep=";") # dochod gminy na jednego mieszkanca w latach 16-19
fem<-read.csv("data/feminizacja.csv", header=TRUE, dec=",", sep=";") # wspolczynnik feminizacji tj liczba kobiet na 100 mezczyzn
people<-read.csv("data/ludnosc 2016-2019.csv", header=TRUE, dec=",", sep=";") # gestosc zaludnienia w km2 (dane z lat 16-19)
people_prod<-read.csv("data/ludnosc ogol przed poprod.csv", header=TRUE, dec=",", sep=";") # ludnosc ogolem, ludnosc w wieku przed i poprodukcyjnym (dane z lat 16-19)
migration<-read.csv("data/saldo migracji stalych na 1000 osob.csv", header=TRUE, dec=",", sep=";") # saldo migracji stalych miedzy gminami na 1000 osob w latach 16-19
soc_ben<-read.csv("data/swiadczenie wychowawcze 500 sty-czer.csv", header=TRUE, dec=",", sep=";") #przecietna miesieczna liczba rodzin pobierajacych 500+ od stycznia do czerwca 2019 roku (i tak wybory byly gdzies w pazdzierniku)
wat_sew_gas<-read.csv("data/wodkangaz.csv", header=TRUE, dec=",", sep=";") # % ludnosci korzystajacej z wody / kanalizacji / gazu
exp<-read.csv("data/wydatki na mieszk 2016-2019.csv", header=TRUE, dec=",", sep=";") # wydatki gminy na jednego mieszkanca w latach 16-19

#data from elections in 2019 (source: https://sejmsenat2019.pkw.gov.pl/sejmsenat2019/pl/dane_w_arkuszach)
elect<-read.csv("data/wyniki_gl_na_listy_po_obwodach_sejm.csv", header=TRUE, dec=",", sep=";") # dane po obwodach
elect_gm<-read.csv("data/wyniki_gl_na_listy_po_gminach_sejm.csv", header=TRUE, dec=",", sep=";") # dane gminne


# maps (mapa na poziomie gminy potrzebna do macierzy itp, reszta tylko co najwyżej do grafik)

pl<-readOGR("data", "Państwo") # 1 jedn. 
voi<-readOGR("data", "Województwa") # 16 jedn. 
pov<-readOGR("data", "Powiaty") # 380 jedn. 
com<-readOGR("data", "Gminy") # 2477 jedn.


# changing projections
pl<-spTransform(pl, CRS("+proj=longlat +datum=NAD83"))
voi<-spTransform(voi, CRS("+proj=longlat +datum=NAD83"))
pov<-spTransform(pov, CRS("+proj=longlat +datum=NAD83"))
com<-spTransform(com, CRS("+proj=longlat +datum=NAD83"))




# przygotowanie danych z gusu


#expenditures - we take mean of the years since last elections
summary(exp$gminy.Ĺ.Ä.cznie.z.miastami.na.prawach.powiatu.ogĂłĹ.em.2016..zĹ..)
summary(exp$gminy.Ĺ.Ä.cznie.z.miastami.na.prawach.powiatu.ogĂłĹ.em.2017..zĹ..)
summary(exp$gminy.Ĺ.Ä.cznie.z.miastami.na.prawach.powiatu.ogĂłĹ.em.2018..zĹ..)
summary(exp$gminy.Ĺ.Ä.cznie.z.miastami.na.prawach.powiatu.ogĂłĹ.em.2019..zĹ..) # there are some NA's but last year is full so we can omit NA's and won't have any lacks of data
exp$expenses<-rowMeans(exp[,3:6], na.rm = T) #mean of years 2016-2019
summary(exp$expenses)
exp[2:7] <- NULL # we leave only code & created data



#fem
colnames(fem)[3] <- "femin"
fem[c(2,4)] <- NULL


#income - robimy to samo co z wydatkami
summary(inc$gminy.Ĺ.Ä.cznie.z.miastami.na.prawach.powiatu.ogĂłĹ.em.2016..zĹ..)
summary(inc$gminy.Ĺ.Ä.cznie.z.miastami.na.prawach.powiatu.ogĂłĹ.em.2017..zĹ..)
summary(inc$gminy.Ĺ.Ä.cznie.z.miastami.na.prawach.powiatu.ogĂłĹ.em.2018..zĹ..)
summary(inc$gminy.Ĺ.Ä.cznie.z.miastami.na.prawach.powiatu.ogĂłĹ.em.2019..zĹ..)
inc$income<-rowMeans(inc[,3:6], na.rm = T)
summary(inc$income)
inc[2:7] <- NULL


#migration - rowniez to samo co z wydatkami: srednia z 4 lat
summary(migration$saldo.migracji.na.1000.osĂłb.ogĂłĹ.em.2016..osoba.)
summary(migration$saldo.migracji.na.1000.osĂłb.ogĂłĹ.em.2017..osoba.)
summary(migration$saldo.migracji.na.1000.osĂłb.ogĂłĹ.em.2018..osoba.)
summary(migration$saldo.migracji.na.1000.osĂłb.ogĂłĹ.em.2019..osoba.)
migration$migration<-rowMeans(migration[,3:6], na.rm = T)
summary(migration$migration)
migration[2:7] <- NULL


#people_density 
colnames(people)[6] <- "people_density2019"
people[c(2:5,7)] <- NULL


#people_prod - recznie dzielimy liczbe ludzi w grupach przed i poprodukcyjnych przez ogolna liczbe ludzi
colnames(people_prod)[6] <- "ogolem"
people_prod$prework <- people_prod$w.wieku.przedprodukcyjnym.ogĂłĹ.em.2019..osoba. / people_prod$ogolem
people_prod$postwork <- people_prod$w.wieku.poprodukcyjnym.ogĂłĹ.em.2019..osoba. / people_prod$ogolem
summary(people_prod$prework)
summary(people_prod$postwork)
people_prod[c(2:5, 7:15)] <- NULL


#soc_ben - czyli 500+ sty-czer 2019, podzielilem przecietna miesieczna liczbe rodzin przez liczbe ludzi w gminie
soc_ben$benefit500 <- soc_ben$przeciÄ.tna.miesiÄ.czna.liczba.rodzin.otrzymujÄ.cych.Ĺ.wiadczenie.wychowawcze.w.okresie.od.1.stycznia.2019.do.30.czerwca.2019.2019.... / people_prod$ogolem
soc_ben[c(2:4)] <- NULL


#unemployment in 2019
colnames(unempl)[3] <- "unemployment"
unempl[c(2,4)] <- NULL


#wat_sew_gas: 3 separate variables
colnames(wat_sew_gas)[3:5] <- c("water", "sewage", "gas")
wat_sew_gas[c(2,6)] <- NULL
summary(wat_sew_gas$water)
summary(wat_sew_gas$sewage)
summary(wat_sew_gas$gas)



### merging all sets into one data by the code of gmina
data <- Reduce(function(x,y) merge(x = x, y = y, by = "Kod"), 
               list(exp, fem, inc, migration, people, people_prod, soc_ben, unempl, wat_sew_gas))


# The last digit in this code defines the type of gmina and is not needed. In data from elections this digit isn't included so we'll remove it from here also.
data$Kod <- data$Kod %>% as.character() %>% substr(1, nchar(data$Kod)-1) %>% as.numeric()




#przygotowanie danych wyborczych na poziomie gmin


# Let's check if the codes from elections on the community level match the codes from the rest of the data
matched <- intersect(data$Kod, elect_gm$Kod.TERYT)
all <-  union(data$Kod, elect_gm$Kod.TERYT)
non_matched <- all[!all %in% matched]
non_matched 

elect_gm[elect_gm$Kod.TERYT %in% non_matched, 1:4]
# There are some numbers that don't match. Most of them results from differences in Warsaw. It is a city
#with code 146501, but in data from election it is divided by districts (codes 146502-146519). We need to merge this data together
# so we'll have one data for Warsaw.


elect_gm[c(5, 7:25)] <- NULL # Removing redundant columns
colnames(elect_gm)[5:6] <- c("entitled", "votes") #Renaming columns: column with number of people entitled to vote and column with number of valid votes

# zsumowanie warszawy do 1 obs jak opisano wyżej
elect_gm[elect_gm$Kod.TERYT == 146502, 5:16] <- colSums(elect_gm[elect_gm$Kod.TERYT >= 146501 & elect_gm$Kod.TERYT <= 146519, 5:16]) # Sum of district values for numerical columns to the 1st district
elect_gm[elect_gm$Kod.TERYT == 146502, 1:2] <- c(146501, "Warszawa") # Renaming 1st district to the code and name of Warsaw


# We have still 2 unmatched codes left. Those are votes from ships and from the abroad so we don't need them, same as for the rest of already sumed Warsaw districts.
# removing rest of Warsaw districts and abroad/ships
elect_gm <- elect_gm[elect_gm$Kod.TERYT < 146502 | elect_gm$Kod.TERYT > 149901,]



## Adding new variables from election results

# frequency - our target variable - in %

elect_gm$freq <- elect_gm$votes / elect_gm$entitled * 100



# votes for oposition - as a fraction of all votes: a will to change the current government as a possible factor to drive people to the elections.

elect_gm$opos <- rowSums(elect_gm[c(7:11, 13:16)], na.rm = T) / elect_gm$votes

elect_gm[c(7:16)] <- NULL # removing redundant variables


# merging data from bdl and elections on community level
data <- merge(x = data, y = elect_gm, by.x = "Kod", by.y = "Kod.TERYT")



# Dodanie bazy danych z wyborów na poziomie obwodow

# places & districts

summary(elect$Kod.TERYT)
elect[is.na(elect$Kod.TERYT),2:7]
# There are some obs without teritorial codes, they are empty and from the unique places outside the country like 
# military bases that didn't participate in voting -> we can remove them.

elect <- elect[!is.na(elect$Kod.TERYT),]



matched <- intersect(data$Kod, elect$Kod.TERYT)
all <-  union(data$Kod, elect$Kod.TERYT)
non_matched <- all[!all %in% matched] #Still we have the same problem with Warsaw divided by districts and with ships and abroad
non_matched


#recoding Warsaw districts to general Warsaw code
elect[elect$Kod.TERYT >= 146502 & elect$Kod.TERYT <= 146519, 2] <- 146501



# WE will need only generally available locations and we will use them to count for community the average  of people enabled to vote at a one place (obwod) to see if maybe possibility of bigger crowds and queues
# makes people less interested in voting in contrast to regions with more locations per number of enabled people. We will also calculate the
# number of physical places which may be useful later
elect <- elect %>% 
  filter(Typ.obwodu == "staĹ‚y") %>% 
  group_by(Kod.TERYT) %>%
  summarise(places = length(unique(Siedziba)),
            mean_enabled = mean(as.numeric(Liczba.wyborcĂłw.uprawnionych.do.gĹ.osowania))) 


# Adding this data to our main one
data <- merge(x = data, y = elect, by.x = "Kod", by.y = "Kod.TERYT")




#saveRDS(data, 'data/data_elections.rda')


data <- readRDS('data/data_elections.rda')




#Initial statistics - mozesz usunac mapke oczywiscie za swoje lepsze

summary(data$freq)
cols<-rev(heat.colors(8))
brks<-(2:5)*0.2
plot(com, col=cols[findInterval(data$freq, brks)])
plot(pov, add=TRUE, lwd=2)
plot(voi, add=TRUE, lwd=3)
legend("bottomleft", legend=brks, pt.bg=cols, bty="n", pch=22)
title(main="Frequency", sub="In legend intervals from….%")





############## spatial weight matrices - Tutaj probujemy rozne typy macierzy zeby znalezc taka ktora najlepiej uchwyci przestreznnosc
# Wiekszosc komentarzy i w zasadzie caly kod jest prosto z zajec niezmieniony wiec moznaby to jakos przerobic np w tekscie a komentarze usunac czy cos

# Contiguity matrix
cont.nb<-poly2nb(as(com, "SpatialPolygons"))
cont.listw<-nb2listw(cont.nb, style="W")
cont.listw # summary of matrix

# coordinates of nts4 units 
#(geometric center of gravity)
crds<-coordinates(com)
colnames(crds)<-c("cx", "cy")

# plot of neighbourhood - jak nic nie wnosi to usun, nie jest potrzebna ani ta ani inne
plot(com) # contour map
plot(cont.nb, crds, add=TRUE)




# spatial weights matrix – k nearest neighbours
knn<-knearneigh(crds, k=1) # knn class (k=1)
knn.nb<-knn2nb(knn)

plot(com) # tak samo mozna usunac mapke
plot(knn.nb, crds, add=TRUE)

# checking for matrix symmetry
print(is.symmetric.nb(knn.nb))
sym.knn.nb<-make.sym.nb(knn.nb)
print(is.symmetric.nb(sym.knn.nb))

# knn as listw class
knn.listw<-nb2listw(sym.knn.nb)



# matrix within the radius that gives at least 1 neighbour to every unit:
# at what distance all units have at least one neighbour?

# implicitly k=1, list of closests neighbours
kkk<-knn2nb(knearneigh(crds)) 

# max of distances between clostests neighbours  we get distance
all<-max(unlist(nbdists(kkk, crds))) 

# neighbours in radius of d km 
# guaranted that all regions have at least one neighbor
all.nb<-dnearneigh(crds, 0, all) 

summary(all.nb, crds)
plot(com, border="grey")  # jak zwykle mozna usunac
plot(all.nb, crds, add=TRUE)
radius.listw<-nb2listw(all.nb)




# spatial weights matrix - inverse distance matrix

knn_inv<-knearneigh(crds, k=2476) # we have 2477 units on the map
knn_inv.nb<-knn2nb(knn_inv)
dist<-nbdists(knn_inv.nb, crds)  
dist1<-lapply(dist, function(x) 1/x)  # object of listw class
knn_inv.dist.listw<-nb2listw(knn_inv.nb, glist=dist1)  # listw class object – spatial weights according to distance criterion




# spatial weights matrix - inverse squared distance matrix

dist2<-lapply(dist, function(x) 1/x^2)  # object of listw class
knn_inv.dist2.listw<-nb2listw(knn_inv.nb, glist=dist2)  # listw class object – spatial weights according to distance criterion





################## Initial spatial statistics for frequency: nw na ile potrzebne wszystkie
# macierze tutaj, ale moze mozna w jedna tabele np zebrac albo wyrzucic inne niz cont.listw
# W każdym razie nie ma przestrzennej autokorelacji tutaj. Ale to bardziej jest
# gdzies do graficznych przedstawien itp zalezy co Ty tam zrobiles w pracy
# Mozna tez cos opisac o tym jak bardzo sie to zmienia p-value w zaleznosci 
# od wybranej macierzy pomimo tego ze w zadnym przypadku nie na tyle zeby wyszla autokorelacja


#Moran I
moran1<-moran.test(data$freq, cont.listw)
moran1  # brak autokorelacji dla samej zmiennej freq

moran2<-moran.test(data$freq, knn.listw)
moran2 # brak autokorelacji dla samej zmiennej freq

moran3<-moran.test(data$freq, radius.listw)
moran3 # brak autokorelacji dla samej zmiennej freq

moran4<-moran.test(data$freq, knn_inv.dist.listw)
moran4 # brak autokorelacji dla samej zmiennej freq

moran5<-moran.test(data$freq, knn_inv.dist2.listw)
moran5 # brak autokorelacji dla samej zmiennej freq





##### To dalej to tez zalezy co potrzebujesz bo to jest robione pod mape, lokalne testy statystyczne itp
# No to niektore rzeczy musza sie w jakiejs formie znalezc ale to sobie do swoich wykorzystaj na ile chcesz
# Ja po prostu przekopiowalem z zajec wszystko

# Moran scatterplot – step by step version

freq_s<-as.data.frame(scale(data$freq))  #standardization of variable
# chcecking the average and standard deviation
round(mean(freq_s$V1),0) 
sd(freq_s$V1)

freq_l<-lag.listw(cont.listw, freq_s$V1) # spatial lag of x
morlm<-lm(freq_l~freq_s$V1) # linear regression            
summary(morlm)
slope<-morlm$coefficients[2] # coefficient from regression
intercept<-morlm$coefficients[1] # constant term in regression

plot(freq_s$V1, freq_l, xlab="freq_s",ylab="spatial lag of freq_s", pch="*")  
abline(intercept, slope)  # regression line
abline(h=0, lty=2) # supplementary horizontal line y=0
abline(v=0, lty=2) # supplementary vertical line x=0


# Mapping of moranscatterplots quarts

cond1<-ifelse(freq_s>=0 & freq_l>=0, 1,0)  # I quarter
cond2<-ifelse(freq_s>=0 & freq_l<0, 2,0)  # II quarter
cond3<-ifelse(freq_s<0 & freq_l<0, 3,0)  # III quarter
cond4<-ifelse(freq_s<0 & freq_l>=0, 4,0)  # IV quarter
cond.all<-cond1+cond2+cond3+cond4 # all quarters in one
cond.all
cond<-as.data.frame(cond.all)
is.data.frame(cond)

# map
brks<-c(1,2,3,4)
cols<-c("grey25", "grey60", "grey85", "grey45")
plot(com, col=cols[findInterval(cond$V1, brks)])
legend("bottomleft", legend=c("I Q - HH – high surrounded by high", "II Q - LH - low surrounded by high", "III Q - LL - low surrounded by low", "IV Q - HL - high surrounded by low"), fill=cols, bty="n", cex=0.80)
title(main="Mapping of Moranscatterplot results
frequency in 2019 elections")


# join count test


var.factor<-factor(cut(data$freq, breaks=c(0, 50, 60, 100), labels=c("low", "medium", "high")))
head(var.factor)

# parameters of graphics
brks1<-c(0, 50, 60, 100)
cols<-c("red", "blue", "green")

# scatterplot of values
plot(data$freq, bg=cols[findInterval(data$freq, brks1)], pch=21)
abline(h=c(50,60), lty=3)

# spatial distribution with three colours
plot(com, col=cols[findInterval(data$freq, brks1)])
plot(pov, add=TRUE, lwd=2)
title(main="Frequency in 2019 elections")
legend("bottomleft", legend=c("low", "medium", "high"), leglabs(brks1), fill=cols, bty="n")


joincount.test(var.factor, cont.listw) # nigdzie nie ma autokorelacji
joincount.test(var.factor, knn_inv.dist.listw) # autokorelacja w srodkowej grupie
joincount.test(var.factor, knn_inv.dist2.listw) # tutaj z kolei jest w 1 grupie a w 2 prawie



# local moran  - tu musialem zmienic nazwy gmin na kody bo sie nazwy powtarzaly a nie chcialem tracic na to czasu
# mozna to tez dla innych macierzy zrobic oczywiscie

locM<-localmoran(spNamedVec("freq", data), cont.listw)
oid1<-order(data$Kod)
locMorMat<-printCoefmat(data.frame(locM[oid1,], row.names=data$Kod[oid1]), check.names  = FALSE)

# map of the significance of Moran's local statistics
names(locMorMat)[5]<-"Prob"
brks<-c(min(locMorMat[,5]), 0.05000, 0.95000, max(locMorMat[,5]))
cols<-c("grey30", "grey90", "grey60")

plot(com, col=cols[findInterval(locMorMat[,5], brks)])
legend("bottomleft", legend=c("surrounded by relatively high values, locM>0", "insignificant", "surrounded by relatively low values, locM<0"), fill=cols, bty="n", cex=0.75)
title(main=" Local Moran statistics ", cex=0.7)
plot(pov, add=TRUE, lwd=2)




# tu jeszcze moran dla samej zmiennej opos i dla unemployment, moze mozna pokazac ze np zmienne
# niezalezne maja przestrzenna autokorelacje, natomiast dalsze wykresy itp to usunalem, jak chcesz to sa w starym pliku

#Moran I - opos
moran1_opos<-moran.test(data$opos, cont.listw)
moran1_opos  # jest autokorelacja dla samej zmiennej opos

moran4_opos<-moran.test(data$opos, knn_inv.dist.listw)
moran4_opos # a tu nie

moran5_opos<-moran.test(data$opos, knn_inv.dist2.listw)
moran5_opos # ani tu



#Moran I - unemployment
moran1_unempl<-moran.test(data$unemployment, cont.listw)
moran1_unempl  # nie ma

moran4_unempl<-moran.test(data$unemployment, knn_inv.dist.listw)
moran4_unempl # jest

moran5_unempl<-moran.test(data$unemployment, knn_inv.dist2.listw)
moran5_unempl # jest





########################################################################################################
# 1
########################################################################################################



###### modele


# ols (od tego moran test musi wyjsc dobrze)


# model wyjsciowy

formula <- freq ~ expenses + femin + income + migration + people_density2019 + prework + postwork + benefit500 + unemployment + water + sewage + gas + opos + mean_enabled

model_lm<-lm(formula, data=data) 
summary(model_lm) # expenses, income and water are insignificant but we'll keep them for now because
# it may change

### diagnostics - tak do konca nw czy potrzebujemy poza czescia przestrzenna ale krotko mozna wspomniec

bptest(model_lm) # there is heteroscedasticity

# Ramsey’s test for functional form
# H1: when non-linear variables (like powers of the variables) should be included then model is mis-specified
resettest(model_lm, power=2, type="regressor") # model is misspecified 	

# spatial distribution of OLS residuals - standardowo up to you jak z tym wykresem 
summary(model_lm$residuals)
res<-model_lm$residuals
brks<-c(min(res), mean(res)-sd(res), mean(res), mean(res)+sd(res), max(res))
cols<-c("steelblue4","lightskyblue","thistle1","plum3")
plot(com, col=cols[findInterval(res,brks)])
plot(pov, add=TRUE, lwd=2)
title(main="Reszty w modelu MNK")
legend("bottomleft", legend=c("<mean-sd", "(mean-sd, mean)", "(mean, mean+sd)", ">mean+sd"), leglabs(brks1), fill=cols, bty="n")


# przestrzenne testy - tutaj generalnie trzeba pokazac ze wszystkimi rodzajami macierzy razem (przynajmniej dla testu morana I)

# z contiguity matrix
lm.morantest(model_lm, cont.listw) # p-value = 0.04814 - wynik wskazuje ze jest autokorelacja przestreznna i nalezy ten problem rozwiazac

resid<-factor(cut(model_lm$residuals, breaks=c(-100, 0, 100), labels=c("negative","positive")))
joincount.test(resid, cont.listw) # tutaj nie wskazuje na przestrzenna autokorelacje, az tak, jedynie w 1 grupie jest pvalue<10%

# z knn matrix
lm.morantest(model_lm, knn.listw) # p-value = 0.3424 - z ta macierza nie ma

joincount.test(resid, knn.listw) # wg tego w 2 grupie jest (pvalue = 0.03229)


# z radius matrix
lm.morantest(model_lm, radius.listw) # p-value = 0.04784 - podobnie do pierwszej

joincount.test(resid, radius.listw) # za to tu nie ma


# z inverse distance matrix
lm.morantest(model_lm, knn_inv.dist.listw) # 0.15 - nie ma

joincount.test(resid, knn_inv.dist.listw) # nie ma


# z squared inverse distance matrix
lm.morantest(model_lm, knn_inv.dist2.listw) # 0.0887 - w zasadzie nie ma

joincount.test(resid, knn_inv.dist2.listw) # w pierwszej blisko ale nie do konca




# Wnioski: nie wszystkie macierze uchwytuja przestrzenna autokorelacje ale contiguity i radius tak,
# wiec jesli chcemy uwzglednic ten prezstreznny czynnik to powinnismy wziac ktoras z macierzy pokazujacych 
# autokorelacje, bo modele z pozostalymi macierzami nic nie wniosa (tak mi sie wydaje)



######## odtad najpierw ida modele z macierza contiguity

#### modele przestrzenne na pelnej bazie danych

# GNS

# Zarowno przy tym jak i przy niektorych innych modelach po uzyciu zwyklej formuly niestety dla niektorych zmiennych pojawiaja sie braki wynikow przy statystykach.
# Jest to zwiazane z jakimis obliczeniowymi problemami z macierza Hessego (Hessian matrix).
# Rozwiazania ktore czasem pozwalaja sie z tym uporac to: 1) usuniecie argumentu method = 'LU' co niestety mocno spowalnia obliczenia
# lub 2) uzycie argumentu trs "a vector of powered spatial weights matrix traces". Uzycie
# tych wag jest zalecane przez autorow szczegolnie przy liczeniu bledu standardowego lambdy i pozwala na obejscie problemow
# z macierza Hessego: dokumentacja: https://mran.microsoft.com/snapshot/2017-08-23/web/packages/spdep/spdep.pdf
# Przy czym trs nie zawsze rozwiazuje problem

#Opis trs z dokumentacji dla Cb do informacji jak bedziesz to pisal: (default NULL, if given, a vector of powered spatial weights matrix traces output)
#by trW; when given, insert the asymptotic analytical values into the numerical
#Hessian instead of the approximated values; may be used to get around some
#problems raised when the numerical Hessian is poorly conditioned, generating
#NaNs in subsequent operations. When using the numerical Hessian to get the
#standard error of lambda, it is very strongly advised that trs be given, as the
#parts of fdHess corresponding to the regression coefficients are badly approximated, affecting the standard error of lambda; the coefficient correlation matrix
#is unusable

GNS_1<-sacsarlm(formula, data=data, listw=cont.listw, type="sacmixed")
summary(GNS_1)
# Manski model nie jest dobry, tylko pojedyncze lagi x-ow sa istotne (migration, density, postwork, i opos na granicy).
# Rho (pvalue=0.0034352) i lambda (pvalue=0.0001501) niby sa istotne ale tak naprawde sie znosza wzajemnie (rho= -0.3494, lambda=0.36188)
#wiec to nie jest dobry model, ewidentnie ma za duzo elementow. Rowniez Akaike Information Criterion = 14115 jest wyzsze niz dla modelu liniowego


# schodzimy poziom nizej do dwuskladnikowych

#SAC
SAC_1<-sacsarlm(formula, data=data, listw=cont.listw)
summary(SAC_1)
#tym razem rho nieistotne, lambda tez nie choc nie ma daleko (pvalue=0.14224) wiec byc moze trzeba isc w strone lambdy.
#AIC juz nizszy ale wciaz wyzszy niz dla lm


#SDEM
SDEM_1<-errorsarlm(formula, data=data, listw=cont.listw, etype="emixed", method="LU")
summary(SDEM_1)
#wszystkie lagi (thety) nieistotne, lambda znowu na granicy.
#AIC wreszcie ponizej lm ale ledwo ledwo, i wyzsza niz dla SAC


#SDM
SDM_1<-lagsarlm(formula, data=data, listw=cont.listw, type="mixed") 
summary(SDM_1)
# thety nieistotne, tym razem tez rho jest na granicy wiec mozna sprawdzic rowniez
# model z samym rho
# AIC jak dla SDEM, dosc wysokie


# jednoskladnikowe: 

#SAR
SAR_1<-lagsarlm(formula, data=data, listw=cont.listw)
summary(SAR_1)
# samo Rho nie jest istotne a AIC jest wyzszy niz dla lm

#SLX
SLX_1<-lmSLX(formula, data=data, listw=cont.listw)
summary(SLX_1)
# tak jak spodziewane lagi nie sa istotne wiec nie sa rozwiazaniem u nas

#SEM

SEM_1<-errorsarlm(formula, data=data, listw=cont.listw, method="LU")
summary(SEM_1)

# Lambda jest na granicy p-value=0.1114, lambda=0.050481, wiec malutki efekt
# ewentualny.
# AIC taki sam jak dla lm


# porownanie modeli
LR.sarlm(GNS_1, SAC_1) # pvalue=0.9579>>5% SAC lepszy
LR.sarlm(GNS_1, SDEM_1) # ledwo ledwo sdem lepszy
LR.sarlm(GNS_1, SDM_1) # jw
LR.sarlm(GNS_1, SAR_1) # SAR lepszy
LR.sarlm(GNS_1, SLX_1) # tu juz bardzo bardzo ledwo na styk
LR.sarlm(GNS_1, SEM_1) # SEM lepszy
# czyli raczej droga SAC 

LR.sarlm(SAC_1, SAR_1) # SAR lepszy
LR.sarlm(SAC_1, SEM_1) # SEM znacznie lepszy


screenreg(list(GNS_1, SAC_1, SDM_1, SDEM_1, SAR_1, SEM_1, model_lm))
# byc moze nalezaloby przeskalowac zmienne na samym poczatku, zeby wyniki lepiej wychodzily w tabeli, ja juz tego nie robilem bo to trzeba
# potem wszystkie modele od nowa wykonac




# Wnioski: Najlepszy model to SEM, jednak nawet sama lambda nie jest do konca istotna
# ale tak jak juz pisalem jesli chcemy uwzglednic w modelu autokorelacje prezstrzenna
# to chyba trzeba wybrac ten model ktory wykazuje minimalne sklonnosci lambdy do istotnosci
# Sa natomiast jeszcze zmienne zupelnie nieistotne: income i expenses ktore byc moze sa ze soba skorelowane i sie znosza oraz water


# diagnostyka SEM:
summary(SEM_1, Hausman=TRUE) # niestety p-value od testu Hausmana = 0.99576 wiec
#model SEM nie jest lepszy od modelu liniowego, co tez widac po tym ze AIC jest takie samo jak AIC dla lm

moran.test(SEM_1$residuals, cont.listw)
# autokorelacji sie pozbylismy tym modelem


### W takim razie usuniemy jakies nieistotne zmienne i probujemy od nowa

          

########################################################################################################
# 2
########################################################################################################



######### model bez zmiennej expenses (bo jest bardziej nieistotna niz income, a one moga byc ze soba skorelowane wiec moze income zrobi sie istotna w ten sposob)


formula2 <- freq ~ femin + migration + income + people_density2019 + prework + postwork + benefit500 + unemployment + water + sewage + gas + opos + mean_enabled

model_lm2<-lm(formula2, data=data) 
summary(model_lm2) # income teraz stal sie istotny, jeszcze water jest b.nieistotna

lrtest(model_lm, model_lm2) # mozna usunac, model ograniczony jest istotnie lepszy

# bez water
formula3 <- freq ~ femin + migration + income + people_density2019 + prework + postwork + benefit500 + unemployment + sewage + gas + opos + mean_enabled

model_lm3<-lm(formula3, data=data) 
summary(model_lm3) # teraz juz wszystkie sa istotne

lrtest(model_lm, model_lm3) # rowniez nalezy usunac, model 3 jest lepszy



# przestrzenne testy na macierzy contiguency i dla radiusowej, ktora poprzednio tez wykazywala autokorelacje

lm.morantest(model_lm3, cont.listw) # 0.04808 # wciaz jest autokorelacja

resid<-factor(cut(model_lm3$residuals, breaks=c(-100, 0, 100), labels=c("negative","positive")))
joincount.test(resid, cont.listw) # w pierwszej grupie prawie


lm.morantest(model_lm3, radius.listw) # 0.04762 # wciaz jest 

resid<-factor(cut(model_lm3$residuals, breaks=c(-100, 0, 100), labels=c("negative","positive")))
joincount.test(resid, radius.listw) # w pierwszej grupie bardziej, ale jednak nie ma




# Modele przestrzenne bez zmiennych water i expense (dalej dla macierzy contiguency)

# GNS
GNS_3<-sacsarlm(formula3, data=data, listw=cont.listw, type="sacmixed")
summary(GNS_3)
# Manski model nie jest dobry, tylko pojedyncze lagi x-ow sa istotne (migration a na granicy jest wiele roznych: income, density, postwork...).
# Rho (pvalue=0.004849) i lambda (pvalue=0.00026613) niby sa istotne ale tak naprawde znowu sie znosza wzajemnie
#wiec to nie jest dobry model, ewidentnie ma za duzo elementow. Rowniez Akaike Information Criterion = 14108 jest wyzsze niz dla modelu liniowego,
#ale nizsze niz bylo ze wszystkimi zmiennymi dla tego modelu


#SAC
SAC_3<-sacsarlm(formula3, data=data, listw=cont.listw, method="LU")
summary(SAC_3)
# rho nieistotne za to lambda prawie istotna wiec byc moze trzeba sie pozbyc jeszcze rho


#SDEM
SDEM_3<-errorsarlm(formula3, data=data, listw=cont.listw, etype="emixed", method="LU")
summary(SDEM_3)
# tutaj  lambda juz mniej istotna ale nadal ponizej 10%
# ACI takie jak przy lm


# Tutaj wchodzi ta macierz trMat, dzieki ktorej da sie obliczyc SDM bez usuwania method = 'LU'
W.c<-as(as_dgRMatrix_listw(cont.listw), "CsparseMatrix") 
trMat<-trW(W.c, type="mult") 

#SDM
SDM_3<-lagsarlm(formula3, data=data, listw=cont.listw, type="mixed", method="LU", trs = trMat) 
summary(SDM_3)
# AIC bez zmian, lagi zupelnie nieistotne, rho na granicy


# jednoskladnikowe:

#SAR
SAR_3<-lagsarlm(formula3, data=data, listw=cont.listw, method="LU")
summary(SAR_3)
#AIC wyzsze niz dla lm nawet i rho nieistotne


#SLX
SLX_3<-lmSLX(formula3, data=data, listw=cont.listw)
summary(SLX_3)
#oczywiscie lagi nieistotne


#SEM
SEM_3<-errorsarlm(formula3, data=data, listw=cont.listw, method="LU")
summary(SEM_3)
# Lambda jest znowu na granicy p-value=0.11133, lambda=0.050479, wiec wyniki niemalze identyczne co dla modelu ze wszystkimi zmiennymi
# AIC dalej sie nawet nie rozni od modelu liniowego




LR.sarlm(GNS_3, SAC_3) # SAC lepszy
LR.sarlm(GNS_3, SDEM_3) # ledwo sdem lepszy
LR.sarlm(GNS_3, SDM_3) # jw
LR.sarlm(GNS_3, SAR_3) # SAR lepszy
LR.sarlm(GNS_3, SLX_3) # tu juz bardzo bardzo ledwo na styk
LR.sarlm(GNS_3, SEM_3) # SEM lepszy
# czyli raczej droga SAC 

LR.sarlm(SAC_3, SAR_3) # SAR lepszy
LR.sarlm(SAC_3, SEM_3) # SEM znacznie lepszy


screenreg(list(GNS_3, SAC_3, SDM_3, SDEM_3, SAR_3, SEM_3, model_lm3))


# diagnostyka SEM:
summary(SEM_3, Hausman=TRUE) # niestety p-value od testu Hausmana = 0.99176 wiec
#model SEM nadal nie jest lepszy od modelu liniowego, co tez widac po tym ze AIC jest takie samo jak AIC dla lm

moran.test(SEM_3$residuals, cont.listw)
# autokorelacji sie pozbylismy tym modelem


# Wnioski: Tym razem wszystkie zmienne podstawowe sa istotne, ale w zasadzie niczego to nie zmienia
# Najlepszy z przestrzennych jest SEM ale lanbda nadal minimalnie powyzej 10%. Zyskujemy na tym ze model
# nie ma zmiennych nieistotnych no i AIC jest minimalnie nizszy dla SEM bez tych 2 zmiennych niz dla pierwszego SEM




########################################################################################################
# 3
########################################################################################################

## w takim razie sprobujemy dodac do modelu spatio-temporal lag frekwencji poprzedniej tj z ostatnich wyborow.
# Wybory oczywiscie byly 4 lata wczesniej wiec w 2015 ale to jest poprzedni okres tak jakby i do tego mozemy sprobowac sie odniesc
# zeby zobaczyc czy to cos pomoze


############ frekwencja w roku poprzednim


#przygotowanie danych
elect_old<-read.csv("data/2015-gl-lis-gm.csv", header=TRUE, dec=",", sep=";")


matched <- intersect(data$Kod, elect_old$TERYT)
all <-  union(data$Kod, elect_old$TERYT)
non_matched <- all[!all %in% matched]
non_matched

# zsumowanie warszawy do 1 obs tak jak poprzednio
elect_old[elect_old$TERYT == 146502, c(5,26)] <- colSums(elect_old[elect_old$TERYT >= 146501 & elect_old$TERYT <= 146519, c(5,26)])
elect_old[elect_old$TERYT == 146502, 2:3] <- c(146501, "Warszawa")

# removing rest of warsaw districts and abroad/ships

elect_old <- elect_old[elect_old$TERYT < 146502 | elect_old$TERYT > 149901,]


# 320304 is code of gmina Ostrowice which bancrupted in 2019 and was divided between two other gmina's. There isn't an easy way
# to track those division and to match it with our data so we simply remove this gmina from the data as well
elect_old <- elect_old[elect_old$TERYT != 229801 & elect_old$TERYT != 229901 & elect_old$TERYT != 320304,]


# frequency in 2015 elections

elect_old$freq15 <- elect_old$GĹ.osy.waĹĽne / elect_old$Liczba.wyborcĂłw *100


elect_old <- elect_old[c(2, 44)]

data <- merge(x = data, y = elect_old, by.x = "Kod", by.y = "TERYT")


############## model ze spatio-temporal interaction of freq (poprzednie wybory) i bez zmiennych nieistotnych


data$freq_st<-lag.listw(cont.listw, data$freq15)

formula_freq_st <- freq ~ femin + migration + income + people_density2019 + prework + postwork + benefit500 + unemployment + sewage + gas + opos + mean_enabled + freq_st


# najpierw model w ktorym mamy nasza spatio-temporal lag of frequency oraz rho (czyli spatial lag of frequency) zeby zobaczyc
# ktora ma silniejszy wplyw i jak to wychodzi
SAR_freq15<-lagsarlm(formula_freq_st, data=data, cont.listw, tol.solve=3e-30)
summary(SAR_freq15)  
# freq_st jako zmienna nie jest istotna zupelnie ani rho takze nie jest istotne wiec wyglada na to ze nie jest to rozwiazaniem


# drugi model to SEM do ktorego dodajemy spatio-temporal lag of freq przez co uzyskujemy cos w stylu SAC tylko ze spatio-temporal
#zamiast zwyklego spatial lag of freq. Skoro SAC bylo najlepsze z dwuskladnikowych modeli a SEM z jednoskladnikowych to moze taki model
# pozwoli uchwycic nasze zjawisko
SEM_freq15<-errorsarlm(formula_freq_st, data=data, cont.listw, tol.solve=3e-30, method = "LU")
summary(SEM_freq15) # freq_st nadal zupelnie nieistotne a lambda tym razem mniej niz przy zwyklym modelu bez 
# spatio-temporal lag. Wyglada na to ze dodanie spatio-temporal lag of freq nie rozwiaze problemu




######## niektore z naszych zmiennych sa od poczatku zagregowane, natomiast mamy tez zmienne tylko z 2019 r.
# Sa takie zmienne ktore raczej na pewno nie wplywaja przestrzennie rowniez poprzez spatio-temporal lags 
# z poprzedniego okresu (jak femin) oraz takie ktore dotycza wyborow wiec sa niedostepne z roku 2018 ale rozwazmy dodanie niektorych z tych ktore sa mozliwe jako lagi w ten sposob




####### troche dodatkowych z 2018 roku

# gas and sewage

wat_sew_gas18<-read.csv("data/wodkangaz18.csv", header=TRUE, dec=",", sep=";")


colnames(wat_sew_gas18)[4:5] <- c("sewage18", "gas18")
wat_sew_gas18[c(2,6)] <- NULL
summary(wat_sew_gas18$sewage18)
summary(wat_sew_gas18$gas18)


#removing last letter meaning type of region (like before)
wat_sew_gas18$Kod <- wat_sew_gas18$Kod %>% as.character() %>% substr(1, nchar(wat_sew_gas18$Kod)-1) %>% as.numeric()

#adding this variable to data
data <- merge(x = data, y = wat_sew_gas18, by = 'Kod', all.x = T)


# people density once again
people<-read.csv("data/ludnosc 2016-2019.csv", header=TRUE, dec=",", sep=";")

colnames(people)[5] <- "people_density2018"
people[c(2:4,6:7)] <- NULL

people$Kod <- people$Kod %>% as.character() %>% substr(1, nchar(people$Kod)-1) %>% as.numeric()

data <- merge(x = data, y = people, by = 'Kod', all.x = T)



# prework, postwork
people_prod<-read.csv("data/ludnosc ogol przed poprod.csv", header=TRUE, dec=",", sep=";")

colnames(people_prod)[5] <- "ogolem18"
people_prod$prework18 <- people_prod$w.wieku.przedprodukcyjnym.ogĂłĹ.em.2018..osoba. / people_prod$ogolem18
people_prod$postwork18 <- people_prod$w.wieku.poprodukcyjnym.ogĂłĹ.em.2018..osoba. / people_prod$ogolem18
summary(people_prod$prework18)
summary(people_prod$postwork18)
people_prod[c(2:4, 6:15)] <- NULL

people_prod$Kod <- people_prod$Kod %>% as.character() %>% substr(1, nchar(people_prod$Kod)-1) %>% as.numeric()

data <- merge(x = data, y = people_prod, by = 'Kod', all.x = T)


#fem

fem18<-read.csv("data/feminizacja18.csv", header=TRUE, dec=",", sep=";")

colnames(fem18)[3] <- "femin18"
fem18[c(2,4)] <- NULL


fem18$Kod <- fem18$Kod %>% as.character() %>% substr(1, nchar(fem18$Kod)-1) %>% as.numeric()

data <- merge(x = data, y = fem18, by = 'Kod', all.x = T)



#soc_ben - 500+ (za caly rok miesiecznie srednio)

soc_ben18<-read.csv("data/swiadczenie wychowawcze 500 18.csv", header=TRUE, dec=",", sep=";")


soc_ben18$Kod <- soc_ben18$Kod %>% as.character() %>% substr(1, nchar(soc_ben18$Kod)-1) %>% as.numeric()


matched <- intersect(soc_ben18$Kod, people_prod$Kod)
all <-  union(soc_ben18$Kod, people_prod$Kod)
non_matched <- all[!all %in% matched] 
non_matched #We need to remove this unexisting gmina



soc_ben18 <- soc_ben18[soc_ben18$Kod != 320304,]
# calculating on the number of people living there
soc_ben18$benefit50018 <- soc_ben18$przeciÄ.tna.miesiÄ.czna.liczba.rodzin.pobierajÄ.cych.Ĺ.wiadczenie.2018.... / people_prod$ogolem18
soc_ben18[c(2:4)] <- NULL

data <- merge(x = data, y = soc_ben18, by = 'Kod', all.x = T)



#unempl

unempl18<-read.csv("data/bezrobocie18.csv", header=TRUE, dec=",", sep=";")

colnames(unempl18)[3] <- "unemployment18"
unempl18[c(2,4)] <- NULL


unempl18$Kod <- unempl18$Kod %>% as.character() %>% substr(1, nchar(unempl18$Kod)-1) %>% as.numeric()

data <- merge(x = data, y = unempl18, by = 'Kod', all.x = T)

# some na's, let's leave them with their values for 2019
data[is.na(data$people_density2018),]$people_density2018 <- data[is.na(data$people_density2018),]$people_density2019
data[is.na(data$prework18),]$prework18 <- data[is.na(data$prework18),]$prework
data[is.na(data$postwork18),]$postwork18 <- data[is.na(data$postwork18),]$postwork
data[is.na(data$benefit50018),]$benefit50018 <- data[is.na(data$benefit50018),]$benefit500



data$sewage_st<-lag.listw(cont.listw, data$sewage18) 
data$gas_st<-lag.listw(cont.listw, data$gas18) 
data$people_density_st<-lag.listw(cont.listw, data$people_density2018)
data$prework_st<-lag.listw(cont.listw, data$prework18)
data$postwork_st<-lag.listw(cont.listw, data$postwork18)
data$femin_st<-lag.listw(cont.listw, data$femin18)
data$unemployment_st<-lag.listw(cont.listw, data$unemployment18) 
data$benefit500_st<-lag.listw(cont.listw, data$benefit50018)



formula_x_st <- freq ~ femin + migration + income + people_density2019 + prework + postwork + benefit500 + unemployment + sewage + gas + opos + mean_enabled + sewage_st + gas_st + people_density_st + prework_st + postwork_st + femin_st + unemployment_st + benefit500_st

# pierwszy model bedzie jako SAC z dodanymi spatio-temporal lags zmiennych niezaleznych czyli praktycznie jak GNS tylko
# ze spatio-temporal lags zamiast zwyklych spatial lags
SAC_x_st<-sacsarlm(formula_x_st, data=data, cont.listw, tol.solve=3e-30)
summary(SAC_x_st)
# zadne ze spatio-temporal lags nie sa istotne wiec wyglada na to ze jednak nic one nie pomagaja ale sprobujmy uproscic model

#drugi model to SEM z dodanymi spatio-temporal lags of x czyli w efekcie cos w stylu SDEM
SEM_x_st<-errorsarlm(formula_x_st, data=data, cont.listw, tol.solve=3e-30, method = 'LU')
summary(SEM_x_st)
# lambda okazuje sie praktycznie istotna i AIC jest minimalnie nizsze od lm ale jednak wysokie, a wszystkie spatio-temporal lags
# sa nieistotne wiec to rowniez nie tedy droga jak widac






####################### Skoro zaden model nie okazal sie dostatecznie dobry to sprobujmy z druga macierza powtorzyc
# dzialania



######## odtad ida modele z macierza radius ale juz bez expenses i bez water



# GNS
GNS_r<-sacsarlm(formula3, data=data, listw=radius.listw, type="sacmixed", method = 'LU')
summary(GNS_r)
# Lagi od Migration i opos mocno istotne, reszta wcale
# Rho i lambda  niby sa istotne ale tak naprawde znowu sie znosza wzajemnie


#SAC
SAC_r<-sacsarlm(formula3, data=data, listw=radius.listw, method="LU")
summary(SAC_r)
# rho nieistotne za to lambda istotna wiec byc moze trzeba sie pozbyc jeszcze rho


#SDEM
SDEM_r<-errorsarlm(formula3, data=data, listw=radius.listw, etype="emixed", method="LU")
summary(SDEM_r)
# tutaj inne rozlozenie istotnosci lagow, ale wiekszosc nieistotna dalej, lambda prawie ze istotna



#SDM
SDM_r<-lagsarlm(formula3, data=data, listw=radius.listw, type="mixed", method="LU", trs = trMat) # z macierza trMat
summary(SDM_r)
# Gas i Unemployment istotne reszta nie, rho na granicy


# jednoskladnikowe:

#SAR
SAR_r<-lagsarlm(formula3, data=data, listw=radius.listw, method="LU")
summary(SAR_r)
#AIC wyzsze niz dla lm nawet i rho nieistotne


#SLX
SLX_r<-lmSLX(formula3, data=data, listw=radius.listw)
summary(SLX_r)
# tylko gas i prawie unemployment


#SEM
SEM_r<-errorsarlm(formula3, data=data, listw=radius.listw, method="LU")
summary(SEM_r)
# Lambda jest znowu na granicy p-value=0.1096, odrobine blizej i minimalnie wieksza, lambda=0.060549,
# AIC dalej sie nawet nie rozni od modelu liniowego




LR.sarlm(GNS_r, SAC_r) # SAC lepszy
LR.sarlm(GNS_r, SDEM_r) # sdem lepszy
LR.sarlm(GNS_r, SDM_r) # jw
LR.sarlm(GNS_r, SAR_r) # SAR lepszy
LR.sarlm(GNS_r, SLX_r) # tu juz lekko tylko
LR.sarlm(GNS_r, SEM_r) # SEM lepszy
#raczej 1 skladnikowe i nie SLX mimo tych 2 lagow istotnych

LR.sarlm(SAC_r, SAR_r) # SAR lepszy na styk
LR.sarlm(SAC_r, SEM_r) # SEM znacznie lepszy


screenreg(list(GNS_r, SAC_r, SDM_r, SDEM_r, SAR_r, SEM_r, model_lm3))


# Wnioski: SEM znowu najlepszy, wyniki zblizone do poprzednich ale chyba troche lepsze wiec mozna powiedziec ze ten model SEM
# z macierza konkretnego promienia odleglosci jest najlepszy ze wszystkich do tej pory choc to ciagle nie jest
# najlepszy wynik

# diagnostyka SEM:
summary(SEM_r, Hausman=TRUE) # nadal bardzo daleko do uznania tego modelu za lepszy od linioowego choc jest znaczna roznica wzgledem poprzednich modeli


moran.test(SEM_r$residuals, radius.listw)
# autokorelacji sie pozbylismy tym modelem



######## sprobujmy dodac spatio-temporal lags niektorych z tych zmiennych ktore byly istotne w  modelach z thethami, czyli
# unemployment & gas

formula_x_st_r <- freq ~ femin + migration + income + people_density2019 + prework + 
  postwork + benefit500 + unemployment + sewage + gas + opos + 
  mean_enabled + gas_st + unemployment_st

# pierwszy model ten sam co wtedy, SAC z dodanymi spatio-temporal lags zmiennych niezaleznych czyli praktycznie jak GNS tylko
# ze spatio-temporal lags zamiast zwyklych spatial lags
SAC_x_st_r<-sacsarlm(formula_x_st_r, data=data, radius.listw, tol.solve=3e-30)
summary(SAC_x_st_r)
# te dwie spatio-temporal lags i tak nie sa istotne tutaj


#drugi model to znowu SEM z dodanymi spatio-temporal lags of x czyli w efekcie cos w stylu SDEM
SEM_x_st_r<-errorsarlm(formula_x_st_r, data=data, radius.listw, tol.solve=3e-30)
summary(SEM_x_st_r)
# tutaj rowniez nie sa istotne






# Although Moran I statistics shows the spatial autocorelation of residuals, it is not easy to find significant
# parameters which would eliminate this problem, probably also because this effect is so small and maybe in fact
# negligible. One can also try different ways to deal with this problem. We'll try now to add completely new
# variable place_area which describes approximate area belonging to one voting place. We will divide the number
# of voting places in the given gmina by the area of this gmina



### zmienna place_area

area<-read.csv("data/powierzchnia.csv", header=TRUE, dec=",", sep=";") # powierzchnia gmin


#area
colnames(area)[3] <- "area"
area[4] <- NULL

area$Kod <- area$Kod %>% as.character() %>% substr(1, nchar(area$Kod)-1) %>% as.numeric()

data <- merge(x = data, y = area, by = "Kod")

data$place_area <- data$area / data$places #how big area more or less is for one place of voting


# now our linear model with all variables is as follows:

formula_pa <- freq ~ expenses + femin + income + migration + people_density2019 + prework + postwork + benefit500 + unemployment + water + sewage + gas + opos + mean_enabled + place_area

model_lm_pa<-lm(formula_pa, data=data) 
summary(model_lm_pa) # place_area is totally significant which is a good thing for us

# przestrzenne testy

# z contiguity matrix
lm.morantest(model_lm_pa, cont.listw) # p-value = 0.07869 - so now it's over 5% and although it is still low and close to the 5% border we can say that spatial autocorrelation is probably not a problem anymore.

# z knn matrix
lm.morantest(model_lm_pa, knn.listw) # p-value = 0.459 - z ta macierza nie ma

# z radius matrix
lm.morantest(model_lm_pa, radius.listw) # p-value = 0.1215 - tu juz tym bardziej nie ma, podczas gdy poprzednio byla

# z inverse distance matrix
lm.morantest(model_lm_pa, knn_inv.dist.listw) # 0.2205 - nie ma

# z squared inverse distance matrix
lm.morantest(model_lm_pa, knn_inv.dist2.listw) # 0.124 - w zasadzie nie ma





############ interpretation of impacts


# Let's take our model SEM_r (SEM with radius matrix) as the best one.
# SEM model has only additional spatial lag of e, unobservable effect
# It means that we can interpret the impacts in the same way we do it for
# OLS, because the indirect, spillower effect is equal to 0. Hence we can use estimates
# to describe the effect of x on y e.g.

# I mozna by jakos podac wybrane przyklady tlumaczenia przykladow zeby udowodnic ze rozumiemy tylko nw czy notacji 
# nie trzebaby w tym celu zmienic


summary(SEM_r)






###################### Podsumowanie, wyniki

# Statystyka Marona I wskazywala na autokorelacje przestrzenna residualsow, ale nawet najprostsze modele mialy swoje pojedyncze
# parametry na granicy istotnosci. Udawalo sie natomiast pozbyc nimi wspomnianej autokorelacji bo w SEM Maron I nie wykazywal juz jej.
# Modele te nie byly istotnie lepsze od liniowych ale z pewnoscia byly jakims sposobem na usuniecie autokorelacji residualsow.
# Natomiast byc moze w tym przypadku nie jest to jakies konieczne, zwlaszcza jesli mozna pozbyc sie autokorelacji w jakis inny sposob np. 
# dodaniem nowej zmiennej. W kazdym razie nawet jesli niektore statystyki
# wskazuja na autokorelacje, to byc moze nie musi to oznaczac ze uzycie modeli przestreznnych znaczaco poprawi wyniki












