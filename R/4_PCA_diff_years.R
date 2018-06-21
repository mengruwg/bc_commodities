#######Vorher Script: 4_pca_years_setup ausführen

#Für Variablen ab 1970:
dat1970 <- na.omit(dat1970)
bart_spher(dat1970[,2:11])
KMOS(x=dat1970[,2:11])
VSS.scree(dat1970[,2:11])
pca.1970<-principal(dat1970[,2:11], nfactors=2)
print(pca.1970, sort=T, cut=0.53, digits=2)

#Für Variablen ab 1970:
dat1975 <- na.omit(dat1975)
bart_spher(dat1975[,2:12])
KMOS(x=dat1975[,2:12])
VSS.scree(dat1975[,2:12])
pca.1975<-principal(dat1975[,2:12], nfactors=2)
print(pca.1975, sort=T, cut=0.53, digits=2)

#Für ab 1980
dat1980 <- na.omit(dat1980)
#Tinindex entfernen damit PCA besser wird. Nicht auf Komponent geladen. 
dat1980 <- subset(dat1980, select=-c(TinIndex))
bart_spher(dat1980[,2:14])
KMOS(x=dat1980[,2:14])
VSS.scree(dat1980[,2:14])
pca.1980<-principal(dat1980[,2:14], nfactors=2)
print(pca.1980, sort=T, cut=0.53, digits=2)

#Für ab 1985
dat1985 <- na.omit(dat1985)
#TinIndex wieder entfernen, NickelIndex auch
dat1985 <- subset(dat1985, select=-c(TinIndex, NickelIndex))
bart_spher(dat1985[,2:16])
KMOS(x=dat1985[,2:16])
VSS.scree(dat1985[,2:16])
pca.1985<-principal(dat1985[,2:16], nfactors=2)
print(pca.1985, sort=T, cut=0.53, digits=2)

#Speichern ab 1980-er. Da wir diese am besten verwenden können. 
round(head(pca.1980$scores),3)
dat.scores1980<- data.frame(pca.1980$scores)
pca.scores.1980 <- cbind(dat1980$Group.1, dat.scores1980)
colnames(pca.scores.1980) <- c("Date","comm.", "ind.met.")

#Datei erstellen für Indices PCA Scores
write_rds(pca.scores.1980, "pca.1980.rds")
datcom3$Date2 <- as.yearqtr(datcom3$Date)
datcom4<- aggregate(datcom3[,2:4], list(datcom3$Date2), custommean)
write_rds(datcom4, "Ind_PCA_q.rds")
