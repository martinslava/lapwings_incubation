getwd()
setwd("C:/Users/Martin/Dropbox/lapwings_incubation_paper/incubation_paper/data")

#nacteni packagu
library(sqldf,tcltk)
library(lme4,Matrix)
library(plyr)

#nacteni dat 2015
inc<-read.delim("inkubace_2015.txt", header=T)# inkubacni rytmy z videi_2015
klima_hod<-read.delim2("klima_data_hod_2015.txt",header=T)# teploty po hodine z Budejic a srazky z  Budejic  a temelina
coor<-read.delim2("coordinates_2015.txt",header=T)# kordinaty hnizd
ornvidea<-read.delim2("orn_videa_2015.txt",header=T)# ornamentace natocenejch samic
nv<-read.delim2("hnizda_videa_2015.txt",header=T)#mayfield, lining, fate,... u natocenejch hnizd
egg2015<-read.delim("vejce_2015.txt",header=T)# rozmery vsech vajec

#nacteni dat 2016
inc<-read.delim("inkubace_2016.txt", header=T)# inkubacni rytmy z videi_2016
klima_hod<-read.delim2("klima_data_hod_2016.txt",header=T)# teploty po hodine z Budejic a srazky z  Budejic  a temelina
coor<-read.delim2("coordinates_2016.txt",header=T)# kordinaty hnizd
ornvidea<-read.delim2("orn_videa_2016.txt",header=T)# ornamentace natocenejch samic
nv<-read.delim2("hnizda_videa_2016.txt",header=T)#mayfield, lining, fate,... u natocenejch hnizd
egg2016<-read.delim("vejce_2016.txt",header=T)# rozmery vsech vajec

# pooling do 1 objektu pro oba roky???


# uprava a kontrola dat nově s přesne useknutými časy na 5:00 a 20:30

inc$pk <- ave( 1:nrow(inc), FUN=function(x) 1:length(x) ) #vytvori primarni klic
inc$datetime_start<-paste(inc$date_start,inc$time_start,sep=" ")#spojeni 2 sloupcu
inc$datetime_end<-paste(inc$date_end,inc$time_end,sep=" ")#spojeni 2 sloupcu
inc$datetime_end<-as.POSIXct(strptime(inc$datetime_end, format="%Y-%m-%d %H:%M:%S"))#zmena na pouzitelny format>
inc$datetime_start<-as.POSIXct(strptime(inc$datetime_start, format="%Y-%m-%d %H:%M:%S"))#zmena na pouzitelny format>
#inc$bout_min <- difftime(inc$datetime_end, inc$datetime_start, units="mins") # spocte delku subboutu v minutach, nyni se dela pozdeji jinak kvuli orezani casu
#inc$bout_min<-as.numeric(inc$bout_min)


#fce k predelani string (time) na numeric 
inc$time_start_new<-as.character(inc$time_start)
inc$time_start_new<-sapply(strsplit(inc$time_start_new,":"),
                           function(x) {
                             x <- as.numeric(x)
                             x[1]+x[2]/60+x[3]/3600
                           }
)
inc$time_start_new<-as.numeric(inc$time_start_new)

inc$time_end_new<-as.character(inc$time_end)
inc$time_end_new<-sapply(strsplit(inc$time_end_new,":"),
                         function(x) {
                           x <- as.numeric(x)
                           x[1]+x[2]/60+x[3]/3600
                         }
)
inc$time_end_new<-as.numeric(inc$time_end_new)

# odstraneni urcitych casti dne (napriklad zde noci, kdy M skoro nesedi a je tam dost U)

inc$time_start_new= ifelse(inc$time_start_new<=5 & inc$time_end_new>=5,5,inc$time_start_new)
inc$time_end_new= ifelse(inc$time_start_new<=20.5 & inc$time_end_new>=20.5,20.5,inc$time_end_new)
inc$bout_min=(inc$time_end_new-inc$time_start_new)*60
inc_day<-inc[c(which(inc$time_start_new>=5 & inc$time_end_new<=20.5)),]# samotne odstraneni nocniho casu
sum(inc_day$bout_min)#suma casu denni inkubace

summary(inc_day$bout_min)

inc[c(which(inc_day$bout_min<0)),]#najde pripadne hodnoty mensi nez 0

ddply(inc_day,c("ID_Nest","day_inc_per"),summarise,N=length(bout_min),sum=sum(bout_min))#kontrola jestli sedi casy v jednotlivych dnech (max 1439.9833)

x=inc_day[c(which(inc_day$sex=="U")),]#subset dat s unknown sex
sum(x$bout_min)#suma minut s unknown sex

sum(x$bout_min)/sum(inc_day$bout_min)#podil casu s unknown sex


inc_day<-inc_day[-c(which(inc_day$sex=="U")),]# odstraneni dat s unknown sex

ddply(inc_day,c("ID_Nest","day_inc_per"),summarise,sum_celk=sum(bout_min))#sumarizace po hnizdodnech

x=ddply(inc_day,c("ID_Nest"),summarise,sum_celk=sum(bout_min))  #sumarizace po hnizdech

inc_day<-sqldf("select * from inc_day where id_nest not in (select id_nest from x where sum_celk<1100)") #odstraneni hnizd pod 1100 minut

#shrnuti cisteho datasetu
x=ddply(inc_day,c("ID_Nest","day_inc_per"),summarise,sum_celk=sum(bout_min))#sumarizace po hnizdodnech
summary(x)
hist(x$sum_celk)

x<-ddply(inc_day,c("ID_Nest"),summarise,sum_celk=sum(bout_min))  #sumarizace po hnizdech
summary(x)
hist(x$sum_celk)

#vytvori identifikator hnizdodne
inc_day$id_day<-paste(inc_day$ID_Nest,inc_day$day_inc_per,sep=" ")

#odstraneni prvniho a posledniho gapu pro kazde hnizdo
#posledni
x<-ddply(inc_day,c("ID_Nest"),summarise,sum_celk=sum(bout_min),max_bout=max(bout_id),pk=max(pk))
inc_day<-sqldf("select * from inc_day where pk not in (select pk from x)")
#prvni
inc_day<-sqldf("select * from inc_day where bout_id is not '1' ")

#odstrani hnizdodny kratsi, nez 120 min
x<-ddply(inc_day,c("ID_Nest","day_inc_per","id_day"),summarise,sum_celk=sum(bout_min))
inc_day<-sqldf("select * from inc_day where id_day not in (select id_day from x where sum_celk<120)")

#konecna sumarizace

sum(inc_day$bout_min)#celkove minut
x=ddply(inc_day,c("ID_Nest"),summarise,sum_celk=sum(bout_min))#kolik minut pro jednotliva hnizda
ddply(inc_day,c("ID_Nest","day_inc_per","id_day"),summarise,sum_celk=sum(bout_min))# kolik minut pro jednotlive hnizdodny
ddply(ddply(inc_day,c("ID_Nest","day_inc_per","id_day"),summarise,sum_celk=sum(bout_min))
      ,c("ID_Nest"),summarise,ndays=length(day_inc_per))#pocet zpracovavanych dni pro  jedotliva hnizda

#shrnuti samplu v min pro jednotliva hnizda
summary(x)
sd(x$sum_celk)
hist(x$sum_celk)

ddply(inc_day,c("Type"),summarize,n=length(Type))#mnozstvi incubacnich boutu a gapu

# filtrovani pripadu kde jsou za sebou 2 zaznamy se stejnym typem
x=inc_day
x$Type=as.character(x$Type)
x$Type_prior=(c(x$Type[1],x$Type[-nrow(x)]))
x[c(which(x$Type==x$Type_prior)),]

#tvorba model framu
mink<-ddply(inc_day,c("ID_Nest","day_inc_per","id_day","date_start"),summarise,sum_celk=sum(bout_min))

#Ibm - suma Ib pro M
x<-sqldf("select * from (select id_day from mink)m
           left join
           (select sum(bout_min),id_day from inc_day where sex is 'M' and Type is 'inc' group by id_day)i
           on i.id_day=m.id_day")
mink$Ibm=x$"sum(bout_min)"# kdyz bez "" hleda tam fci
mink$Ibm[is.na(mink$Ibm)] <- 0 #nahradi NA za 0

#Ibf - suma Ib pro F
x<-sqldf("select * from (select id_day from mink)m
           left join
           (select sum(bout_min),id_day from inc_day where sex is 'F' and Type is 'inc' group by id_day)i
           on i.id_day=m.id_day")
mink$Ibf=x$"sum(bout_min)"
mink$Ibf[is.na(mink$Ibf)] <- 0

#Igm - suma Ig pro M
x<-sqldf("select * from (select id_day from mink)m
           left join
           (select sum(bout_min),id_day from inc_day where sex is 'M' and Type is 'gap' group by id_day)i
           on i.id_day=m.id_day")
mink$Igm=x$"sum(bout_min)"
mink$Igm[is.na(mink$Igm)] <- 0

#Igf - suma Ig pro F
x<-sqldf("select * from (select id_day from mink)m
           left join
           (select sum(bout_min),id_day from inc_day where sex is 'F' and Type is 'gap' group by id_day)i
           on i.id_day=m.id_day")
mink$Igf=x$"sum(bout_min)"
mink$Igf[is.na(mink$Igf)] <- 0

#Ia - suma Ib a Ig pro M, resp. F
mink$Iam=mink$Ibm+mink$Igm
mink$Iaf=mink$Ibf+mink$Igf

# kontrolni soucty
summary(mink$sum_celk-(mink$Iam+mink$Iaf))
summary(mink$Iam-(mink$Ibm+mink$Igm))
summary(mink$Iaf-(mink$Ibf+mink$Igf))

# zda nezavisi na delce hnizdodne
x<-data.frame((mink$Iam/(mink$Iam+mink$Iaf)),mink$sum_celk)
cor(x,method="spearman")#nyni 0.21...

# priprava dat pro ornamentaci samic na videich
# obj ornvidea
# agregace po exemplarich

x<-sqldf("select id_pc-substr(id_pc,-2,2) from ornvidea")
ornvidea$id_nest<-x$"id_pc-substr(id_pc,-2,2)"

m<-lmer(breast_video~1+(1|id_pc),data=ornvidea,na.action=na.exclude) # pro jednotlive obrazky
m<-lmer(breast_video~1+(1|id_nest),data=ornvidea,na.action=na.exclude) # pro jednotlive ptaky

#repeatabilita
{
vc<- VarCorr(m)
residual_var<- attr(vc,'sc')^2
intercept_var<- attr(vc$id_pc,'stddev')[1]^2
R<- intercept_var/(intercept_var+residual_var)
n<-as.data.frame(table(ornvidea$id_pc))
k<-nrow(n)
N<-sum(n$Freq)
n0<-(N-(sum(n$Freq^2)/N))/(k-1)
Rn<-R/(R+(1-R)/n0)
Rn
}
###

#pridani F ornamentace k mink
x<-ddply(ornvidea,"id_nest",summarise,Forn=mean(breast_video,na.rm=T))
mink<-sqldf("select * from mink
            join
            x
            on mink.id_nest=x.id_nest")
mink[11]<-NULL

#korelace mezi srazkami v CB a TE
cor(klima_hod$srazky_CB,klima_hod$srazky_Temelin,method='spearman')

plot(klima_hod$srazky_CB~klima_hod$srazky_Temelin)



#pridani teploty
x<-sqldf("select substr(date_time,1,10) from klima_hod")#vytvoreni samostatneho data
names(x)[1]<-paste("date")
klima_hod<-cbind(klima_hod,x[1])

x<-sqldf("select substr(date_time,12,2) from klima_hod")#vytvoreni samostatneho casu (jen cele hodiny)
names(x)[1]<-paste("time")
klima_hod<-cbind(klima_hod,x[1])
klima_hod$time<-as.numeric(klima_hod$time)

x<-klima_hod[c(which(klima_hod$time>5 & klima_hod$time<21)),]# jen casy prekryvajici se s minkem

x<-ddply(x,("date"),summarise,tmean=mean(teplota_CB),pt=sum(srazky_Temelin),pcb=sum(srazky_CB))#prumery klimatickych promennych v case prekryvu s minkem

x<-sqldf("select * from
         (select date_start from mink)m
         join  (select * from x)k
         on 
         k.date=m.date_start
         ")
mink<-cbind(mink,x[c(3:5)])

#spocteni vzdalenosti hnizd k jednotlivym stanicim
library(sp)
library(rgeos)

coor$lat<-as.numeric(levels(coor$latitude)[coor$latitude])#prevede faktor na numeric spravne
coor$long<-as.numeric(levels(coor$longitude)[coor$longitude])

#prevedeni souradnic na km od 48?N,14?E, pocita s presnosti~ 0.987
coor$nlat<-(ifelse(coor$lat<49,((coor$lat-48)*111.2), 
                   (111.2 +((coor$lat-49)*111.22))))

coor$nlong<-(ifelse(coor$lat<49,((coor$long-14)*(73.17+((coor$lat-48)*(74.62-73.17)))),
                    ((coor$long-14)*(71.70+((coor$lat-49)*(73.17-71.70))))))

sp.coor<-coor
coordinates(sp.coor)<-~nlong+nlat

chmu<-as.data.frame(cbind(c('CB','TE'),c(48.95211433,49.1937394),c(14.4690222,14.3466131)))#objekt s koordinatami chmu stanic
names(chmu)[1:3]<-paste(c("stan","lat","long"))
chmu$lat<-as.numeric(levels(chmu$lat)[chmu$lat])
chmu$nlat<-(ifelse(chmu$lat<49,((chmu$lat-48)*111.2), 
                   (111.2 +((chmu$lat-49)*111.22))))
chmu$long<-as.numeric(levels(chmu$long)[chmu$long])
chmu$nlong<-(ifelse(chmu$lat<49,((chmu$long-14)*(73.17+((chmu$lat-48)*(74.62-73.17)))),
                    ((chmu$long-14)*(71.70+((chmu$lat-49)*(73.17-71.70))))))

sp.chmu<-chmu
coordinates(sp.chmu)<-~nlong+nlat

x<-as.data.frame(gDistance(sp.chmu,sp.coor,byid=T))

coor<-cbind(coor,x)
names(coor)[11:12]<-paste(c("cb","te"))

#prirazeni vzdalenosti k meteostanicim k hnizdum
x<-sqldf("select coor.id_nest,m.id_nest,cb,te from coor
         join
         (select id_nest from mink)m
         on coor.id_nest=m.id_nest
         ")
mink<-cbind(mink,x[3:4])

# spocteni prumernych srazek

mink$precip<-(mink$pcb*(mink$cb/(mink$cb+mink$te)))+(mink$pt*(mink$te/(mink$cb+mink$te)))

# date start na datum
mink$date<-as.character(as.Date(mink$date_start))
mink$date <- strftime(mink$date, format = "%j")
mink$date<-as.numeric(mink$date)

#pridani prumerne velikosti vejce

x<-sqldf("select * from egg2015 where egg2015.id_nest in (select distinct id_nest from mink)")
mink<-sqldf("select * from 
            (select mean_vol,id_nest from x)x
            join
            mink
            on mink.id_nest=x.id_nest")
mink[3]<-NULL# odstraneni duplikovaneho sloupce
# mink<-cbind(mink,inv) # to je zrejme k nicemu...

#pridani prediktoru z hnizd

x<-sqldf("select * from nv
         join
         (select id_nest from mink)m
         on nv.id_nest=m.id_nest
         ")
mink<-cbind(mink,x[3:8])

#z- transformace prediktoru a uprava faktoru
mean(mink$date)
sd(mink$date)
mink$sdate<-(mink$date-mean(mink$date))/sd(mink$date)

mink$mean_vol<-(mink$mean_vol-mean(mink$mean_vol))/sd(mink$mean_vol)


mink$sti<-mink$date-mink$day_inc_per
mean(mink$sti)
sd(mink$sti)
mink$ssti<-(mink$sti-mean(mink$sti))/sd(mink$sti)

mean(mink$day_inc_per)
sd(mink$day_inc_per)
mink$sdip<-(mink$day_inc_per-mean(mink$day_inc_per))/sd(mink$day_inc_per)

mean(mink$Forn)
sd(mink$Forn)
mink$sForn<-(mink$Forn-mean(mink$Forn))/sd(mink$Forn)

mean(mink$tmean)
sd(mink$tmean)
mink$stmean<-(mink$tmean-mean(mink$tmean))/sd(mink$tmean)

mink$fprecip<-ifelse(mink$precip<2,
                     ifelse(mink$precip==0,0,1),2)
mink$fprecip<-as.factor(mink$fprecip)

mink$dens<-as.factor(mink$dens)

# model mink
zaloha.mink<-mink
x<-data.frame(mink$tmean,mink$mean_vol,mink$precip,mink$date,mink$sti,mink$sdip,mink$Forn,mink$day_inc_per)#test korelaci zavislych promennych
library(psych)
c<-as.matrix(cor(x),method='spearman')
test<-corr.p(c,170,adjust="holm",alpha=.05)#s library(psych)
print(test,short=FALSE)#korelace signifikantni, ale do 0.5

library(MuMIn)
options(na.action = "na.fail")
library(RVAideMemoire)
overdisp.glmer(Tic)

mia<-cbind(round(mink$Iam,digits=0),round(mink$Iaf,digits=0))
mic<-cbind(mink$Ibm,mink$Igm)
fic<-cbind(mink$Ibf,mink$Igf)


m<-glmer(mia~1+(1|id_nest),family=binomial,data=mink)
plot(augPred(m))
vc<- VarCorr(m)
residual_var<- attr(vc,'sc')^2
intercept_var<- attr(vc$id_nest,'stddev')[1]^2
R<- intercept_var/(intercept_var+residual_var)
n<-as.data.frame(table(mink$id_nest))
k<-nrow(n)
N<-sum(n$Freq)
n0<-(N-(sum(n$Freq^2)/N))/(k-1)
Rn<-R/(R+(1-R)/n0)
Rn#0.94

#Mmia
Mmia<-glmer(mia~stmean+mean_vol+fprecip+ssti+sForn+sdip+dens+(1|id_nest)+(1|id_day),family=binomial,data=mink)# s pridanym objemem vajec
mink$id_nest<-as.factor(mink$id_nest)


Mmia<-betabin(cbind(round(mink$Iam,digits=0),round(mink$Iaf,digits=0))~stmean+fprecip+ssti+sForn+sdip,~id_nest,data=mink,link="cloglog")

Mmia<-betareg(I~stmean+fprecip+ssti+sForn+sdip+dens+1|id_nest,data=mink)

I<-(mink$Iam/(mink$Iam+mink$Iaf))
1
dmia<-dredge(Mmia)
dmiavg<-model.avg(dmia)
summary(dmiavg)

x<-glmer(mia~sForn+(1|id_nest)+(1|id_day),family=binomial,data=mink)
m<-glmer(mia~stmean+fprecip+ssti+sForn+sdip+(1|ID_Nest)+(1|id_day),family=binomial,data=mink)
m<-glmer(mia~stmean+ssti+sForn+sdip+(1|ID_Nest)+(1|id_day),family=binomial,data=mink)
m<-glmer(mia~stmean+sForn+sdip+(1|ID_Nest)+(1|id_day),family=binomial,data=mink)
m<-glmer(mia~sForn+sdip+(1|ID_Nest)+(1|id_day),family=binomial,data=mink)#posledni do dAICc<2 a OBROVSKEJ skok v sign. k horsimu
m<-glmer(mia~stmean+fprecip+ssti+sForn+sdip+dens+(1|ID_Nest)+(1|id_day),family=binomial,data=mink)
m<-glmer(mia~stmean+fprecip+ssti+sForn+sdip+dens+(1|ID_Nest)+(1|id_day),family=binomial,data=mink)

#z-stand Iam a Iaf
mink$siam<-(mink$Iam-mean(mink$Iam))/sd(mink$Iam)
mink$siaf<-(mink$Iaf-mean(mink$Iaf))/sd(mink$Iaf)

#Mmic
Mmic<-glmer(mic~siam+stmean+mean_vol+fprecip+ssti+sdip+Forn+dens+(1|id_nest)+(1|id_day),family=binomial,data=mink)
dmic<-dredge(Mmic) 
dmicvg<-model.avg(dmic)
summary(dmicvg)

m<-glmer(mic~siam+stmean+dens+(1|ID_Nest)+(1|id_day),family=binomial,data=mink)
m<-glmer(mic~siam+stmean+(1|ID_Nest)+(1|id_day),family=binomial,data=mink)
m<-glmer(mic~siam+fprecip+(1|ID_Nest)+(1|id_day),family=binomial,data=mink)#jiz s dAICc>2

#Mfic
Mfic<-glmer(fic~siaf+stmean+mean_vol+fprecip+ssti+sdip+Forn+dens+(1|id_nest)+(1|id_day),family=binomial,data=mink)

Mfic<-betabin(cbind(mink$Ibf,mink$Igf)~siaf+stmean+fprecip+ssti+sdip+Forn+dens,~ID_Nest,data=mink)


dfic<-dredge(Mfic)
dficvg<-model.avg(dfic)
summary(dficvg)

m<-glmer(fic~siaf+stmean+dens+fprecip+sdip+(1|ID_Nest)+(1|id_day),family=binomial,data=mink)
m<-glmer(fic~siaf+stmean+dens+fprecip+(1|ID_Nest)+(1|id_day),family=binomial,data=mink)
m<-glmer(fic~siaf+dens+fprecip+(1|ID_Nest)+(1|id_day),family=binomial,data=mink)
m<-glmer(fic~siaf+fprecip+sdip+(1|ID_Nest)+(1|id_day),family=binomial,data=mink)
m<-glmer(fic~siaf+Forn+fprecip+(1|ID_Nest)+(1|id_day),family=binomial,data=mink)
m<-glmer(fic~siaf+fprecip+(1|ID_Nest)+(1|id_day),family=binomial,data=mink)#jiz s dAICc>2

#celkova Ic 
mink$tib<-mink$Ibm+mink$Ibf
mink$tig<-mink$Igm+mink$Igf
tic<-cbind(mink$tib,mink$tig)

Tic<-glmer(tic~stmean+fprecip+ssti+mean_vol+sdip+Forn+dens+Iam+(1|id_nest)+(1|id_day),family=binomial,data=mink)

dTic<-dredge(Tic)
dTicvg<-model.avg(dTic)
summary(dTicvg)


summary(dmiavg)
summary(dficvg)
summary(dmicvg)
summary(dTicvg)
#grafy

x=ddply(mink,c("id_nest","sForn"),summarise,mia=mean(Iam/sum_celk))
m=glm(mia~sForn,data=x)
plot(mia~sForn,data=x,abline(m,col="blue",lwd=2))
abline(m,col="blue",lwd=2)
x=resid(m)
shapiro.test(x)

#ggplot mia~sForn s prumerovanymi mia pro jednotliva hnízda
{g<-ggplot(data=x, aes(x=sForn, y=mia),fill="blue")
g<-g + geom_point(shape=2,size=4,fill="blue") +   
  geom_smooth(method=loess, span=1,se=T,alpha=0.5)

g<-g + labs(x="z-standardized ornamentation score",
            y="mean male incubation attendance")

g<-g+coord_cartesian(xlim=c(-2.55,2),ylim= c(-0.01,0.6))

g<-g+theme(axis.text=element_text(size=14,face="bold"),
           axis.title=element_text(size=16,face="bold")
)

print(g)}

#ggplot vztahu mezi Iam a ic

{
  #x=subset(mink,sum_celk>800)#vzfiltruje dny s vice nez 800 analyzovanymi min - alternativa
  x=ddply(mink,c("id_nest"),summarise,Iam=mean(Iam/sum_celk),tic=mean(tib/(tib+tig)))
  
  g<-ggplot(data=x, aes(y=tic, x=Iam),fill="blue")
 g<-g + geom_point(shape=2,size=4,fill="blue") +   
   geom_smooth(method=loess, span=1,se=T,alpha=0.5)
 
 g<-g + labs(x="mean male incubation attendance per nest",
             y="mean total incubation constancy per nest")
 
# g<-g+coord_cartesian(xlim=c(-2.55,2),ylim= c(-0.01,0.6))
 
 g<-g+theme(axis.text=element_text(size=14,face="bold"),
            axis.title=element_text(size=16,face="bold")
 )
 
 print(g)}

#ggplot Icm~Iam

{
  x=ddply(mink,c("id_nest"),summarise,iam=mean(Iam/sum_celk),icm=mean(Ibm/(Iam+0.0000000001)))
                                                                       
  g<-ggplot(data=x, aes(x=iam, y=icm),fill="blue")
  g<-g + geom_point(shape=2,size=4,fill="blue") +   
    geom_smooth(method=loess, span=1,se=T,alpha=0.5)
  
  g<-g + labs(x="mean male incubation attendance per nest",
              y="mean male incubation constancy per nest")
  
  # g<-g+coord_cartesian(xlim=c(-2.55,2),ylim= c(-0.01,0.6))
  
  g<-g+theme(axis.text=element_text(size=14,face="bold"),
             axis.title=element_text(size=16,face="bold")
  )
  
  print(g)}

#ggplot Icf~Iaf

{
  x=ddply(mink,c("id_nest"),summarise,iaf=mean(Iaf/sum_celk),icf=mean(Ibf/(Iaf)))
  
  g<-ggplot(data=x, aes(x=iaf, y=icf),fill="blue")
  g<-g + geom_point(shape=2,size=4,fill="blue") +   
    geom_smooth(method=loess, span=1,se=T,alpha=0.5)
  
  g<-g + labs(x="mean female incubation attendance per nest",
              y="mean female incubation constancy per nest")
  
  # g<-g+coord_cartesian(xlim=c(-2.55,2),ylim= c(-0.01,0.6))
  
  g<-g+theme(axis.text=element_text(size=14,face="bold"),
             axis.title=element_text(size=16,face="bold")
  )
  
  print(g)}

#ggplot Ic~teplota - neni poresenej nahodnej efekt hnizda!!!

{  
  x=lm(tib/(tib+tig)~stmean,data=mink)
  g<-ggplot(data=mink, aes(x=stmean, y=tib/(tib+tig)),fill="blue")
  g<-g + geom_point(shape=2,size=4,fill="blue") +   
    geom_smooth(method=loess, span=1,se=T,alpha=0.5)
  
  g<-g + labs(x="day incubation constancy",
              y="mean temperature")
  
  # g<-g+coord_cartesian(xlim=c(-2.55,2),ylim= c(-0.01,0.6))
  
  g<-g+theme(axis.text=element_text(size=14,face="bold"),
             axis.title=element_text(size=16,face="bold")
  )
  
  print(g)}

#ggplot Ic~srazky - neni poresenej nahodnej efekt hnizda!!!

{  
  g<-ggplot(data=mink, aes(x=fprecip, y=tib/(tib+tig)),fill="blue")
  g<-g + geom_point(shape=2,size=4,fill="blue") +   
    geom_smooth(method=lm, span=1,se=T,alpha=0.5)
  
  g<-g + labs(x="day incubation constancy",
              y="precipitation")
  
  # g<-g+coord_cartesian(xlim=c(-2.55,2),ylim= c(-0.01,0.6))
  
  g<-g+theme(axis.text=element_text(size=14,face="bold"),
             axis.title=element_text(size=16,face="bold")
  )
  
  print(g)}

#ggplot Iam~inc per - neni poresenej nahodnej efekt hnizda!!!

{  
  g<-ggplot(data=mink, aes(x=day_inc_per, y=Iam/sum_celk),fill="blue")
  g<-g + geom_point(shape=2,size=4,fill="blue") +   
    geom_smooth(method=loess, span=1,se=T,alpha=0.5)
  
  g<-g + labs(x="male incubation attendance",
              y="incubation period")
  
  # g<-g+coord_cartesian(xlim=c(-2.55,2),ylim= c(-0.01,0.6))
  
  g<-g+theme(axis.text=element_text(size=14,face="bold"),
             axis.title=element_text(size=16,face="bold")
  )
  
  print(g)

  x=ddply(mink,c("day_inc_per"),summarise,iam=mean(Iam/sum_celk))
  g<-ggplot(data=x, aes(x=day_inc_per, y=iam),fill="blue")
  g<-g + geom_point(shape=2,size=4,fill="blue") +   
    geom_smooth(method=loess, span=1,se=T,alpha=0.5)
  
  g<-g + labs(x="incubation period",
              y="male incubation attendance")
  
  # g<-g+coord_cartesian(xlim=c(-2.55,2),ylim= c(-0.01,0.6))
  
  g<-g+theme(axis.text=element_text(size=14,face="bold"),
             axis.title=element_text(size=16,face="bold")
  )
  
  print(g)}

#ggplot Icm~teplota - neni poresenej nahodnej efekt hnizda!!!

{  
  g<-ggplot(data=mink, aes(x=te, y=Ibm/(Ibm+Igm)),fill="blue")
  g<-g + geom_point(shape=2,size=4,fill="blue") +   
    geom_smooth(method=lm, span=1,se=T,alpha=0.5)
  
  g<-g + labs(y="male incubation constancy",
              x="mean temperature")
  
  # g<-g+coord_cartesian(xlim=c(-2.55,2),ylim= c(-0.01,0.6))
  
  g<-g+theme(axis.text=element_text(size=14,face="bold"),
             axis.title=element_text(size=16,face="bold")
  )
  
  print(g)}

#ggplot Icf~srazky - neni poresenej nahodnej efekt hnizda!!!

{  
  g<-ggplot(data=mink, aes(x=fprecip, y=Ibf/(Ibf+Igf)),fill="blue")
  g<-g + geom_point(shape=2,size=4,fill="blue") +   
    geom_smooth(method=lm, span=1,se=T,alpha=0.5)
  
  g<-g + labs(y="female incubation constancy",
              x="preciitation")
  
  # g<-g+coord_cartesian(xlim=c(-2.55,2),ylim= c(-0.01,0.6))
  
  g<-g+theme(axis.text=element_text(size=14,face="bold"),
             axis.title=element_text(size=16,face="bold")
  )
  
  print(g)}



x=resid(Mmia,type="partial")
plot (Iam/(Iam+Iaf)~sForn,predict=Mmia,data=mink)
plot (Iam/(Iam+Iaf)~sdip,data=mink)
plot (Iam/(Iam+Iaf)~stmean,data=mink)
plot (siam~Ibm/(Ibm+Igm),data=mink)


###deskripce inc_rhythms
library(sqldf)
library(tcltk)
i<-inc
i<-sqldf("select* from i where sex is not 'U' ")
i$dts<-as.character(i$datetime_start)
x<-sqldf("select substr(dts,12,2) from i")
names(x)[1]<-paste("hrs")
i<-cbind(i,x$hrs)
names(i)[22]<-paste("hrs")
i$dte<-as.character(i$datetime_end)
x<-sqldf("select substr(dte,12,2) from i")
names(x)[1]<-paste("hre")
i<-cbind(i,x$hre)
names(i)[24]<-paste("hre")
names(i)[10]<-paste("bout")
i$hrs<-(as.numeric(i$hrs))-1
i$hre<-(as.numeric(i$hre))-1

a<-sqldf("select* from i where hrs=hre ")

b<-sqldf("select* from i where hrs=hre-1 ")
ba<-b
ba$time_start_new<-trunc(ba$time_start_new)+1
ba$diff<-(ba$time_end_new-ba$time_start_new)*60
bb<-b
bb$time_end_new<-trunc(bb$time_end_new)-(1/3600)
bb$diff<-(bb$time_end_new-bb$time_start_new) * 60

c<-sqldf("select* from i where hrs=hre-2 ")
ca<-c
ca$time_start_new<-trunc(ca$time_start_new)+2
ca$diff<-(ca$time_end_new-ca$time_start_new)*60
cb<-c
cb$time_start_new<-trunc(cb$time_start_new)+1
cb$time_end_new<-trunc(cb$time_end_new)-(1/3600)
cb$diff<-(cb$time_end_new-cb$time_start_new)*60
cc<-c
cc$time_end_new<-trunc(cc$time_end_new)-(1+(1/3600))
cc$diff<-(cc$time_end_new-cc$time_start_new)*60

d<-sqldf("select* from i where hrs=hre-3 ")
da<-d
da$time_start_new<-trunc(da$time_start_new)+3
da$diff<-(da$time_end_new-da$time_start_new)*60
db<-d
db$time_start_new<-trunc(db$time_start_new)+2
db$time_end_new<-trunc(db$time_end_new)-(1/3600)
db$diff<-(db$time_end_new-db$time_start_new)*60
dc<-d
dc$time_start_new<-trunc(dc$time_start_new)+1
dc$time_end_new<-trunc(dc$time_end_new)-(1+(1/3600))
dc$diff<-(dc$time_end_new-dc$time_start_new)*60
dd<-d
dd$time_end_new<-trunc(dd$time_end_new)-(2+(1/3600))
dd$diff<-(dd$time_end_new-dd$time_start_new)*60

e<-sqldf("select* from i where hrs=hre-4 ")
ea<-e
ea$time_start_new<-trunc(ea$time_start_new)+4
ea$diff<-(ea$time_end_new-ea$time_start_new)*60
eb<-e
eb$time_start_new<-trunc(eb$time_start_new)+3
eb$time_end_new<-trunc(eb$time_end_new)-(1/3600)
eb$diff<-(eb$time_end_new-eb$time_start_new)*60
ec<-e
ec$time_start_new<-trunc(ec$time_start_new)+2
ec$time_end_new<-trunc(ec$time_end_new)-(1+(1/3600))
ec$diff<-(ec$time_end_new-ec$time_start_new)*60
ed<-e
ed$time_start_new<-trunc(ed$time_start_new)+1
ed$time_end_new<-trunc(ed$time_end_new)-(2+(1/3600))
ed$diff<-(ed$time_end_new-ed$time_start_new)*60
ee<-e
ee$time_end_new<-trunc(ee$time_end_new)-(3+(1/3600))
ee$diff<-(ee$time_end_new-ee$time_start_new)*60


f<-sqldf("select* from i where hrs=hre-5 ")
fa<-f
fa$time_start_new<-trunc(fa$time_start_new)+5
fa$diff<-(fa$time_end_new-fa$time_start_new)*60
fb<-f
fb$time_start_new<-trunc(fb$time_start_new)+4
fb$time_end_new<-trunc(fb$time_end_new)-(1/3600)
fb$diff<-(fb$time_end_new-fb$time_start_new)*60
fc<-f
fc$time_start_new<-trunc(fc$time_start_new)+3
fc$time_end_new<-trunc(fc$time_end_new)-(1+(1/3600))
fc$diff<-(fc$time_end_new-fc$time_start_new)*60
fd<-f
fd$time_start_new<-trunc(fd$time_start_new)+2
fd$time_end_new<-trunc(fd$time_end_new)-(2+(1/3600))
fd$diff<-(fd$time_end_new-fd$time_start_new)*60
ff<-f
ff$time_start_new<-trunc(ff$time_start_new)+1
ff$time_end_new<-trunc(ff$time_end_new)-(3+(1/3600))
ff$diff<-(ff$time_end_new-ff$time_start_new)*60
fg<-f
fg$time_end_new<-trunc(fg$time_end_new)-(4+(1/3600))
fg$diff<-(fg$time_end_new-fg$time_start_new)*60


g<-sqldf("select* from i where hrs=hre-6 ")
ga<-g
ga$time_start_new<-trunc(ga$time_start_new)+6
ga$diff<-(ga$time_end_new-ga$time_start_new)*60
gb<-g
gb$time_start_new<-trunc(gb$time_start_new)+5
gb$time_end_new<-trunc(gb$time_end_new)-(1/3600)
gb$diff<-(gb$time_end_new-gb$time_start_new)*60
gc<-g
gc$time_start_new<-trunc(gc$time_start_new)+4
gc$time_end_new<-trunc(gc$time_end_new)-(1+(1/3600))
gc$diff<-(gc$time_end_new-gc$time_start_new)*60
gd<-g
gd$time_start_new<-trunc(gd$time_start_new)+3
gd$time_end_new<-trunc(gd$time_end_new)-(2+(1/3600))
gd$diff<-(gd$time_end_new-gd$time_start_new)*60
ge<-g
ge$time_start_new<-trunc(ge$time_start_new)+2
ge$time_end_new<-trunc(ge$time_end_new)-(3+(1/3600))
ge$diff<-(ge$time_end_new-ge$time_start_new)*60
gf<-g
gf$time_start_new<-trunc(gf$time_start_new)+1
gf$time_end_new<-trunc(gf$time_end_new)-(4+(1/3600))
gf$diff<-(gf$time_end_new-gf$time_start_new)*60
gg<-g
gg$time_end_new<-trunc(gg$time_end_new)-(5+(1/3600))
gg$diff<-(gg$time_end_new-gg$time_start_new)*60

h<-sqldf("select* from i where hrs=hre-7 ")
ha<-h
ha$time_start_new<-trunc(ha$time_start_new)+7
ha$diff<-(ha$time_end_new-ha$time_start_new)*60
hb<-h
hb$time_start_new<-trunc(hb$time_start_new)+6
hb$time_end_new<-trunc(hb$time_end_new)-(1/3600)
hb$diff<-(hb$time_end_new-hb$time_start_new)*60
hc<-h
hc$time_start_new<-trunc(hc$time_start_new)+5
hc$time_end_new<-trunc(hc$time_end_new)-(1+(1/3600))
hc$diff<-(hc$time_end_new-hc$time_start_new)*60
hd<-h
hd$time_start_new<-trunc(hd$time_start_new)+4
hd$time_end_new<-trunc(hd$time_end_new)-(2+(1/3600))
hd$diff<-(hd$time_end_new-hd$time_start_new)*60
he<-h
he$time_start_new<-trunc(he$time_start_new)+3
he$time_end_new<-trunc(he$time_end_new)-(3+(1/3600))
he$diff<-(he$time_end_new-he$time_start_new)*60
hf<-h
hf$time_start_new<-trunc(hf$time_start_new)+2
hf$time_end_new<-trunc(hf$time_end_new)-(4+(1/3600))
hf$diff<-(hf$time_end_new-hf$time_start_new)*60
hg<-h
hg$time_start_new<-trunc(hg$time_start_new)+1
hg$time_end_new<-trunc(hg$time_end_new)-(5+(1/3600))
hg$diff<-(hg$time_end_new-hg$time_start_new)*60
hh<-h
hh$time_end_new<-trunc(hh$time_end_new)-(6+(1/3600))
hh$diff<-(hh$time_end_new-hh$time_start_new)*60

#selecty
a$diff<-a$bout_min
a<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from a")
ba<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from ba")
bb<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from bb")
ca<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from ca")
cb<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from cb")
cc<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from cc")
da<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from da")
db<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from db")
dc<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from dc")
dd<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from dd")
ea<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from ea")
eb<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from eb")
ec<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from ec")
ed<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from ed")
ee<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from ee")
fa<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from fa")
fb<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from fb")
fc<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from fc")
fd<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from fd")
ff<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from ff")
fg<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from fg")
ga<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from ga")
gb<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from gb")
gc<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from gc")
gd<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from gd")
ge<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from ge")
gf<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from gf")
gg<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from gg")
ha<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from ha")
hb<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from hb")
hc<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from hc")
hd<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from hd")
he<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from he")
hf<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from hf")
hg<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from hg")
hh<-sqldf("select ID_Nest,bout_id,date_start,bout,Type,left_type,sex,time_start_new,time_end_new,diff from hh")

#rbind
ih<-rbind(a,ba,bb,ca,cb,cc,da,db,dc,dd,ea,eb,ec,ed,ee,fa,fb,fc,fd,ff,fg,ga,gb,gc,gd,ge,gf,gg,ha,hb,hc,hd,he,hf,hg,hh)
ih<-sqldf("select* from ih order by ID_Nest,bout_id,time_start_new")

#nulla
x<-sqldf("select case when time_start_new <10 then substr(time_start_new,1,1) else substr(time_start_new,1,2)end from ih ")
names(x)[1]<-paste("hrs")
ih<-cbind(ih,x$hrs)
names(ih)[11]<-paste("hrs")


hink<-ddply(ih,c("hrs","sex"),summarize,sum=sum(diff))
hink$hour<-as.numeric(levels(hink$hrs))[hink$hrs]
hink$ord<-ifelse(hink$sex=='M',1,2)

#barplot proporce IaM v zavislosti na hodine
z<-ggplot(data=hink,aes(x=hour,y=sum,fill=sex,order=ord))

z<-z+geom_bar(stat="identity",position="fill")+scale_fill_manual(values=c("#FF0000","#0000FF"))
z<-z + labs(fill="Pohlav?",
            x="Hodina",
            y="% ?asu")

z<-z+scale_y_continuous(breaks=seq(0, 1, 0.1))
z<-z+scale_x_discrete(labels=c(seq(from=1,by=1,length.out=24)),limits=c(0:23))+coord_cartesian(xlim=c(-0.5,23.5),ylim= c(0,1))

z<-z+theme(axis.text=element_text(size=14,face="bold"),
           axis.title=element_text(size=16,face="bold"),
           legend.text=element_text(size=14),
           legend.title=element_text(size=16,face="bold"))

print(z)


#'Total Incubation GRaphs...:-)
tigr<-ddply(ih,c("hrs","sex","Type"),summarize,sum=sum(diff))
tigr$hour<-as.numeric(levels(tigr$hrs))[tigr$hrs]#nevim k cemu???
tigr$state<-ifelse(tigr$sex=='M',
                   ifelse(tigr$Type=='gap','IgM','IcM'),
                   ifelse(tigr$Type=='gap','IgF','IcF')
)
tigr$order<-ifelse(tigr$sex=='M',
                   ifelse(tigr$Type=='gap','2','1'),
                   ifelse(tigr$Type=='gap','4','3')
)


#graf s sex specific inc constancy vs. time

z<-ggplot(data=tigr,aes(x=hour,y=sum,fill=state,order=order))

z<-z+geom_bar(stat="identity",position="fill")+scale_fill_manual(values=c("#FF0000","#0000FF","#FF9999","#99CCFF"))
z<-z + labs(fill="Stav",
            x="Hodina",
            y="% ?asu")

z<-z+scale_y_continuous(breaks=seq(0, 1, 0.1))
z<-z+scale_x_discrete(labels=c(seq(from=1,by=1,length.out=24)),limits=c(0:23))
z<-z+coord_cartesian(xlim=c(-0.5,23.5),ylim= c(0:1))

z<-z+theme(axis.text=element_text(size=14,face="bold"),
           axis.title=element_text(size=16,face="bold"),
           legend.text=element_text(size=14),
           legend.title=element_text(size=16,face="bold"))

print(z)

# celkova Ic
tigr$ic<-ifelse(tigr$Type=='gap','p?est?vka','inkubace')
tigr$ordic<-ifelse(tigr$Type=='gap','2','1')

z<-ggplot(data=tigr,aes(x=hour,y=sum,fill=ic,order=ordic))

z<-z+geom_bar(stat="identity",position="fill")+scale_fill_manual(values=c("#CC6666","#CCCCCC"))
z<-z + labs(fill="Stav",
            x="Hodina",
            y="% ?asu")

z<-z+scale_y_continuous(breaks=seq(0, 1, 0.1))
z<-z+scale_x_discrete(labels=c(seq(from=1,by=1,length.out=24)),limits=c(0:23))
z<-z+coord_cartesian(xlim=c(-0.5,23.5),ylim= c(0:1))

z<-z+theme(axis.text=element_text(size=14,face="bold"),
           axis.title=element_text(size=16,face="bold"),
           legend.text=element_text(size=14),
           legend.title=element_text(size=16,face="bold"))

print(z)


#vytvoreni promennych se sumarnimi 
ttigr<-as.data.frame(c(0:23))
names(ttigr)[1]<-paste("hour")

x1<-sqldf("select sum from (select hour from ttigr)t
          left join (select sum, hour from tigr where sex ='M' and Type= 'gap')x
          on t.hour=x.hour
          ")
ttigr<-cbind(ttigr,x1)
names(ttigr)[2]<-paste("mig")

x1<-sqldf("select sum from (select hour from ttigr)t
          left join (select sum, hour from tigr where sex ='F' and Type= 'gap')x
          on t.hour=x.hour
          ")
ttigr<-cbind(ttigr,x1)
names(ttigr)[3]<-paste("fig")

x1<-sqldf("select sum from (select hour from ttigr)t
          left join (select sum, hour from tigr where sex ='M' and Type= 'inc')x
          on t.hour=x.hour
          ")
ttigr<-cbind(ttigr,x1)
names(ttigr)[4]<-paste("mib")

x1<-sqldf("select sum from (select hour from ttigr)t
          left join (select sum, hour from tigr where sex ='F' and Type= 'inc')x
          on t.hour=x.hour
          ")
ttigr<-cbind(ttigr,x1)
names(ttigr)[5]<-paste("fib")

ttigr[is.na(ttigr)] <- 0 

ttigr$tic<-ttigr$mib+ttigr$fib
ttigr$tig<-ttigr$mig+ttigr$fig
ttigr$sample<-ttigr$tic+ttigr$tig

#celkove shrnuti inkubace
sqldf("select sum(bout_min) from i where sex is 'M'")#=26671
sqldf("select sum(bout_min) from i where sex is 'F'")#161223#je treba pa jeste vzloucit prvni a posledni gapy!!!
26671+161223#=187894
26671/187223#=0.142

### shrnuti inkubace pro paper
sqldf("select sum(bout_min) from i where sex is 'M' 
      and bout_id>1 
      and pk not in
      (select pk from (select id_nest,pk,max(bout_id) from i group by ID_Nest))
      and id_nest not in 
      (select id_nest from i where id_nest = 35 or id_nest = 142 or id_nest = 161 or id_nest = 165 group by id_nest)
      and id_nest in 
      (select id_nest from mink group  by id_nest)
      
      ")#25699.32   # jen z dat pouzitych pro paper, odfiltrovany prvni a posledni a hnizda co nejdou do paperu

sqldf("select sum(bout_min) from i where sex is 'F' 
      and bout_id>1 
      and pk not in
      (select pk from (select id_nest,pk,max(bout_id) from i group by ID_Nest))
      and id_nest not in 
      (select id_nest from i where id_nest = 35 or id_nest = 142 or id_nest = 161 or id_nest = 165 group by id_nest)
      and id_nest in 
      (select id_nest from mink group  by id_nest)
      
      ")#141606.9   # jen z dat pouzitych pro paper, odfiltrovany prvni a posledni a hnizda co nejdou do paperu, zahrnuje ale i noc

ih$pk=seq(1,nrow(ih),1)# asi nepotrebuju...
ih$hrs=as.integer(floor(ih$time_start_new))
names(mink)[2]=paste("mean_vol2")
names(mink)[39]=paste("mean_vol3")

### total incubation care
sqldf("select sum(diff) from ih where sex is 'F' 
      and bout_id>1
      and bout_id not in
      (select pk from (select id_nest,pk,bout_id,max(bout_id) from ih group by ID_Nest))
      and id_nest not in 
      (select id_nest from ih where id_nest = 35 or id_nest = 142 or id_nest = 161 or id_nest = 165 group by id_nest)
      and id_nest in 
      (select id_nest from mink group  by id_nest)
      and hrs >=5 and hrs<=20
      
      ")#88861.98   # jen z dat pouzitych pro paper, odfiltrovany prvni a posledni a hnizda co nejdou do paperu, bez noci

sqldf("select sum(diff) from ih where sex is 'M' 
      and bout_id>1
      and bout_id not in
      (select pk from (select id_nest,pk,bout_id,max(bout_id) from ih group by ID_Nest))
      and id_nest not in 
      (select id_nest from ih where id_nest = 35 or id_nest = 142 or id_nest = 161 or id_nest = 165 group by id_nest)
      and id_nest in 
      (select id_nest from mink group  by id_nest)
      ")# 25739.78  # jen z dat pouzitych pro paper, odfiltrovany prvni a posledni a hnizda co nejdou do paperu, bez noci
25739.78/(88861.98+25739.78)#0.2246

sqldf("select * from ih where sex is 'M' 
      and bout_id>1
      and bout_id not in
      (select pk from (select id_nest,pk,bout_id,max(bout_id) from ih group by ID_Nest))
      and id_nest not in 
      (select id_nest from ih where id_nest = 35 or id_nest = 142 or id_nest = 161 or id_nest = 165 group by id_nest)
      and id_nest in 
      (select id_nest from mink group  by id_nest)
      and hrs <5 or sex is 'M'  and hrs>20
      
      ")


### incubation constancy



i$id_day<-as.factor(paste(i$ID_Nest,i$day_inc_per,sep=' '))#199 hnizdodni
i$ID_Nest<-as.factor(i$ID_Nest)#52 hnizd

#prop MIa jako prumer pres hodiny(vyvazeni nerovnomernosti vzorku)
x<-(ttigr$mig+ttigr$mib)/((ttigr$fig+ttigr$fib)+(ttigr$mig+ttigr$mib))
mean(x)#0.139

#prop Ic jako prumer pres hodiny
x<-(ttigr$mib+ttigr$fib)/(ttigr$sample)
mean(x)#0.814
ttigr$Ic<-ttigr$tic/ttigr$sample
sd(ttigr$Ic)

#rozpeti Mink mezi hnizdama
x<-ddply(mink,c("ID_Nest"),summarize,n_days=length(date_start),mia=sum(Iam),mic=sum(Ibm),fia=sum(Iaf),fic=sum(Ibf),sampl=sum(sum_celk))
x$mprop<-x$mia/x$sampl#jen denni inkubace
x$mpropcelk<-x$mprop/(24/15.5)
x$mc<-x$mic/x$sampl
x$mccelk<-x$mc/(24/15.5)
x$fprop<-x$fia/x$samp
x$fpropcelk<-x$fprop/(24/15.5)
x$fc<-x$fic/x$sampl
x$fccelk<-x$fc/(24/15.5)

x<-sqldf("select * from x order by mprop desc")
x$order<-c(1:47)

min(x$mpropcelk)#0.01085
max(x$mpropcelk)#0.4409
mean(x$mpropcelk)#0.1698
median(x$mpropcelk)#0.1658
sd(x$mpropcelk)#0.124
sqldf("select (sum(mib+mig)/26671) from ttigr where mib>1000")

##### pro paper jen hnizda s male inc. a jen pro den
x=subset(x,mpropcelk>0)
min(x$mprop)#0.0168
max(x$mprop)#0.682
mean(x$mprop)#0.2629
median(x$mprop)#0.2568
sd(x$mprop)#0.192
sqldf("select (sum(mib+mig)/25739.78) from ttigr where mib>1000")

##### female ic
min(x$fic/x$fia)#0.61844
max(x$fic/x$fia)#0.9121
mean(x$fic/x$fia)#0.7867
median(x$fic/x$fia)#0.7722
sd(x$fic/x$fia)#0.0758


##### prop paper jen hnizda s male ic. a jen pro den
x=subset(x,mpropcelk>0)
min(x$mic/x$mia)#0.03989
max(x$mic/x$mia)#0.9534
mean(x$mic/x$mia)#0.7345
median(x$mic/x$mia)#0.8008
sd(x$mic/x$mia)#0.2062

##### female ic
min(x$fic/x$fia)#0.61844
max(x$fic/x$fia)#0.9121
mean(x$fic/x$fia)#0.7867
median(x$fic/x$fia)#0.7722
sd(x$fic/x$fia)#0.0758


##### total ic
#x=subset(x,mpropcelk>0)
sum(x$sampl)#103918.8
((sum(x$mic) + sum(x$fic))/(sum(x$mia)+sum(x$fia)))#0.7839

min((x$mic + x$fic)/(x$mia+x$fia))#0.5992
max((x$mic + x$fic)/(x$mia+x$fia))#0.9185
mean((x$mic + x$fic)/(x$mia+x$fia))#0.7805
median((x$mic + x$fic)/(x$mia+x$fia))#0.7756
sd((x$mic + x$fic)/(x$mia+x$fia))#0.078

hist((x$mic + x$fic)/(x$mia+x$fia))

z<-ggplot(data=x,aes(x=order,y=mpropcelk,order=order))

z<-z+geom_bar(stat="identity",color="black",fill="#99CCFF")
z<-z+geom_bar(aes(x=order,y=mccelk),stat="identity",color="black",fill="blue")
z<-z + labs(x="??slo hn?zda",
            y=" % inkubace zast?van? samcem")

z<-z+scale_y_continuous(breaks=seq(0, 1, 0.1))
z<-z+scale_x_discrete(labels=x$ID_Nest,limits=c(1:47))
z<-z+coord_cartesian(ylim= c(0:1),xlim=c(0.5:48))

z<-z+theme(axis.text=element_text(size=14,face="bold"),
           axis.title=element_text(size=16,face="bold"),
           axis.text.x  = element_text(angle=75, vjust=0.5, size=16))
print(z)

# v Aj
z<-ggplot(data=x,aes(x=order,y=mpropcelk,order=order))

z<-z+theme(panel.background = element_rect(fill = "gray24"))

z<-z+geom_bar(stat="identity",color="black",fill="#E69F00")
z<-z + labs(x="Nest",
            y=" Male incubation constancy")

z<-z+scale_y_continuous(breaks=seq(0, 1, 0.1))
z<-z+scale_x_discrete(labels=x$ID_Nest,limits=c(1:47))
z<-z+coord_cartesian(ylim= c(0:1),xlim=c(0.5:48))

z<-z+theme(axis.text=element_text(size=14,face="bold"),
           axis.title=element_text(size=30,face="bold"),
           axis.text.x  = element_text(angle=75, vjust=0.5, size=16))
print(z)


# prumerny pocet ink udalosti
i$bout<-as.factor(i$bout)
x<-ddply(i,c("bout"),summarize,length=sum(bout_min),count=length(Type))
x<-sqldf("select * from 
         (select sex,bout,id_day,id_nest from i)i
         join (select * from x)x 
         on i.bout=x.bout
         ")
x[5]<-NULL
x<-sqldf("select distinct * from x")#zakladni x pro dalsi usek


#rozdeleni ink.epizod samcu
q<-sqldf("select * from x where sex is 'M'")#n=500
hist(q$length)
min(q$length)#0.87
max(q$length)#421.16
mean(q$length)#53.98
median(q$length)#32
sd(q$length)#61.48

#rozdeleni ink.epizod samic
q<-sqldf("select * from x where sex is 'F'")#n=500
hist(q$length)
min(q$length)#2.37
max(q$length)#7104.733
mean(q$length)#604.74
median(q$length)#157.375
sd(q$length)#1060.61

#pocet inkubacnich epizod /den

q<-ddply(x,c("id_day"),summarize,exchange=length(sex))
max(q$exchange)#25
mean(q$exchange)#~6 (5.95)
median(q$exchange)#5
sd(q$exchange)#5.44

#deskripce delky inkubacnich boutu
x<-sqldf("select sex,bout_id,bout_min,time_start_new from i where Type is 'inc'")

#inc bouty pro M
q<-sqldf("select * from x where sex is 'M'")
hist(q$bout_min)
min(q$bout_min)#0.01
max(q$bout_min)#236.75
mean(q$bout_min)#24.62
median(q$bout_min)#16.68
sd(q$bout_min)#27.08

#inc bouty pro F
qq<-sqldf("select * from x where sex is 'F'")
hist(qq$bout_min)
min(qq$bout_min)#0.08
max(qq$bout_min)#452.33
mean(qq$bout_min)#43.66
median(qq$bout_min)#25.18
sd(qq$bout_min)#56.63

#zavislost delky na zacatku# bude potreba sloucit prespulnocni bouty
plot(q$bout_min~q$time_start_new)

#test delky boutu
shapiro.test(x$bout_min)
f<-sqldf("select bout_min from x where sex is 'F'")
m<-sqldf("select bout_min from x where sex is 'M'")
wilcox.test(f$bout_min,m$bout_min)

#exchange gapy
x<-ddply(i,c("bout"),summarize,bs=min(bout_id))
q<-paste(x$bout,x$bs,sep=' ')
x<-cbind(x,q)
i$q<-paste(i$bout,i$bout_id,sep=' ')

z<-sqldf("select sex,q,Type,ID_Nest,day_inc_per,bout_min from i where q in (select q from x)")
z<-sqldf("select distinct * from z")

x<-sqldf("select * from x
         join (select * from z) z 
         on z.q=x.q
         ")
x[5]<-NULL

z<-ggplot(data=x, aes(x = sex,y=bout_min))


z<-z+geom_boxplot()

z<-z+scale_y_continuous(breaks=seq(0,50, 10))
z<-z+coord_cartesian(ylim= c(0,20))


z<-z + labs(x="pohlav?",
            y="d?lka p?est?vky p?i st??d?n? partnera")

z<-z+theme(axis.text=element_text(size=14,face="bold"),
           axis.title=element_text(size=16,face="bold"))

print(z)

mean(x$bout_min)
sd(x$bout_min)

#test rozdilu pohlavi pri exch_gapu
f<-sqldf("select bout_min from x where sex is 'F'")
mean(f$bout_min)
sd(f$bout_min)
m<-sqldf("select bout_min from x where sex is 'M'")
mean(m$bout_min)
sd(m$bout_min)

wilcox.test(f$bout_min,m$bout_min)#5.855e-10

#model
m<-glm(bout_min~sex*day_inc_per,data=x)#gamma?
shapiro.test(resid(m))#v prdeli

plot(bout_min~day_inc_per,data=x)
f<-sqldf("select bout_min,day_inc_per from x where sex is 'F'")
m<-sqldf("select bout_min,day_inc_per from x where sex is 'M'")

plot(bout_min~day_inc_per,data=f)
plot(bout_min~day_inc_per,data=m)
abline(m,col="blue",lwd=2)

z<-ggplot(data=x, aes(x = day_inc_per,y=bout_min))#nic moc tam nevidim, mozna tazeny extremnima hodnotama, jsou tam i useky tesne po instalaci


z<-z+geom_point(position=position_jitter(width=0.1,height=0.1),size=1)

z<-z+scale_y_continuous(breaks=seq(0,50, 10))
z<-z+coord_cartesian(ylim= c(0,20))

z<-z+geom_smooth(se=T,alpha=0.5)
z<-z + labs(x="obdob? v r?mci inkuba?n? periody",
            y="d?lka p?est?vky p?i st??d?n? partnera")

z<-z+theme(axis.text=element_text(size=14,face="bold"),
           axis.title=element_text(size=16,face="bold"))

print(z)

#graf korelace srazek mezi stanicemi
x<-ddply(klima_hod, c("date"),summarize,tem=sum(srazky_Temelin),cb=sum(srazky_CB))
plot(tem~cb,data=x)

z<-ggplot(data=x,aes(x=tem,y=cb))

z<-z+geom_point(position=position_jitter(width=0.1,height=0.1),size=3)

z<-z + labs(x="Denn? sr??ky Temel?n (mm)",
            y="Denn? sr??ky ?esk? Bud?jovice (mm)")

z<-z+theme(axis.text=element_text(size=14,face="bold"),
           axis.title=element_text(size=16,face="bold"))

print(z)

###

















#parovaci statusy
psik<-sqldf("select * from foks
            join
            (select ring,ms from rozmery)r
            on foks.ring=r.ring
            ")
psik[25]<-NULL
psik<-sqldf("select * from psik where  ms is 'p1' or ms is 'p2' or ms is 'm'")
zaloha_psik<-psik

m<-lm(pc1.pelichani~ms,data=psik)
m<-lm(pc1.ornamenty~ms,data=psik)

pca.psikk<-prcomp(~pc1.ornamenty+pc1.pelichani+weight+wing+tail+crest, data=psik, center=T, scale.=T)
bprint(pca.psik)
summary(pca.psik)
plot(pca.psik, type="lines")
y<-cbind(psik$id_bird,psik$ms)
biplot(pca.psik)
c<- t(pca.psik$rotation)*pca.psik$sdev
pc1.psik<-pca.psik$x[1:41]

x<-ggbiplot(pca.psikk)

a<-ggbiplot(pca.psik, obs.scale = 1, var.scale = 1)

x <- x + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(a)

#shrnuti vlastnosti pro mating stat
x<-ddply(psik,c("ms"),summarize,breast=mean(breast), crown_moult=mean(crown_moult), face=mean(face),wstripe=mean(wstripe), bstripe=mean(bstripe),
         face_moult=mean(face_moult), tail=mean(tail), crest=mean(crest), wing=mean(wing),weight=mean(weight),pcorn=mean(pc1.ornamenty),
         pcpel=mean(pc1.pelichani), timing=mean(timing),inv=mean(inv))
xz<-ddply(foks,c("clutch"),summarize,breast=mean(breast), crown_moult=mean(crown_moult), face=mean(face),wstripe=mean(wstripe), bstripe=mean(bstripe),
          face_moult=mean(face_moult), tail=mean(tail), crest=mean(crest), wing=mean(wing),weight=mean(weight),pcorn=mean(pc1.ornamenty),
          pcpel=mean(pc1.pelichani), timing=mean(timing),inv=mean(inv))
names(xz)[1]<-paste("ms")
x<-rbind(x,xz)

#priprava fokse pro vztoreni ordinacnih prostoru pro promitnuti ms
foks$pc1_ornamenty<-foks$pc1.ornamenty
foks$pc1_pelichani<-foks$pc1.pelichani

sqldf("select f.id_bird,pc1_ornamenty,pc1_pelichani,weight,wing,tail,crest,ms
      from (select id_bird,pc1_ornamenty,pc1_pelichani,weight,wing,tail,crest from foks)f
      left join (select id_bird,ms from psik)p
      on f.id_bird=p.id_bird
      ")

#### synchronizace M pri inkubaci
# subset pro pokus

inc_sync=subset(inc,ID_Nest>30&ID_Nest<40)#vybrany hnizda z zabin

unique(inc$ID_Nest)
x=as.POSIXct(c(min(inc$datetime_start),max(inc$datetime_end)))# rozpeti pro ktery je aspon jedno z hnizd
x=cut.POSIXt(x,breaks = "min")#nastrihani po minutach

a=as.vector(levels(x))#prevedeni vznikleho faktoru na vector a posleze df
a=as.data.frame(a)
names(a)[1]=paste("datetime")
a$datetime=as.POSIXct(strptime(a$datetime, format="%Y-%m-%d %H:%M:%S"))
a$pk_time=seq(1,nrow(a),1)


inc_sync2=NULL
inc_sync2=as.data.frame(inc_sync2)

c=NULL
c=as.data.frame (c)

for(i in 1:nrow(a)) {     
  n=subset(a,a$pk_time==i)
    b=subset(inc_sync,n$datetime>=inc_sync$datetime_start 
             & n$datetime<= inc_sync$datetime_end)
    b$pk_time=n$pk_time
    b$datetime=n$datetime
    inc_sync2=rbind(inc_sync2,b);
}#prirazeni vsech boutu k dane minute za predpokladu, ze v kazdem case je dostupny nejaky video

######## nechat jet pres noc
for(i in 1:nrow(a)) {     
  n=subset(a,a$pk_time==i)
  b=subset(inc,n$datetime>=inc$datetime_start 
           & n$datetime<= inc$datetime_end)
  if (nrow(b)>0) {
  b$pk_time=n$pk_time
  b$datetime=n$datetime
  }
  inc_sync2=rbind(inc_sync2,b);
}#verze pro deravej datovej ramec


#for(i in unique(inc_sync2$ID_Nest)){
 # x=subset(inc_sync2,inc_sync2$ID_Nest==i
  #         &sex=="M")
  #b=as.vector(x$pk_time)
  #a=paste("minc", i, sep = "_")
  #assign(a,b)
#}#pro kazdy hnizdo zvlastni df s subsetem minut, kdy inkuboval samec - asi zbytecny

#x=ls(pattern='minc_*')
#n=length(x)
#intersects = matrix(nrow=n, ncol=n)
#colnames(intersects) = ls(pattern='minc_*')
#rownames(intersects) = ls(pattern='minc_*')
#intersects=as.matrix(length(intersect(intersects)))

intersects=as.data.frame(NULL)

for(i in unique(inc_sync2$ID_Nest)) {
  for(j in unique(inc_sync2$ID_Nest)) {
    a=subset(inc_sync2,ID_Nest==i&sex=="M")
    b=subset(inc_sync2,ID_Nest==j&sex=="M")
    c=as.vector(a$pk_time)
    d=as.vector(b$pk_time)
    e=i
    f=j
    g=length(c)
    k=length(d)
    l=(length(intersect(c,d)))
    x=cbind(e,f,g,k,l)
    intersects=rbind(intersects,x)
    ;
  };
  }
names(intersects)=paste(c("first_nest","second_nest","minc_first","minc_second","intersect"))
intersects$pk=seq(1,nrow(intersects),1)

#jeste nutno doresit celej intersect videi!
#pridani pairwise vzdalenosti pro hnizda

#prirazeni souradnic z coor do intersects

x=as.data.frame(NULL)

for (i in 1:nrow(intersects)) {
  d=subset(intersects,intersects$pk==i)
  a=subset(coor,d$first_nest==coor$id_nest)
  b=subset(coor,d$second_nest==coor$id_nest)
  c=cbind(a$nlat,a$nlong,b$nlat,b$nlong)
  x=rbind(x,c)
}

intersects= cbind(intersects,x)
names(intersects)[7:10]=paste(c("fn_lat","fn_lon","sn_lat","sn_lon"))

intersects$distance=as.integer(1000*(sqrt(((abs(intersects$fn_lat-intersects$sn_lat))^2)+
                           ((abs(intersects$fn_lon-intersects$sn_lon))^2))))#vzdalenost mezi dvojicemi hnizd

polygam=subset(intersects,intersects$distance<100
               &intersects$distance>0
               &intersects$intersect==0)


# subset potencialnich polygamnich paru
#filtr na vzdalenost, prekryv, ale taky celkovy mnozstvi inkubaci pece?

