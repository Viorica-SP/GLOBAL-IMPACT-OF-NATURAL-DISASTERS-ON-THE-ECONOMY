
####Încărcarea librăriilor folosite####

library(readxl)
library(dplyr)
library(plotly)
library(forecast)
library(ggplot2)
library(readxl)
library(TSstudio)
library(lmtest)
library(Metrics)
library(uroot)
library(urca)
library(dplyr)
library(seasonal)
library(readxl)
library(hrbrthemes)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(extrafont)
library(readr)
library(plotly)
library(gganimate)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(esquisse)
library(RColorBrewer)
library(dygraphs)
library(xts)          
library(tidyverse)
library(lubridate)
library(readxl)
library(ggthemes)
library(extrafont)
library(readr)
library(showtext)
library(fpp2)
library(vars)
library(tseries)
library(urca)
library(stats)
library(changepoint)
library(dplyr)
library(uroot)
library(TSA)
library(readxl)
library(gt)
font_add_google("Rubik")
showtext_auto()
options(scipen = 999)
library(modelsummary)
library(kableExtra)
library(writexl)
library(urca)
library(forecast)
library(vars)
library(tidyverse)
library(tsDyn)
library(dynlm)
library(aTSA)
library(stargazer)

######################################Încărcarea datelor################################################################################

Date1900_2021 <- read_excel("Date1900-2021.xlsx")
attach(Date1900_2021)
PIB19902017 <- read.csv("PIB.csv")

#####################################Crearea setului de date pentru reprezentarea pe hartă###############################################

numar_dezastre <- 
  Date1900_2021 %>%
  arrange(Continent,ISO,`Start Year`,`Start Month`)%>%
  group_by(`Start Year`,`Start Month`,Continent,ISO) %>%
  count(ISO) 
  

numar_dezastre<-
  numar_dezastre %>% 
  group_by(`Start Year`,`Start Month`,Continent,ISO) %>%
  summarize(summary_variable = sum(n))

Plot_harta = subset(numar_dezastre, select = -c(Continent,`Start Month`) )
  
#####################################Crearea setului de date pentru reprezentare -număr morti pe an ###############################################

numar_morti <- 
  Date1900_2021 %>%
  #select(ISO,Continent,`Start Year`,`Start Month`, `Total Deaths`) %>%
  arrange(Continent,ISO,`Start Year`,`Start Month`,`Total Deaths`)%>%
  group_by(`Start Year`) %>%
  summarise(Număr_morți= sum(na.omit(`Total Deaths`)))


p <- (ggplot(numar_morti, aes(x = `Start Year`, y =Număr_morți ,colour="#16677C")) +
        geom_line() +
        ylab("Număr decese")+
        theme_fivethirtyeight()+
        theme(axis.title = element_text())+
        xlab("An") +
        theme (legend.title = element_blank ())+
        theme (legend.position = "none")+
        scale_color_manual(values=c("#16677C"))
        )

ggplotly(p)



#####################################Crearea setului de date pentru reprezentare - daune bănesti ###############################################

daune <- 
  Date1900_2021 %>%
  #select(ISO,Continent,`Start Year`,`Start Month`, `Total Deaths`) %>%
  arrange(Continent,ISO,`Start Year`,`Start Month`,`Total Damages, Adjusted ('000 US$)`)%>%
  group_by(`Start Year`) %>%
  summarise(Daune= sum(na.omit(`Total Damages, Adjusted ('000 US$)`)))


p <- (ggplot(daune, aes(x = `Start Year`, y = Daune,colour="#16677C")) +
        geom_line() +
        ylab("Daune provocate de dezastre naturale ($)")+
        theme_fivethirtyeight()+
        theme(axis.title = element_text())+
        xlab("An") +
        theme (legend.title = element_blank ())+
        theme (legend.position = "none")+
        scale_color_manual(values=c("#16677C"))
        )

ggplotly(p)




#####################################Crearea setului de date pentru dezastre după tipul de dezastru ###############################################

numar_dezastre <- 
  Date1900_2021 %>%
  #select(ISO,Continent,`Start Year`,`Start Month`, `Total Deaths`) %>%
  arrange(Continent,ISO,`Start Year`,`Start Month`,`Total Damages, Adjusted ('000 US$)`)%>%
  group_by(`Start Year`,`Disaster Subgroup`) %>%
  summarise(Daune= sum(na.omit(`Total Damages, Adjusted ('000 US$)`)))

#####################################Crearea setului de date pentru rația PIB-DAUNE ###############################################

DaunePIB<- ts(PIB19902017$Disaster.losses.as...of.global.GDP, start=1990, frequency = 1)
attach(PIB19902017)

par(mfrow=c(3,2), mar=c(2,2,2,2))
p <- (ggplot(PIB19902017, aes(x = Year, y = Disaster.losses.as...of.global.GDP,fill="#16677C")) +
      geom_bar(stat = "identity") +
      ylab("Daune ca procent din PIB (%)")+
      theme_fivethirtyeight()+
      theme(axis.title = element_text())+
      xlab("An") +
      theme (legend.title = element_blank ())+
      theme (legend.position = "none")+
      scale_fill_manual(values = c("#16677C"))+
      scale_x_continuous("An",breaks=seq(min(Year),max(Year), by=3), expand = c(0,0))
     )

ggplotly(p)
  
###########Reprezentarea grafică de tip hartă pentru statele ce au avut dezastre naturale pe an (1990-2021)###################################
  

l <- list(color ="grey", width = 0.5)

harta <- plot_geo(Plot_harta,
                  frame=~`Start Year`)

harta <- harta %>% 
  add_trace( z=~summary_variable,
             zmin=0,
             zmax=max(Plot_harta$summary_variable),
             color=~summary_variable,
             locations=~ISO,
             colorscale="Viridis")%>%
  layout(geo=list())
  harta <- harta %>% layout(theme_minimal())
  harta

##############Reprezentarea grafică de tip hartă pentru statele ce au avut dezastre naturale cumulate din anii 1990-2021####################
numar_dezastre <- 
  Date1900_2021 %>%
  #select( ISO,`Dis No`,Continent) %>%
  arrange(Continent,ISO)%>%
  group_by(Continent,ISO) %>%
  count(`ISO`) 
  
harta <- plot_geo(numar_dezastre)
harta <- harta %>% 
  add_trace( z=~n,
             zmin=0,
             zmax=max(numar_dezastre$n),
             color=~n,
             locations=~ISO,
             colors = c("#84eecd", "#69d8c4","#51c2ba","#3cacaf","#2b96a1","#1e8192","#176c81","#13586f"))
harta

##########################################Tipul dezastrelor#####################################################


date <- data.frame(
  grup=c("Biologic","Climatic","Extraterestru","Geofizic",'Hidrologic',"Meteorologhic"),
  valoare=c(1601,1252,1,1864,6385,5134)
)

p <- ggplot(date, aes(x="", y=valoare, fill=grup)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_fivethirtyeight()
p


########################################################## Analiză China #################################################


  Preț_închidere_HSI <- read.csv("CHINA.csv")

  numar_dezastre <- 
    Date1900_2021 %>%
    #select(ISO,Continent,`Start Year`,`Start Month`) %>%
    arrange(`Start Year`,`Start Month`)%>%
    filter(Country=="China") %>%
    group_by(`Start Year`,`Start Month`) %>%
    count(ISO) 

  numar_dezastre<-
    numar_dezastre %>% 
    group_by(`Start Year`,`Start Month`) %>%
    summarize(summary_variable = sum(na.omit(n)))

  #write_xlsx(numar_dezastre, 'DezastreChina.xlsx')

  Număr_dezastre_naturale_în_China <- read_excel("DezastreChina.xlsx")
  
  dset <- data.frame(Preț_închidere_HSI,Număr_dezastre_naturale_în_China)

  datasummary(All(dset)~(Media=mean)
                  +(`Deviația standard`= SD)+(Minim=min)+(Maxim=max)+(`25%`=P25)+(`50%`=P50)
                  +(`75%`=P75)+(Histograma=Histogram), data=dset, align = 'lcccccccc')


  Preț_închidere_HSI<- ts(Preț_închidere_HSI,start=1987, frequency = 4)
  
  p<-(  autoplot(Preț_închidere_HSI, colour = "#69b3a2") +
          labs(title = "",
               x = "An",
               y = "Preț închidere (dolar Hong Kong)") +
          theme_fivethirtyeight() +
          theme(axis.title = element_text())+
          scale_x_continuous("An",breaks=seq(min(start(Preț_închidere_HSI)),max(end(Preț_închidere_HSI)), by=3), expand = c(0,0))
  )
 ggplotly(p)
  

  Număr_dezastre_naturale_în_China <- ts(Număr_dezastre_naturale_în_China,start=1987, frequency = 4)
  
  p<-(  autoplot(Număr_dezastre_naturale_în_China, colour = "#69b3a2") +
          labs(title = "",
               x = "An",
               y = "Număr dezastre naturale") +
          theme_fivethirtyeight() +
          theme(axis.title = element_text())+
          scale_x_continuous("An",breaks=seq(min(start(Număr_dezastre_naturale_în_China)),max(end(Număr_dezastre_naturale_în_China)), by=3), expand = c(0,0))
        
  )
  ggplotly(p)
 
  

LPreț_închidere_HSI <- log(Preț_închidere_HSI)
LNumăr_dezastre_naturale_în_China <- log(Număr_dezastre_naturale_în_China)


# Crearea unui df cu toate cele 2 variabile
dset <- cbind(LPreț_închidere_HSI,LNumăr_dezastre_naturale_în_China)


 
#####Testarea stationaritatii seriilor pentru pretul de inchidere al indicelui HSI ####

# Trend si intercept

adf_t_i <- LPreț_închidere_HSI %>% ur.df(., type='trend', selectlags=c("AIC"))
summary(adf_t_i) #Serie nestationara

# Intercept

adf_i <- LPreț_închidere_HSI %>%ur.df(., type='drift', selectlags=c("AIC"))
summary(adf_i)  #Serie nestationara

# Elemente deterministe

adf_e <- LPreț_închidere_HSI %>% ur.df(., type="none", selectlags=c("AIC"))
summary(adf_e) #Serie nestationara


#### Testarea stationaritatii prin KPSS ####

LPreț_închidere_HSI %>% ur.kpss() %>% summary() # valoarea testului 2.4238   > toate valorile critice
#seria este nestationara

# Testarea stationaritatii prin Philips-Perron pentru seria nediferentiata
PP.test(LPreț_închidere_HSI) # p = 0.4859 serie nestaționară


  


#####Testarea stationaritatii seriilor pentru numarul de dezastre naturale - China ####

# Trend si intercept

adf_t_i <- LNumăr_dezastre_naturale_în_China %>%ur.df(., type='trend', selectlags=c("AIC"))
summary(adf_t_i) 

# Intercept

adf_i <- LNumăr_dezastre_naturale_în_China %>%ur.df(., type='drift', selectlags=c("AIC"))
summary(adf_i) 

# Elemente deterministe

adf_e <- LNumăr_dezastre_naturale_în_China %>%ur.df(., type="none", selectlags=c("AIC"))
summary(adf_e) 


#### Testarea stationaritatii prin KPSS pentru seria nediferentiata ####

LNumăr_dezastre_naturale_în_China %>% ur.kpss() %>% summary() # valoarea testului 2.4238  > toate valorile critice
# seria este nestationara

# Testarea stationaritatii prin Philips-Perron pentru seria nediferentiata
PP.test(LNumăr_dezastre_naturale_în_China) # p = 0.4 serie nestationara



#Pentru a confirma faptul că ambele serii sunt nestaționare am utilizat și funcția ndiffs()

ndiffs(LNumăr_dezastre_naturale_în_China)
ndiffs(LPreț_închidere_HSI)
#Seriile sunt nestationare si integrate de acelasi ordin si anume I(1).



##### Cointegrarea Johansen ####

# H0: nu exista cointegrare
# H1: exista cel putin o relatie de cointegrare

# Selectarea lagului 
lagselect <- VARselect(dset, lag.max = 8, type = 'const')
lagselect$selection 

# Testul Johansen 
ctest1 <- ca.jo(na.omit(dset), type = 'trace', ecdet = 'const',K=5)
summary(ctest1) 


# Testul Johansen
ctest2 <- ca.jo(dset, type = 'eigen', ecdet = 'const',K=5)
summary(ctest2)


model <- VAR(diff(dset), p = 5, type = 'both', season =NULL, exog = NULL,ic = c("AIC"))
summary(model)

stargazer(model[['varresult']], type = 'text')


# Autocorelarea
Serial <- serial.test(model, lags.pt = 12, type = 'PT.asymptotic')
Serial 

# Heteroscedasticitate
Arch <- vars::arch.test(model,lags.multi = 12,multivariate.only = TRUE)
Arch 

# Normalitatea reziduurilor
Norm <- normality.test(model, multivariate.only = TRUE)
Norm 


Stability <- stability(model,type = 'OLS-CUSUM')
plot(Stability)
dev.off()


# Cauzalitate Granger
Granger_dezastre <- causality(model, cause = "LNumăr_dezastre_naturale_în_China")
Granger_dezastre 

Granger_pret <- causality(model, cause = 'LPreț_închidere_HSI')
Granger_pret 


##### Functia de raspuns la impuls (IRF) ####
dezastre_irf <- irf(model, impulse = 'LPreț_închidere_HSI', response = 'LNumăr_dezastre_naturale_în_China', 
                      n.ahead = 20, boot = TRUE, ci=0.90) 
plot(dezastre_irf, ylab = 'Număr dezastre naturale', 
     main = 'Raspunsul dezastrelor naturale la socurile prețului')

pret_irf <- irf(model, impulse = 'LNumăr_dezastre_naturale_în_China', response = 'LPreț_închidere_HSI', 
                   n.ahead = 20, boot = TRUE, ci=0.90)
 plot(pret_irf, ylab = 'Pret închidere HSI', 
     main = 'Raspunsul prețului de închidere la numărul dezastrelor naturale')
 
 
svg("is.svg",width=10, height=10)
dev.off()


##### Descompunerea variantei####

FEVD <- fevd(model, n.ahead = 8)
plot(FEVD) 
dev.off()


#### Prognoza VAR####
model_forecast <- VAR(dset, p = 5, type = 'const', season = NULL, exog = NULL)
forecast <- predict(model_forecast, n.ahead = 12, ci = 0.90) 

plot(forecast, name = 'LNumăr_dezastre_naturale_în_China')
dev.off()

plot(forecast, name = 'LPreț_închidere_HSI')
dev.off()



####INDIA#################################################

library(modelsummary)
library(kableExtra)
library(writexl)


df_intc <- getSymbols('^NSEI',src='yahoo',from = '2008-01-01',auto.assign=FALSE)
?getSymbols
Preț_închidere_N225 <- to.quarterly(df_intc)
#write_xlsx(data.frame(Preț_închidere_NSEI), 'Pretindia.xlsx')


Preț_închidere_NSEI <- read.csv("Pretindia.csv")

numar_dezastre <- 
  Date1900_2021 %>%
  #select(ISO,Continent,`Start Year`,`Start Month`) %>%
  arrange(`Start Year`,`Start Month`)%>%
  filter(Country=="India") %>%
  filter(`Start Year`>2007) %>%
  group_by(`Start Year`,`Start Month`) %>%
  count(ISO) 

numar_dezastre<-
  numar_dezastre %>% 
  group_by(`Start Year`,`Start Month`) %>%
  summarize(summary_variable = sum(na.omit(n)))

#write_xlsx(numar_dezastre, 'DezastreIndia.xlsx')


Număr_dezastre_naturale_în_India <- read_excel("DezastreIndia.xlsx")

dset <- data.frame(Preț_închidere_NSEI,Număr_dezastre_naturale_în_India)


library(moments)
skewness(Preț_închidere_NSEI)  # distributie asimetrica la stanga, PREDOMINA VALORILE MICI
kurtosis(Preț_închidere_NSEI)  #distributie leptocurtica, EXISTAND FRECEVNTE 


skewness(Număr_dezastre_naturale_în_India)  # distributie asimetrica la stanga
kurtosis(Număr_dezastre_naturale_în_India)   #asimetrie la dreapta, 


datasummary(All(dset)~(Media=mean)
            +(`Deviația standard`= SD)+(Minim=min)+(Maxim=max)+(`25%`=P25)+(`50%`=P50)
            +(`75%`=P75)+(Histograma=Histogram), data=dset, align = 'lcccccccc')


Preț_închidere_NSEI<- ts(Preț_închidere_NSEI,start=2008, frequency = 4)
p<-(  autoplot(Preț_închidere_NSEI, colour = "#69b3a2") +
        labs(title = "",
             x = "An",
             y = "Preț închidere (Rupie indiană ₹)") +
        theme_fivethirtyeight() +
        theme(axis.title = element_text())+
        scale_x_continuous("An",breaks=seq(min(start(Preț_închidere_NSEI)),max(end(Preț_închidere_NSEI)), by=3), expand = c(0,0))
)

ggplotly(p)
ggsave(file="Pret inchidreIdia.svg", plot=p, width=10, height=10)



Număr_dezastre_naturale_în_India <- ts(Număr_dezastre_naturale_în_India,start=2008, frequency = 4)
p<-(  autoplot(Număr_dezastre_naturale_în_India, colour = "#69b3a2") +
        labs(title = "",
             x = "An",
             y = "Număr dezastre naturale") +
        theme_fivethirtyeight() +
        theme(axis.title = element_text())+
        scale_x_continuous("An",breaks=seq(min(start(Număr_dezastre_naturale_în_India)),max(end(Număr_dezastre_naturale_în_India)), by=3), expand = c(0,0))
      
)
ggplotly(p)
ggsave(file="Număr dezastre India.svg", plot=p, width=10, height=10)




library(urca)
library(forecast)
library(vars)
library(tidyverse)
library(tsDyn)
library(dynlm)
library(aTSA)
library(stargazer)


LPreț_închidere_NSEI <- log(Preț_închidere_NSEI)
LNumăr_dezastre_naturale_în_India <- log(Număr_dezastre_naturale_în_India)


# Crearea unui df cu toate cele 2variabile
dset <- cbind(LPreț_închidere_NSEI,LNumăr_dezastre_naturale_în_India)


# Graficul seriei
autoplot(dset) + 
  ylab('') + 
  ggtitle('Graficul seriei multivariate') + 
  theme_bw()




#####Testarea stationaritatii seriilor pentru pretul de inchidere al indicelui NSEI ####

# Trend si intercept

adf_t_i <- LPreț_închidere_NSEI %>%ur.df(., type='trend', selectlags=c("AIC"))
summary(adf_t_i)

# Intercept

adf_i <- LPreț_închidere_NSEI %>%ur.df(., type='drift', selectlags=c("AIC"))
summary(adf_i) 

# Elemente deterministe

adf_e <- LPreț_închidere_NSEI %>%ur.df(., type="none", selectlags=c("AIC"))
summary(adf_e) 


#### Testarea stationaritatii prin KPSS pentru seria nediferentiata ####

LPreț_închidere_NSEI %>% ur.kpss() %>% summary() 

# Testarea stationaritatii prin Philips-Perron pentru seria nediferentiata
PP.test(LPreț_închidere_NSEI) 







#####Testarea stationaritatii seriilor pentru numarul de dezastre naturale - india ####

# Trend si intercept

adf_t_i <- LNumăr_dezastre_naturale_în_India %>%ur.df(., type='trend', selectlags=c("AIC"))
summary(adf_t_i)  

# Intercept

adf_i <- LNumăr_dezastre_naturale_în_India %>%ur.df(., type='drift', selectlags=c("AIC"))
summary(adf_i) 

# Elemente deterministe

adf_e <- LNumăr_dezastre_naturale_în_India %>%ur.df(., type="none", selectlags=c("AIC"))
summary(adf_ur) 


#### Testarea stationaritatii prin KPSS  ####

LNumăr_dezastre_naturale_în_India %>% ur.kpss() %>% summary() 

# Testarea stationaritatii prin Philips-Perron 
PP.test(LNumăr_dezastre_naturale_în_India) 



#Pentru a confirma faptul că ambele serii sunt nestaționare am utilizat și funcția ndiffs()

ndiffs(LNumăr_dezastre_naturale_în_India)
ndiffs(LPreț_închidere_NSEI)
#Una din serii este staționară, cealaltă fiind nestaționară, deci avem o serie I(1) și UNA I(0)




# Cointegrarea Johansen

# H0: nu exista cointegrare
# H1: exista cel putin o relatie de cointegrare

# Selectarea lagului 
lagselect <- VARselect(dset, lag.max = 12, type = 'trend')
lagselect$selection 


# Testul Johansen - metoda Trace
ctest1 <- ca.jo(na.omit(dset), type = 'trace', ecdet = 'const',K=5)
summary(ctest1) 




# Testul Johansen - metoda valorilor proprii maxime
ctest2 <- ca.jo(dset, type = 'eigen', ecdet = 'const',K=5)
summary(ctest2)


dset <- cbind(diff(LPreț_închidere_NSEI),LNumăr_dezastre_naturale_în_India)

model <- VAR(na.omit(dset), p =6, type = 'trend', season =NULL, exog = NULL)
summary(model)
?VAR

stargazer(model[['varresult']], type = 'text')



# Autocorelarea
Serial <- serial.test(model, lags.pt = 24, type = 'PT.asymptotic')
Serial 

# Heteroscedasticitate
Arch <- vars::arch.test(model,lags.multi = 12,multivariate.only = TRUE)
Arch 

# Normalitatea reziduurilor
Norm <- normality.test(model, multivariate.only = TRUE)
Norm 


Stability <- stability(model,type = 'OLS-CUSUM')
svg("India-STABILITATE.svg",width=10, height=10)
plot(Stability) 
dev.off()



# Cauzalitate Granger
Granger_dezastre <- causality(model, cause = "Număr_dezastre_naturale_în_India")
Granger_dezastre 

Granger_pret <- causality(model, cause = 'LPreț_închidere_NSEI')
Granger_pret 



# Functia de raspuns la impuls (IRF) 
dezastre_irf <- irf(model, impulse = 'diff.LPreț_închidere_NSEI.', response = 'Număr_dezastre_naturale_în_India', 
                    n.ahead = 20, boot = TRUE, ci=0.90) 
plot(dezastre_irf, ylab = 'Număr dezastre naturale', 
     main = 'Raspunsul dezastrelor naturale la socurile prețului')


pret_irf <- irf(model, impulse = 'Număr_dezastre_naturale_în_India', response = 'diff.LPreț_închidere_NSEI.', 
                n.ahead = 20, boot = TRUE, ci=0.90)
plot(pret_irf, ylab = 'Pret închidere HSI', 
     main = 'Raspunsul prețului de închidere la numărul dezastrelor naturale')
svg("India-Funcțe de impuls la răspuns.svg",width=10, height=10)
dev.off()



# Descompunerea variante

FEVD <- fevd(model, n.ahead = 8)
plot(FEVD) 
svg(" india varianta.svg",width=10, height=10)
dev.off()


dset <- cbind(LPreț_închidere_NSEI,LNumăr_dezastre_naturale_în_India)


# Prognoza VAR
model_forecast <- VAR(dset, p = 5, type = 'const', season = NULL, exog = NULL)
forecast <- predict(model_forecast, n.ahead = 8, ci = 0.90) # prognoza pe 2 ani

plot(forecast, name = 'LNumăr_dezastre_naturale_în_India')
svg(" India forecast dezastre .svg",width=10, height=10)
dev.off()
plot(forecast, name = 'LPreț_închidere_NSEI')
svg(" india forecast pret.svg",width=10, height=10)
dev.off()



##########################################################SUA#################################################

library(modelsummary)
library(kableExtra)
library(writexl)
library('TTR')
library('quantmod')

df_intc <- getSymbols('^GSPC',src='yahoo',from = '1941-01-01',auto.assign=FALSE)
?getSymbols
Preț_închidere_GSPC <- to.yearly(df_intc)
#write_xlsx(data.frame(Preț_închidere_GSPC), 'PretSUA.xlsx')


Preț_închidere_GSPC <- read.csv("PretSUA.csv")

numar_dezastre <- 
  Date1900_2021 %>%
  #select(ISO,Continent,`Start Year`,`Start Month`) %>%
  arrange(`Start Year`)%>%
  filter(Country=="United States of America (the)") %>%
  group_by(`Start Year`) %>%
  count(ISO) 

numar_dezastre<-
  numar_dezastre %>% 
  group_by(`Start Year`) %>%
  summarize(summary_variable = sum(na.omit(n)))

#write_xlsx(numar_dezastre, 'DezastreSUA.xlsx')

Număr_dezastre_naturale_în_SUA <- read_excel("DezastreSUA.xlsx")

dset <- data.frame(Preț_închidere_GSPC,Număr_dezastre_naturale_în_SUA)

datasummary(All(dset)~(Media=mean)
            +(`Deviația standard`= SD)+(Minim=min)+(Maxim=max)+(`25%`=P25)+(`50%`=P50)
            +(`75%`=P75)+(Histograma=Histogram), data=dset, align = 'lcccccccc',output = 'america.docx')

library(moments)
skewness(Preț_închidere_GSPC)  # distributie asimetrica la stanga
kurtosis(Preț_închidere_GSPC)  #distributie leptocurtica


skewness(Număr_dezastre_naturale_în_SUA)  # distributie asimetrica la stanga
kurtosis(Număr_dezastre_naturale_în_SUA)   #asimetrie la dreapta, 

Preț_închidere_GSPC<- ts(Preț_închidere_GSPC,start=1965, frequency =1)
p<-(  autoplot(Preț_închidere_GSPC, colour = "#69b3a2") +
        labs(title = "",
             x = "An",
             y = "Preț închidere ($)") +
        theme_fivethirtyeight() +
        theme(axis.title = element_text())+
        scale_x_continuous("An",breaks=seq(min(start(Preț_închidere_GSPC)),max(end(Preț_închidere_GSPC)), by=4), expand = c(0,0))
)

ggplotly(p)
ggsave(file="Pret inchidre SUA.svg", plot=p, width=10, height=10)



Număr_dezastre_naturale_în_SUA <- ts(Număr_dezastre_naturale_în_SUA,start=1965, frequency = 1)
p<-(  autoplot(Număr_dezastre_naturale_în_SUA, colour = "#69b3a2") +
        labs(title = "",
             x = "An",
             y = "Număr dezastre naturale") +
        theme_fivethirtyeight() +
        theme(axis.title = element_text())+
        scale_x_continuous("An",breaks=seq(min(start(Număr_dezastre_naturale_în_SUA)),max(end(Număr_dezastre_naturale_în_SUA)), by=4), expand = c(0,0))
      
)
ggplotly(p)
ggsave(file="Număr dezastre SUA.svg", plot=p, width=10, height=10)




library(urca)
library(forecast)
library(vars)
library(tidyverse)
library(tsDyn)
library(dynlm)
library(aTSA)
library(stargazer)


LPreț_închidere_GSPC <- log(Preț_închidere_GSPC)
LNumăr_dezastre_naturale_în_SUA <- log(Număr_dezastre_naturale_în_SUA)


# Crearea unui df cu toate cele 2variabile
dset <- cbind(LPreț_închidere_GSPC,LNumăr_dezastre_naturale_în_SUA)


# Graficul seriei
autoplot(dset) + 
  ylab('') + 
  ggtitle('Graficul seriei multivariate') + 
  theme_bw()




#####Testarea stationaritatii seriilor pentru pretul de inchidere al indicelui GSPC ####

# Trend si intercept

adf_t_i <- LPreț_închidere_GSPC %>%ur.df(., type='trend', selectlags=c("AIC"))
summary(adf_t_i) 

# Intercept

adf_i <- LPreț_închidere_GSPC %>%ur.df(., type='drift', selectlags=c("AIC"))
summary(adf_i) 

# Elemente deterministe

adf_e <- LPreț_închidere_GSPC %>%ur.df(., type="none", selectlags=c("AIC"))
summary(adf_ur)


#### Testarea stationaritatii prin KPSS ####

LPreț_închidere_GSPC %>% ur.kpss() %>% summary() 

# Testarea stationaritatii prin Philips-Perron
PP.test(LPreț_închidere_GSPC) 






#####Testarea stationaritatii seriilor pentru numarul de dezastre naturale - USA ####

# Trend si intercept

adf_t_i <- LNumăr_dezastre_naturale_în_SUA %>%ur.df(., type='trend', selectlags=c("AIC"))
summary(adf_t_i) 

# Intercept

adf_i <- LNumăr_dezastre_naturale_în_SUA %>%ur.df(., type='drift', selectlags=c("AIC"))
summary(adf_i) 

# Elemente deterministe

adf_e <- LNumăr_dezastre_naturale_în_SUA %>%ur.df(., type="none", selectlags=c("AIC"))
summary(adf_e) 


#### Testarea stationaritatii prin KPSS  ####

LNumăr_dezastre_naturale_în_SUA %>% ur.kpss() %>% summary() 


# Testarea stationaritatii prin Philips-Perron 
PP.test(LNumăr_dezastre_naturale_în_SUA) 


#Pentru a confirma faptul că ambele serii sunt nestaționare am utilizat și funcția ndiffs()

ndiffs(LNumăr_dezastre_naturale_în_SUA)
ndiffs(LPreț_închidere_GSPC)



#### Cointegrarea Johansen####

# H0: nu exista cointegrare
# H1: exista cel putin o relatie de cointegrare

# Selectarea lagului 
lagselect <- VARselect(dset, lag.max =12, type = 'const')
lagselect$selection 



# Testul Johansen 
ctest1 <- ca.jo(na.omit(dset), type = 'trace', ecdet = 'const',K=5)
summary(ctest1) 


# Testul Johansen 
ctest2 <- ca.jo(na.omit(dset), type = 'eigen', ecdet = 'const',K=5)
summary(ctest2)


model <- VAR(diff(dset), p =6, type = 'trend', season =NULL, exog = NULL,ic = c("AIC"))
summary(model)
?VAR

stargazer(model[['varresult']], type = 'text')




# Autocorelarea
Serial <- serial.test(model, lags.pt = 12, type = 'PT.asymptotic')
Serial 

# Heteroscedasticitate
Arch <- vars::arch.test(model,lags.multi = 12,multivariate.only = TRUE)
Arch 

# Normalitatea reziduurilor
Norm <- normality.test(model, multivariate.only = TRUE)
Norm 



Stability <- stability(model,type = 'OLS-CUSUM')
svg("SUA-stabilitate.svg",width=10, height=10)
plot(Stability) 
dev.off()


# Cauzalitate Granger
Granger_dezastre <- causality(model, cause = "LNumăr_dezastre_naturale_în_SUA")
Granger_dezastre # p > 0.1 => Dezastrele naturale   prezinta cauzalitate Granger cu pretul de inchidere

Granger_pret <- causality(model, cause = 'LPreț_închidere_GSPC')
Granger_pret # p > 0.1 => Prețul de închidere   prezinta cauzalitate Granger cu numărul de dezastre



# Functia de raspuns la impuls (IRF) 
dezastre_irf <- irf(model, impulse = 'LPreț_închidere_GSPC', response = 'LNumăr_dezastre_naturale_în_SUA', 
                    n.ahead = 20, boot = TRUE, ci=0.90) 
plot(dezastre_irf, ylab = 'Număr dezastre naturale', 
     main = 'Raspunsul dezastrelor naturale la socurile prețului')

pret_irf <- irf(model, impulse = 'LNumăr_dezastre_naturale_în_SUA', response = 'LPreț_închidere_GSPC', 
                n.ahead = 20, boot = TRUE, ci=0.90)
plot(pret_irf, ylab = 'Pret închidere ^GSPC', 
     main = 'Raspunsul prețului de închidere la numărul dezastrelor naturale')


svg("sua raspuns la impuls.svg",width=10, height=10)
dev.off()


# Descompunerea variante

FEVD <- fevd(model, n.ahead = 6)
plot(FEVD) 
svg("sua varianta .svg",width=10, height=10)
dev.off()


# Prognoza VAR
model_forecast <- VAR(dset, p = 6, type = 'const', season = NULL, exog = NULL)
forecast <- predict(model_forecast, n.ahead = 12, ci = 0.90) # prognoza pe 1 an

plot(forecast, name = 'LNumăr_dezastre_naturale_în_SUA')
svg(" SUA  forecast dezastre .svg",width=10, height=10)
dev.off()
plot(forecast, name = 'LPreț_închidere_GSPC')
svg(" SUA forecast PRET.svg",width=10, height=10)
dev.off()



##########################################################JAPONIA#################################################

library(modelsummary)
library(kableExtra)
library(writexl)
library('TTR')
library('quantmod')

df_intc <- getSymbols('^N225',src='yahoo',from = '1966-01-01',auto.assign=FALSE)
Preț_închidere_N225 <- to.yearly(df_intc)
#write_xlsx(data.frame(Preț_închidere_N225), 'Pretjaponia.xlsx')


Preț_închidere_N225 <- read.csv("Pretjaponia.csv")



numar_dezastre <- 
  Date1900_2021 %>%
  #select(ISO,Continent,`Start Year`,`Start Month`) %>%
  arrange(`Start Year`)%>%
  filter(Country=="Japan") %>%
  group_by(`Start Year`) %>%
  count(ISO) 

numar_dezastre<-
  numar_dezastre %>% 
  group_by(`Start Year`) %>%
  summarize(summary_variable = sum(na.omit(n)))

#write_xlsx(numar_dezastre, 'DezastreJaponia.xlsx')

Număr_dezastre_naturale_în_Japonia <- read_excel("DezastreJaponia.xlsx")

dset <- data.frame(Preț_închidere_N225,Număr_dezastre_naturale_în_Japonia)

datasummary(All(dset)~(Media=mean)
            +(`Deviația standard`= SD)+(Minim=min)+(Maxim=max)+(`25%`=P25)+(`50%`=P50)
            +(`75%`=P75)+(Histograma=Histogram), data=dset, align = 'lcccccccc',output = 'japonia.docx')


library(moments)
skewness(Preț_închidere_N225)  # predomină valorile mici
kurtosis(Preț_închidere_N225)  #distribuție leptocurtică
skewness(Număr_dezastre_naturale_în_Japonia)  # predomină valorile mici
kurtosis(Număr_dezastre_naturale_în_Japonia)  #distribuție platicurtica


Preț_închidere_N225<- ts(Preț_închidere_N225,start=1973, frequency =1)
p<-(  autoplot(Preț_închidere_GSPC, colour = "#69b3a2") +
        labs(title = "",
             x = "An",
             y = "Preț închidere (dolar Hong Kong)") +
        theme_fivethirtyeight() +
        theme(axis.title = element_text())+
        scale_x_continuous("An",breaks=seq(min(start(Preț_închidere_GSPC)),max(end(Preț_închidere_GSPC)), by=3), expand = c(0,0))
)

ggplotly(p)
ggsave(file="Japonia Pret inchidre.svg", plot=p, width=10, height=10)



Număr_dezastre_naturale_în_Japonia <- ts(Număr_dezastre_naturale_în_Japonia,start=1973, frequency = 1)
p<-(  autoplot(Număr_dezastre_naturale_în_Japonia, colour = "#69b3a2") +
        labs(title = "",
             x = "An",
             y = "Număr dezastre naturale") +
        theme_fivethirtyeight() +
        theme(axis.title = element_text())+
        scale_x_continuous("An",breaks=seq(min(start(Număr_dezastre_naturale_în_Japonia)),max(end(Număr_dezastre_naturale_în_Japonia)), by=3), expand = c(0,0))
      
)
ggplotly(p)
ggsave(file="Japonia număr dezastre.svg", plot=p, width=10, height=10)





library(urca)
library(forecast)
library(vars)
library(tidyverse)
library(tsDyn)
library(dynlm)
library(aTSA)
library(stargazer)


LPreț_închidere_N225 <- log(Preț_închidere_N225)
LNumăr_dezastre_naturale_în_Japonia <- log(Număr_dezastre_naturale_în_Japonia)



dset <- cbind(LPreț_închidere_N225,LNumăr_dezastre_naturale_în_Japonia)
na.omit(dset)

# Graficul seriei
autoplot(dset) + 
  ylab('') + 
  ggtitle('Graficul seriei multivariate') + 
  theme_bw()




#####Testarea stationaritatii seriilor pentru pretul de inchidere al indicelui n225 ####

# Trend si intercept

adf_t_i <- LPreț_închidere_N225 %>%ur.df(., type='trend', selectlags=c("AIC"))
summary(adf_t_i) 

# Intercept

adf_i <- LPreț_închidere_N225 %>%ur.df(., type='drift', selectlags=c("AIC"))
summary(adf_i) 

# Elemente deterministe

adf_e <- LPreț_închidere_N225 %>%ur.df(., type="none", selectlags=c("AIC"))
summary(adf_e) 

#### Testarea stationaritatii prin KPSS  ####

LPreț_închidere_N225 %>% ur.kpss() %>% summary() 

# Testarea stationaritatii prin Philips-Perron 
PP.test(LPreț_închidere_N225) 





#####Testarea stationaritatii seriilor pentru numarul de dezastre naturale - USA ####

# Trend si intercept

adf_t_i <- LNumăr_dezastre_naturale_în_Japonia %>%ur.df(., type='trend', selectlags=c("AIC"))
summary(adf_t_i) 

# Intercept

adf_i <- LNumăr_dezastre_naturale_în_Japonia %>%ur.df(., type='drift', selectlags=c("AIC"))
summary(adf_i) 

# Elemente deterministe

adf_e <- LNumăr_dezastre_naturale_în_Japonia %>%ur.df(., type="none", selectlags=c("AIC"))
summary(adf_e) 


#### Testarea stationaritatii prin KPSS p ####

LNumăr_dezastre_naturale_în_Japonia %>% ur.kpss() %>% summary() 

# Testarea stationaritatii prin Philips-Perron
PP.test(LNumăr_dezastre_naturale_în_Japonia) 



#Pentru a confirma faptul că ambele serii sunt nestaționare am utilizat și funcția ndiffs()

ndiffs(LNumăr_dezastre_naturale_în_Japonia)
ndiffs(LPreț_închidere_N225)



# Cointegrarea Johansen

# H0: nu exista cointegrare
# H1: exista cel putin o relatie de cointegrare

lagselect <- VARselect(dset, lag.max =24, type = 'none')
lagselect$selection 



# Testul Johansen
ctest1 <- ca.jo(na.omit(dset), type = 'trace', ecdet = 'const',K=12)
summary(ctest1) 


# Testul Johansen 
ctest2 <- ca.jo(na.omit(dset), type = 'eigen', ecdet = 'const',K=12)
summary(ctest2)

# Ambele metode de cointegrare Johanses confirma ca avem cel putin o relatie de cointegrare



#Modelul VECM - metoda de estimare 2OLS
Model1 <- VECM(dset,
               lag = 12, 
               r=1, 
               estim = ('2OLS'),
               LRinclude = 'const')
summary(Model1)


Model1VAR <- vec2var(ctest1, r = 1)



# Autocorelarea
Serial <- serial.test(Model1VAR, lags.pt = 15, type = 'PT.asymptotic')
Serial 

# Heteroscedasticitate
Arch <- vars::arch.test(Model1VAR,lags.multi = 15,multivariate.only = TRUE)
Arch 

# Normalitatea reziduurilor
Norm <- normality.test(Model1VAR, multivariate.only = TRUE)
Norm 



# Functia de raspuns la impuls (IRF) 
dezastre_irf <- irf(Model1VAR, impulse = 'LPreț_închidere_N225', response = 'LNumăr_dezastre_naturale_în_Japonia', 
                    n.ahead = 20, boot = TRUE, ci=0.90) 
plot(dezastre_irf, ylab = 'Număr dezastre naturale', 
     main = 'Raspunsul dezastrelor naturale la socurile prețului')

pret_irf <- irf(Model1VAR, impulse = 'LNumăr_dezastre_naturale_în_Japonia', response = 'LPreț_închidere_N225', 
                n.ahead = 8, boot = TRUE, ci=0.90)
plot(pret_irf, ylab = 'Pret închidere HSI', 
     main = 'Raspunsul prețului de închidere la numărul dezastrelor naturale')
#svg("Japonia-Funcția de răspuns la inpuls.svg",width=10, height=10)
#dev.off()



# Descompunerea variante

FEVD1 <- fevd(Model1VAR , n.ahead = 6)
plot(FEVD1) 
#svg("Japonia-Descompunerea variantei.svg",width=10, height=10)
#dev.off()


# Prognoza 
forecast <- predict(Model1VAR, n.ahead = 6, ci = 0.90) 
svg("Japonia - Forcast pret.svg",width=10, height=10)
plot(forecast, name = 'LPreț_închidere_N225')
dev.off()
svg("Japonia - Forcast dezastre.svg",width=10, height=10)
plot(forecast, name = 'LNumăr_dezastre_naturale_în_Japonia')
dev.off()




