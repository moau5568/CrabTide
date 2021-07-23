---
title: "Interactive Effect of Crab Burrows and Tides on Mangrove Carbon Storage"
author: "Moritz Klaassen"
date: "May 27th 2021"
output: github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

This document displays the R-Script and Output of the thesis project "Interactive Effect of Crab Burrows and Tides on Mangrove Carbon Storage" conducted at the Leibniz Centre for Marine Tropical Research in Bremen, Germany within the frame of the International Master's in Marine Biological Resources (IMBRSea)

### Content 

00_Load Packages

01_Attach, Subset and Prepare Data
    
    DOC Over Time
    DOC Changes
    Fractions
    POM
    CO2 Flux
    Leaching
    
02_DOC Over Time

    Visualization 
        Mesotidal Concentration 
        Microtidal Concentration 
        Mesotidal Absolute 
        Microtidal Absolute
        
03_DOC Changes 
 
    Visualitation 
        Concentrations 
        Absolute 
    Statistics 
        Concentrations
            ANCOVA 
                Assumptions 
                    Linearity 
                    Homogeneity of Regression Slopes 
                    Normality of Residuals 
                    Homogeneity of Variances 
                    Visual Inspections 
                    Outliers
        Absolute
            ANCOVA 
                Assumptions 
                    Linearity 
                    Homogeneity of Regression Slopes 
                    Normality of Residuals 
                    Homogeneity of Variances 
                    Visual Inspections 
                    Outliers
                    
04_Fractions

    Visualization 
        Dry Mass 
        Percentage 
    Statistics
        NMDS Analysis 
        PERMANOVA Assumptions 
        PERMANOVA (ADONIS)
        Pairwise Adonis 
        SIMPER
        
05_POM

     Visualization
         Absolute Consumption 
         Percentage Consumption 
         Carapace Correlation GLOBAL 
         Carapace Correlation Meso vs. Micro
     Statistics 
         ANCOVA POM Consumption
                Assumptions 
                    Linearity 
                    Homogeneity of Regression Slopes 
                    Normality of Residuals 
                    Homogeneity of Variances 
                    Visual Inspections 
                    Outliers
                    
06_CO2 Flux

     Visualization 
     Statistics
         ANCOVA CO2 Consumption
                Assumptions 
                    Linearity 
                    Homogeneity of Regression Slopes 
                    Normality of Residuals 
                    Homogeneity of Variances 
                    Visual Inspections 
                    Outliers
         ANCOVA log CO2 Consumption
                Assumptions 
                    Linearity 
                    Homogeneity of Regression Slopes 
                    Normality of Residuals 
                    Homogeneity of Variances 
                    Visual Inspections 
                    Outliers
         ANCOVA sqrt CO2 Consumption
                Assumptions 
                    Linearity 
                    Homogeneity of Regression Slopes 
                    Normality of Residuals 
                    Homogeneity of Variances 
                    Visual Inspections 
                    Outliers
07_Leaching

     Visualization 
         DOC Leached 
         Leaching Rate
     Statistics 
         Anova Leaching Rate 
         Anova Assumptions 
             Normality of residuals 
             Homogeneity of Variances 
             Visual Inspections 

###00_Load Packages

```{r, results = FALSE, message=FALSE}
library(ggplot2)
library(car)
library(RColorBrewer)
library(dplyr)
library(plyr)
library(magrittr)
library(knitr)
library(plyr)
library(knitr)
library(cowplot)
library(ggpubr)
library(metaviz)
library(irr)
library(vcd)
library(graphics)
library(reshape2)
library(lme4)
library(multcomp)
library(emmeans)
library(agricolae)
library(psych)
library(MuMIn)
library(MASS)
library(devtools)
library(pairwiseAdonis)
library(rstatix)
library(formatR)
library(rmarkdown)
library(tinytex)
library(htmltools)
library(xfun)
library(lindia)
library(MASS)
```

### 01_Attach, Subset and Prepare Data

DOC Over Time

```{r, results = FALSE, message=FALSE}


setwd("C:/Users/Moritz Klaassen/Desktop/Thesis")
data <- read.csv("DOCtime.csv",header = TRUE, dec = ",", sep = ";", na.string = "")
attach(data)
str(data)
data$Crab <- as.factor(data$Crab)
data$Tide <- as.factor(data$Tide)
data$T <- as.factor(data$T)
 
Meso <- droplevels((data[c(which(data$Tide=="Meso")),]))
CrabPMeso <- droplevels((Meso[c(which(Meso$Crab=="P")),]))
Micro <- droplevels((data[c(which(data$Tide=="Micro")),]))
CrabPMicro <- droplevels((Micro[c(which(Micro$Crab=="P")),]))
CrabAMeso <- droplevels((Meso[c(which(Meso$Crab=="A")),]))
CrabAMicro <- droplevels((Micro[c(which(Micro$Crab=="A")),]))

MesoP <- ddply(CrabPMeso, c("T", "Crab"), 
                                        summarise, 
                                        N    = length(Tide),
                                        mean = mean(DOC),
                                        sd   = sd(DOC),
                                        se   = sd / sqrt(N))
MesoA <- ddply(CrabAMeso, c("T", "Crab"), 
               summarise, 
               N    = length(Tide),
               mean = mean(DOC),
               sd   = sd(DOC),
               se   = sd / sqrt(N))

MesoAP <- ddply(Meso, c("T", "Crab"), 
               summarise, 
               N    = length(Tide),
               mean = mean(DOC),
               sd   = sd(DOC),
               se   = sd / sqrt(N))

MicroP <- ddply(CrabPMicro, c("T", "Crab"), 
               summarise, 
               N    = length(Tide),
               mean = mean(DOC),
               sd   = sd(DOC),
               se   = sd / sqrt(N))
MicroA <- ddply(CrabAMicro, c("T", "Crab"), 
               summarise, 
               N    = length(Tide),
               mean = mean(DOC),
               sd   = sd(DOC),
               se   = sd / sqrt(N))

MicroAP <- ddply(Micro, c("T", "Crab"), 
                summarise, 
                N    = length(Tide),
                mean = mean(DOC),
                sd   = sd(DOC),
                se   = sd / sqrt(N))

MesototP <- ddply(CrabPMeso, c("T", "Crab"), 
               summarise, 
               N    = length(Tide),
               mean = mean(DOC.per.Day),
               sd   = sd(DOC.per.Day),
               se   = sd / sqrt(N))
MesototA <- ddply(CrabAMeso, c("T", "Crab"), 
               summarise, 
               N    = length(Tide),
               mean = mean(DOC.per.Day),
               sd   = sd(DOC.per.Day),
               se   = sd / sqrt(N))

MesototAP <- ddply(Meso, c("T", "Crab"), 
                summarise, 
                N    = length(Tide),
                mean = mean(DOC.per.Day),
                sd   = sd(DOC.per.Day),
                se   = sd / sqrt(N))

MicrototP <- ddply(CrabPMicro, c("T", "Crab"), 
                summarise, 
                N    = length(Tide),
                mean = mean(DOC.per.Day),
                sd   = sd(DOC.per.Day),
                se   = sd / sqrt(N))
MicrototA <- ddply(CrabAMicro, c("T", "Crab"), 
                summarise, 
                N    = length(Tide),
                mean = mean(DOC.per.Day),
                sd   = sd(DOC.per.Day),
                se   = sd / sqrt(N))

MicrototAP <- ddply(Micro, c("T", "Crab"), 
                 summarise, 
                 N    = length(Tide),
                 mean = mean(DOC.per.Day),
                 sd   = sd(DOC.per.Day),
                 se   = sd / sqrt(N))
```

DOC Changes

```{r, results = FALSE, message=FALSE}
DOC <- read.csv("DOC.csv",header = TRUE, dec = ",", sep = ";", na.string = "")
attach(DOC)
str(DOC)
DOC$Crab <- as.factor(DOC$Crab)
DOC$Tide <- as.factor(DOC$Tide)

DOCMESOTIDAL <- droplevels((DOC[c(which(DOC$Tide=="Meso")),]))
DOCMICROTIDAL <- droplevels((DOC[c(which(DOC$Tide=="Micro")),]))


```

Fractions

```{r, results = FALSE, message=FALSE}
FractAbs <- read.csv("FractionAbs.csv",header = TRUE, dec = ",", sep = ";", na.string = "")
attach(FractAbs)
str(FractAbs)
FractAbs$Fraction <- as.factor(FractAbs$Fraction)

Abs <-  ddply(FractAbs, c("Fraction", "Tide"), 
              summarise, 
              N    = length(Tide),
              mean = mean(Weight),
              sd   = sd(Weight),
              se   = sd / sqrt(N))

Abs$Fraction <- factor(Abs$Fraction,levels = c(">0.5", ">1", ">2", ">4", ">8", ">16", ">32"))

FractPerc <- read.csv("FractionPerc.csv",header = TRUE, dec = ",", sep = ";", na.string = "")
attach(FractPerc)
str(FractPerc)
FractPerc$Fraction <- as.factor(FractPerc$Fraction)
FractPercMeso <- droplevels((FractPerc[c(which(FractPerc$Tide=="Mesotidal")),]))
FractPercMicro <- droplevels((FractPerc[c(which(FractPerc$Tide=="Microtidal")),]))

Perc <- ddply(FractPerc, c("Fraction", "Tide"), 
              summarise, 
              N    = length(Tide),
              mean = mean(Weight),
              sd   = sd(Weight),
              se   = sd / sqrt(N))

Perc$Fraction <- factor(Perc$Fraction,levels = c(">0.5", ">1", ">2", ">4", ">8", ">16", ">32"))

PERMFRAC1 <- read.csv("PERMFRAC1.csv",header = TRUE, dec = ",", sep = ";", na.string = "")
attach(PERMFRAC1)
str(PERMFRAC1)
PERMFRAC2 <- read.csv("PERMFRAC2.csv",header = TRUE, dec = ",", sep = ";", na.string = "")
attach(PERMFRAC2)
str(PERMFRAC2)
```

POM

```{r, results = FALSE, message=FALSE}
POM <- read.csv("POM2.csv",header = TRUE, dec = ",", sep = ";", na.string = "")
attach(POM)
str(POM)
POM$Tide <- as.factor(POM$Tide)
POM$Crab <- as.factor(POM$Crab)

POMP <- droplevels((POM[c(which(POM$Crab=="P")),]))
POMMESO <- droplevels((POMP[c(which(POMP$Tide=="Meso")),]))
POMMICRO <- droplevels((POMP[c(which(POMP$Tide=="Micro")),]))
```

CO2 Flux

```{r, results = FALSE, message=FALSE}
FLUX <- read.csv("CO2 FLUX.csv",header = TRUE, dec = ",", sep = ";", na.string = "")
attach(FLUX)
str(FLUX)
FLUX$Tide <- as.factor(FLUX$Tide)
FLUX$Crab <- as.factor(FLUX$Crab)

MESOFLUX <- droplevels((FLUX[c(which(FLUX$Tide=="Meso")),]))
MICROFLUX <- droplevels((FLUX[c(which(FLUX$Tide=="Micro")),]))
FLUX <- mutate(FLUX, sqrtFLUX = sqrt(mmol.Flux))
```

Leaching 

```{r, results = FALSE, message=FALSE}
Leaching <- read.csv("Leaching.csv",header = TRUE, dec = ",", sep = ";", na.string = "")
attach(Leaching)
str(Leaching)
Leaching$Crab <- as.factor(Leaching$Crab)
Leaching$Tide <- as.factor(Leaching$Tide)
Leaching$T <- as.factor(Leaching$T)
Leaching <- droplevels((Leaching[c(which(Leaching$Crab=="A")),]))
```

### 02_DOC Over Time 

Visualization

```{r, results = FALSE, message=FALSE}
pd <- position_dodge(0.1) # move points .05 to the left and right from SE

a<-ggplot(MesoAP, aes(x=T, y=mean, group=Crab, colour=Crab)) + 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=19) +
  xlab("T") +
  ylab("DOC [umol/L]") +
  scale_colour_hue(name="Crab",    
                   breaks=c("A", "P"),
                   labels=c("Absent", "Present"),
                   l=40) +  
  theme_classic()+
  theme(legend.position ="bottom")+
  expand_limits(y=0) +            
  scale_y_continuous(labels = scales::comma,limits=c(0, 1250), expand = c(0, 0))+
  xlab("Time")+
  theme(legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 14),
        axis.title.x = element_text(size =12),
        axis.title.y = element_text(size =12))

b<-ggplot(MicroAP, aes(x=T, y=mean, group=Crab, colour=Crab)) + 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  scale_colour_hue(name="Crab",    
                   breaks=c("A", "P"),
                   labels=c("Absent", "Present"),
                   l=40) +    geom_point(position=pd, size=3, shape=19) +
  xlab("T") +
  ylab("DOC [umol/L]") +
  theme_classic()+
  theme(legend.position ="bottom")+
  expand_limits(y=0) +            
  scale_y_continuous(labels = scales::comma,limits=c(0, 1250), expand = c(0, 0))+
  xlab("Time") +
  theme(legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 14),
        axis.title.x = element_text(size =12),
        axis.title.y = element_text(size =12))

c <- ggplot(MesototAP, aes(x=T, y=mean, group=Crab, colour=Crab)) + 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=19) +
  xlab("T") +
  ylab("DOC [umol/L]") +
  scale_colour_hue(name="Crab",    
                   breaks=c("A", "P"),
                   labels=c("Absent", "Present"),
                   l=40) +  
  theme_classic()+
  theme(legend.position ="bottom")+
  expand_limits(y=0) +            
  scale_y_continuous(name="total outflowing DOC [umol]", labels = scales::comma, breaks=c(0, 500, 1000, 1500, 2000, 2500))+
  xlab("Time")+
  theme(legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 14),
        axis.title.x = element_text(size =12),
        axis.title.y = element_text(size =12))

d<-ggplot(MicrototAP, aes(x=T, y=mean, group=Crab, colour=Crab)) + 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=19) +
  xlab("T") +
  ylab("DOC [umol/L]") +  
  theme_classic()+
  theme(legend.position ="bottom")+
  expand_limits(y=0) +            
  scale_y_continuous(name="total outflowing DOC [umol]", labels = scales::comma, breaks=c(0, 500, 1000, 1500, 2000, 2500))+
  xlab("Time")+
  theme(legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 14),
        axis.title.x = element_text(size =12),
        axis.title.y = element_text(size =12))+ 
  scale_colour_hue(name="Crab",    
                   breaks=c("A", "P"),
                   labels=c("Absent", "Present"),
                   l=40)

z <- ggarrange(a, b, c, d, ncol = 2, nrow = 2, common.legend = TRUE, legend ="right",  labels = "AUTO",   font.label = list(size = 16), hjust = c(-5, -5))


```

```{r, echo = TRUE}
plot(z)
```

## 03_DOC Changes 

Visualization 

```{r, results = FALSE, message=FALSE}

# DOC Concentration
a <- ggplot(DOC, aes(x = Tide, y = DOCCONCTOT, fill = Crab)) +
  scale_y_continuous(name="DOC Concentration Change(umol/L)", labels = scales::comma, breaks=c(0, 250, 500, 750, 1000))+
  geom_boxplot(outlier.shape = NA) +
  theme_classic()+
  theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black"))+  scale_fill_brewer(breaks=c("A", "P"), 
                                                                                                                      labels=c('Absent', 'Present'),palette="Set2")+
  theme(axis.title.x = element_text(size =13))+
  theme(axis.title.y = element_text(size =11))+
  theme(legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 14))+
  theme(legend.position ="bottom")

# DOC Absolute
b <-ggplot(DOC, aes(x = Tide, y = DOCABSTOT, fill = Crab)) +
  scale_y_continuous(name="Absolute DOC Change (umol)", labels = scales::comma, breaks=c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000))+
  geom_boxplot(outlier.shape = NA) +
  theme_classic()+
  theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black"))+  scale_fill_brewer(breaks=c("A", "P"), 
                                                                                                                      labels=c('Absent', 'Present'),palette="Set2")+
  theme(axis.title.x = element_text(size =13))+
  theme(axis.title.y = element_text(size =11))+
  theme(legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 14))+
  theme(legend.position ="bottom")

z <- ggarrange(a + rremove("xlab"), b , ncol = 1, nrow = 2, common.legend = TRUE, legend ="right", labels = "AUTO",   font.label = list(size = 16), hjust = c(-5.5, -5.5))

```

```{r, echo = TRUE}

plot (z)

```

```{r, warnings=FALSE}
# DOC Concentration
# ANCOVA 
DOC.Conc.model <- lm(DOCCONCTOT~ Crab * Tide * POM.TOT , data = DOC)
anova(DOC.Conc.model)
summary(DOC.Conc.model)
# Assumptions 
      # Linearity
ggscatter(
  DOC, x = "POM.TOT", y = "DOCCONCTOT",
  facet.by  = c("Tide", "Crab"), 
  short.panel.labs = FALSE
)+
  stat_smooth(method = "loess", span = 0.9)
      # Homogeneity of Regression Slopes 
anova_test(DOCCONCTOT ~ Crab + Tide + POM.TOT +  Crab*Tide + Crab*POM.TOT +
      Tide*POM.TOT + Crab*Tide*POM.TOT, data = DOC)
      # Normality of Residuals 
aov_residuals <- residuals(object = DOC.Conc.model )
shapiro.test(aov_residuals)
      # Homogeneity of Variances 
model <- lm(DOCCONCTOT ~ POM.TOT + Crab*Tide, data = DOC)
# Inspect the model diagnostic metrics
model.metrics <- augment(model)
head(model.metrics, 3)
res<-DOC.Conc.model$residuals
hist(res, main="Histogram of standardised residuals",xlab="Standardised residuals")
leveneTest(.resid ~ Crab * Tide, data = model.metrics)
#     Visual Inspectations
plot(DOC.Conc.model, add.smooth = FALSE, which = 1)
plot(DOC.Conc.model, which = 2)
plot(DOC.Conc.model, add.smooth = FALSE, which = 3)
gg_diagnose(DOC.Conc.model)
#     Outliers
model.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()

# DOC Absolute

# ANCOVA 
DOC.Abs.Model <- lm(DOCABSTOT~ Crab * Tide * POM.TOT , data = DOC)
anova(DOC.Abs.Model)
summary(DOC.Abs.Model)
# Assumptions 
# Linearity
ggscatter(
  DOC, x = "POM.TOT", y = "DOCABSTOT",
  facet.by  = c("Tide", "Crab"), 
  short.panel.labs = FALSE
)+
  stat_smooth(method = "loess", span = 0.9)
# Homogeneity of Regression Slopes 
anova_test(DOCABSTOT ~ Crab + Tide + POM.TOT +  Crab*Tide + Crab*POM.TOT +
             Tide*POM.TOT + Crab*Tide*POM.TOT, data = DOC)
# Normality of Residuals 
aov_residuals <- residuals(object = DOC.Abs.Model )
shapiro.test(aov_residuals)
# Homogeneity of Variances 
model <- lm(DOCABSTOT ~ POM.TOT + Crab*Tide, data = DOC)
# Inspect the model diagnostic metrics
model.metrics2 <- augment(model)
head(model.metrics2, 3)
res<-DOC.Conc.model$residuals
hist(res, main="Histogram of standardised residuals",xlab="Standardised residuals")
leveneTest(.resid ~ Crab * Tide, data = model.metrics2)
#     Visual Inspectations
plot(DOC.Abs.Model, add.smooth = FALSE, which = 1)
plot(DOC.Abs.Model, which = 2)
plot(DOC.Abs.Model, add.smooth = FALSE, which = 3)
gg_diagnose(DOC.Conc.model)
#     Outliers
model.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()

```

## 04_Fractions 

Visualization
```{r, warnings = FALSE}
a <- ggplot(Abs, aes(x=Fraction, y=mean, fill=Tide)) + 
  theme_classic()+
  geom_bar(stat="identity", color="black", width=0.6, position = position_dodge(width=0.6))+
  scale_fill_brewer(palette="Greys")+
  geom_errorbar(aes(ymin=mean, ymax=mean+sd), width=.2,
              position=position_dodge(.6))+
  scale_y_continuous(labels = scales::comma, breaks=c(0, 100, 200, 300, 400, 500, 600))+
  theme(axis.title.x = element_text(size =13))+
  theme(axis.title.y = element_text(size =13))+
  theme(legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 14))+
  xlab("Fraction Size (mm)")+
  ylab("Dry Mass (mg)")



# Percentage
b <- ggplot(Perc, aes(x=Fraction, y=mean, fill=Tide)) + 
  theme_classic()+
  geom_bar(stat="identity", color="black", width=0.6, position = position_dodge(width=0.6))+
  scale_fill_brewer(palette="Greys")+
  geom_errorbar(aes(ymin=mean, ymax=mean+sd), width=.2,
                position=position_dodge(.6)) +
  scale_y_continuous(labels = scales::comma, breaks=c(0, 0.2, 0.4, 0.6, 0.8))+
  theme(axis.title.x = element_text(size =13))+
  theme(axis.title.y = element_text(size =13))+
  theme(legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 14))+
  xlab("Fraction Size (mm)")+
  ylab("Dry Mass (% of Total Dry Mass)")

ggarrange(a + rremove("xlab"), b , ncol = 1, nrow = 2, common.legend = TRUE, legend ="right", labels = "AUTO",   font.label = list(size = 16), hjust = c(-5, -5))

```

Statistics 

```{r, warning=FALSE}
# NMDS Analysis
NMDS <- metaMDS(PERMFRAC1, distance = "bray", k = 2)
NMDS

datascores <- as.data.frame(scores(NMDS))  #extract the site scores

scores <- cbind(as.data.frame(datascores), Tide = PERMFRAC2$Tide)
centroids <- aggregate(cbind(NMDS1, NMDS2) ~ Tide, data = scores, FUN = mean)
seg <- merge(scores, setNames(centroids, c('Tide','oNMDS1','oNMDS2')),
             by = 'Tide', sort = FALSE)

ggplot(scores, aes(x = NMDS1, y = NMDS2,  color = Tide)) + 
  geom_point(data = centroids, size = 4) +  
  scale_color_brewer(palette="Set1")+
  geom_segment(data = seg,
               mapping = aes(xend = oNMDS1, yend = oNMDS2)) +# add centroids
  geom_point() + 
  coord_fixed()+    
  theme_bw()+ 
  theme(legend.position="right",legend.text=element_text(size=10),legend.direction='vertical')+
  theme(axis.title.x = element_text(size =12))+
  theme(axis.title.y = element_text(size =12))+
  theme(legend.title = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 12))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# PERMANOVA Assumptions

#Check assumption of homogeneity of multivariate dispersion
distances_data <- vegdist(PERMFRAC1)
anova(betadisper(distances_data, PERMFRAC2$Tide))

# PERMANOVA (Bootstrapping and testing for differences between the groups)
PERMFIT <- adonis(PERMFRAC1 ~ Tide, data=PERMFRAC2, permutations=999, method="bray")
PERMFIT

# Pairwise Comparison
pair.mod<-pairwise.adonis(PERMFRAC1,factors=PERMFRAC2$Tide)
pair.mod

# SIMPER 

sim <- with(PERMFRAC2, simper(PERMFRAC1, Tide, permutations = 999))
summary(sim)

```

## 05_POM 

Visualization 

```{r, warning=FALSE}
# Absolute Consumption 
a<-ggplot(POM, aes(x = Tide, y = POM.diff, fill = Tide)) +
  scale_y_continuous(name="POM Consumption (mg)", labels = scales::comma, breaks=c(0, 500, 1000, 1500, 2000, 2500))+
  geom_boxplot(outlier.shape = NA) +
  theme_classic()+
  scale_fill_brewer(palette="Set2")+
  xlab("Tide")+ylab("POM")+
  theme(legend.position ="bottom")+
  theme(axis.title.x = element_text(size =12))+
  theme(axis.title.y = element_text(size =10))+
  theme(legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 14))

# Percentage Consumption
b<-ggplot(POMP, aes(x = Tide, y = POM.perc, fill = Tide)) +
  scale_y_continuous(name="POM Consumption (% of initial dry mass)", labels = scales::comma, breaks=c(0, 0.2, 0.4, 0.6, 0.8,1))+
  geom_boxplot(outlier.shape = NA) +
  scale_fill_brewer(palette="Set2")+
  theme_classic()+
  theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black"))+
  xlab("Tide")+ylab("Pin")+
  theme(legend.position ="bottom")+
  theme(legend.position ="bottom")+
  theme(axis.title.x = element_text(size =12))+
  theme(axis.title.y = element_text(size =10))+
  theme(legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 14))

ggarrange(a + rremove("xlab"), b , ncol = 1, nrow = 2, common.legend = TRUE, legend ="right", labels = "AUTO",   font.label = list(size = 16), hjust = c(-5, -5))

# Carapace Correlation Global

a<- ggscatter(POMP, x = "Carapace", y = "POM.perc",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Carapace size [mm]", ylab = "POM Consumption [% of initial dry mass]")
# Carapace Correlation Meso vs. Micro

b <- ggscatter(POMP, x = "Carapace", y = "POM.perc", size = 1,
          color = "Tide", palette = "jco",
          facet.by = "Tide", #scales = "free_x", l
          add = "reg.line", conf.int = TRUE, ylab = "POM Consumption (% of initial dry mass)", xlab = "Carapace Size (mm)") +
  stat_cor(aes(color = Tide), method = "pearson", label.y = 1.25)+
  theme(legend.position = "bottom")+
  theme(axis.title.x = element_text(size =12))+
  theme(axis.title.y = element_text(size =12))

ggarrange(a , b + rremove("ylab") , ncol = 2, nrow = 1, common.legend = FALSE, legend ="bottom", widths = c(2, 1.6), heights = c(2, 0.1))

```

Statistics 

```{r, warning = FALSE}
# Ancova POM Consumption
POM.Model <- lm(POM.diff ~ Tide * POM.before , data = POM)
anova(POM.Model)
summary(POM.Model)

# Assumptions 
# Linearity
ggscatter(
  POMP, x = "POM.before", y = "POM.diff",
  color = "Tide", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Tide)
  )
# Homogeneity of Regression Slopes 
POMP %>% anova_test(POM.diff ~ Tide*POM.before)
# Normality of Residuals 
model <- lm(POM.diff ~ POM.before + Tide, data = POMP)
# Inspect the model diagnostic metrics
model.metrics3 <- augment(model)
model.metrics3
head(model.metrics3, 3)
shapiro_test(model.metrics3$.resid)
# Homogeneity of Variances 
model.metrics3 %>% levene_test(.resid ~ Tide)
#     Visual Inspectations
plot(POM.Model, add.smooth = FALSE, which = 1)
plot(POM.Model, which = 2)
plot(POM.Model, add.smooth = FALSE, which = 3)
gg_diagnose(POM.Model)
#     Outliers
model.metrics3 %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()
```

## 06_CO2 Flux 

Visualization

```{r, warning = FALSE}
ggplot(FLUX, aes(x = Tide, y = mmol.Flux, fill = Crab)) +
  scale_y_continuous(name="CO2 Flux (mmol CO2 / m s)", labels = scales::comma, breaks=c(0, 2.5, 5, 7.5, 10, 12.5))+
  geom_boxplot(outlier.shape = NA) +
  theme_classic()+
  theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black"))+  scale_fill_brewer(breaks=c("A", "P"), 
                    labels=c('Absent', 'Present'),palette="Set2")+
  theme(axis.title.x = element_text(size =12))+
  theme(axis.title.y = element_text(size =12))+
  theme(legend.title = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 12))+
  theme(legend.position ="bottom")
```

Statistics 

```{r, warning=FALSE}
# ANCOVA CO2
CO2.Model <- lm(mmol.Flux ~ POM.before * Tide * Crab, data = FLUX)
anova(CO2.Model)
summary(CO2.Model)

# Assumptions 
# Linearity
ggscatter(
  FLUX, x = "POM.before", y = "mmol.Flux",
  facet.by  = c("Tide", "Crab"), 
  short.panel.labs = FALSE
)+
  stat_smooth(method = "loess", span = 0.9)

# Homogeneity of Regression Slopes 
anova_test(mmol.Flux ~ Crab + Tide + POM.before +  Crab*Tide + Crab*POM.before +
             Tide*POM.before + Crab*Tide*POM.before, data = FLUX)
# Normality of Residuals 
aov_residuals <- residuals(object = CO2.Model )
shapiro.test(aov_residuals)                               ## Normality Violated
# Homogeneity of Variances 
model <- lm(mmol.Flux ~ POM.before + Crab*Tide, data = DOC)
# Inspect the model diagnostic metrics
model.metrics4 <- augment(model)
head(model.metrics4, 3)
res<-CO2.Model$residuals
hist(res, main="Histogram of standardised residuals",xlab="Standardised residuals")
leveneTest(.resid ~ Crab * Tide, data = model.metrics4)  # Homogeneity accepted
#     Visual Inspectations
plot(DOC.Conc.model, add.smooth = FALSE, which = 1)
plot(DOC.Conc.model, which = 2)
plot(DOC.Conc.model, add.smooth = FALSE, which = 3)
gg_diagnose(CO2.Model)

#     Outliers
model.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()

# Normality was violated, try with log transformation

# ANCOVA log CO2
FLUX <- mutate(FLUX, logFLUX = log(mmol.Flux))
logCO2.Model <- lm(logFLUX ~ POM.before * Tide * Crab, data = FLUX)
anova(logCO2.Model)
summary(logCO2.Model)

# Assumptions 
# Linearity
ggscatter(
  FLUX, x = "POM.before", y = "logFLUX",
  facet.by  = c("Tide", "Crab"), 
  short.panel.labs = FALSE
)+
  stat_smooth(method = "loess", span = 0.9)

# Homogeneity of Regression Slopes 
anova_test(logFLUX ~ Crab + Tide + POM.before +  Crab*Tide + Crab*POM.before +
             Tide*POM.before + Crab*Tide*POM.before, data = FLUX)
# Normality of Residuals 
aov_residuals <- residuals(object = logCO2.Model )
shapiro.test(aov_residuals)                               ## Normality accepted
# Homogeneity of Variances 
model <- lm(logFLUX ~ POM.before + Crab*Tide, data = FLUX)
# Inspect the model diagnostic metrics
model.metrics5 <- augment(model)
head(model.metrics5, 3)
res<-CO2.Model$residuals
hist(res, main="Histogram of standardised residuals",xlab="Standardised residuals")
leveneTest(.resid ~ Crab * Tide, data = model.metrics5)  # Homogeneity accepted
#     Visual Inspectations
plot(logCO2.Model, add.smooth = FALSE, which = 1)
plot(logCO2.Model, which = 2)
plot(logCO2.Model, add.smooth = FALSE, which = 3)
gg_diagnose(logCO2.Model)

#     Outliers
model.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()

# log Transformation solves issues of non-normality of the residuals, 
# however, the p-value of crabs changes dramatically from 0.02 to 0.06
# therefore, another try with sqrt-transformation

# ANCOVA sqrt CO2
FLUX <- mutate(FLUX, sqrtFLUX = sqrt(mmol.Flux))
sqrtCO2.Model <- lm(sqrtFLUX ~ POM.before * Tide * Crab, data = FLUX)
anova(sqrtCO2.Model) # similar p value
summary(sqrtCO2.Model)

# Assumptions 
# Linearity
ggscatter(
  FLUX, x = "POM.before", y = "sqrtFLUX",
  facet.by  = c("Tide", "Crab"), 
  short.panel.labs = FALSE
)+
  stat_smooth(method = "loess", span = 0.9)

# Homogeneity of Regression Slopes 
anova_test(sqrtFLUX ~ Crab + Tide + POM.before +  Crab*Tide + Crab*POM.before +
             Tide*POM.before + Crab*Tide*POM.before, data = FLUX)
# Normality of Residuals 
aov_residuals <- residuals(object = sqrtCO2.Model )
shapiro.test(aov_residuals)                               ## Normality accepted
# Homogeneity of Variances 
model <- lm(sqrtFLUX ~ POM.before + Crab*Tide, data = FLUX)
# Inspect the model diagnostic metrics
model.metrics6 <- augment(model)
head(model.metrics6, 3)
res<-sqrtCO2.Model$residuals
hist(res, main="Histogram of standardised residuals",xlab="Standardised residuals")
leveneTest(.resid ~ Crab * Tide, data = model.metrics6)  # Homogeneity accepted
#     Visual Inspectations
plot(sqrtCO2.Model, add.smooth = FALSE, which = 1)
plot(sqrtCO2.Model, which = 2)
plot(sqrtCO2.Model, add.smooth = FALSE, which = 3)
gg_diagnose(sqrtCO2.Model)

#     Outliers
model.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()
## sqrt transformation, best fit, similar p-values and all assumptions accepted

```

## 07_Leaching

Visualization

```{r, warning=FALSE}

# DOC Leaching Rate 

ggplot(Leaching, aes(x = Tide, y = Leaching.Rate, fill = T)) +
  scale_y_continuous(name="Leaching Rate [µmol/g/day]", labels = scales::comma, breaks=c(0, 50, 100, 150, 200))+
  geom_boxplot(outlier.shape = NA) +
  theme_classic()+
  theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black"))+  scale_fill_brewer(breaks=c("1", "2"), 
                                                                                                                      labels=c('1', '2'),palette="Set2")+
  theme(axis.title.x = element_text(size =12))+
  theme(axis.title.y = element_text(size =12))+
  theme(legend.position ="bottom")

```

Statistics 

```{r, warning=FALSE}
# ANOVA Leaching Rate CO2
Leach.Model <- lm(Leaching.Rate ~ Tide + T, data = Leaching)
anova(Leach.Model) # similar p value
summary(Leach.Model)
str(Leaching)
# Normality of Residuals 
aov_residuals <- residuals(object = Leach.Model )
shapiro.test(aov_residuals)                               ## Normality accepted
# Homogeneity of Variances 
leveneTest(Leaching.Rate ~ Tide * T, data = Leaching)     ## Homogeneity accepted
# Visual Inspectations
plot(Leach.Model, add.smooth = FALSE, which = 1)
plot(Leach.Model, which = 2)
plot(Leach.Model, add.smooth = FALSE, which = 3)
gg_diagnose(Leach.Model)
################################################################################
# done, time for a beer
```




