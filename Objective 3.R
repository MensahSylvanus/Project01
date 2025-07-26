data <- read.csv("fireplotdata.csv")
names(data)

all_variables<-names(data[c(7:23)])

# Standardize predictors
data <- data %>%
  mutate(across(all_of(all_variables), ~scale(.)[,1]))


### simple linear mixed-effect model testing the effects of fire frequency on aboveground carbon (AGC)

AGC_fire_mod<-lme(AGC~Fire_frequency, random= ~1|Plot_size, data)
summary(AGC_fire_mod)

Longlatdist <- as.matrix(dist(cbind(data$Longitude, data$Latitude)))
Longlatdist.inv <- 1/Longlatdist
diag(Longlatdist.inv) <- 0

AGC_fire_mod_ac<-update(AGC_fire_mod,correlation = corExp(form = ~ Longitude+Latitude))
AGC_fire_mod.resid<-as.data.frame(AGC_fire_mod$residuals)
AGC_fire_mod_ac.resid<-as.data.frame(AGC_fire_mod_ac$residuals)

library(ape)
AGC_fire_mod.Moran<-Moran.I(AGC_fire_mod.resid$fixed, Longlatdist.inv);AGC_fire_mod.Moran
AGC_fire_mod_ac.Moran<-Moran.I(AGC_fire_mod_ac.resid$fixed, Longlatdist.inv);AGC_fire_mod_ac.Moran

shapiro.test(AGC_fire_mod.resid$fixed)
shapiro.test(AGC_fire_mod_ac.resid$fixed)
anova(AGC_fire_mod,AGC_fire_mod_ac)### Table S2
summary(AGC_fire_mod)### Table S2
summary(AGC_fire_mod_ac)### Table S2
r.squaredGLMM(AGC_fire_mod)
r.squaredGLMM(AGC_fire_mod_ac)



##### Decision tree

data <- read.csv("fireplotdata.csv")
#theme_set(theme_bw())
set_theme(theme_sjplot2(base_size = 12),axis.title.color = "black",
          axis.tickslen = 0, # hides tick marks
          axis.title.size = 0.9,
          axis.textsize = 0.9,
          legend.size = 1.1,
          legend.title.size = 1.1,plot.margin=unit(c(0,0,0.1,0), "cm"),
          geom.label.size = 3.5)
a<- ggplot(data,  aes(x=Fire_frequency, y=AGC))+geom_point(size=2, color="#00AFBB")+
  geom_smooth(method="lm", size=1,linetype=1, se=T,color = "#FC4E07")+
 #   geom_quantile(quantiles = 0.99, size=1, colour = "black",linetype=1)+
  labs(x="Fire frequency", y= "AGC (Mg/ha)"); a


library(ggparty)
data$`Fire frequency` <- data$Fire_frequency
tr_tree <- lmtree(AGC~ `Fire frequency`, data, caseweights = T)

treeplot1<-ggparty(tr_tree , terminal_space = 0.4, horizontal = F) +
  geom_edge() +
  geom_node_splitvar() +
  geom_edge_label() +
  geom_node_plot(
    gglist = list(geom_boxplot(aes(y=AGC, x=""),position=position_dodge(), color = "#FC4E07",fill="lightgrey",outlier.shape = NA),
                  geom_jitter(aes(y = AGC, x = ""), width = 0.1, size = 1.5, color = "#00AFBB", alpha = 0.8),
                  theme_bw()+theme(plot.margin = margin(t = -40, r = 10, b = -40, l = 0)),
                  ylab("AGC (Mg/ha)"), xlab("")),
    ids = "terminal", # not necessary since default
    shared_axis_labels = T);treeplot1


plot_grid(a,treeplot1, ncol=2, labels =c("a","b"), align="hv")


extract_model_stats <- function(model) {
  r2 <- r.squaredGLMM(model)[1]
  p <- summary(model)$tTable["Fire_frequency", "p-value"]
  label <- paste0("R² = ", round(r2 * 100, 2), "%\np = ", format.pval(p, digits = 1, eps = .001))
  return(label)
}

label_text <- extract_model_stats(AGC_fire_mod);label_text

grid.text(label_text, x=0.40, y=0.90, gp=gpar(cex = 0.8, font = 4, col = "darkblue"))# Fig 2



######## structural equation model (SEM)


# To avoid constructing the SEM with redundant and less important variables,
#1# we only focused on the most important climatic variables that affect fire (Supplementary Method 1 and Table S3),
#2# as well as diversity and structural attributes that influenced AGC (see Supplementary Method 2 and Tables S4-S7 for more information).

# Supplementary Method 1: Assessing climate - fire relationship 
data <- read.csv("fireplotdata.csv")
names(data)

#checking for overdispersion
Fire_clim_mod0<-glmer(Fire_frequency~scale(rainfall)+scale(rainfall_seasonality)+scale(temperature)+(1|Plot_size), family=poisson, data)
summary(Fire_clim_mod0)
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type = "pearson")
  sum(rp^2) / rdf
}
overdisp_fun(Fire_clim_mod0)### [1] 4.266107 showing overdispersion
library(performance)
check_overdispersion(Fire_clim_mod0)
        # Overdispersion test

        #dispersion ratio =   4.266
        #Pearson's Chi-Squared = 328.490
        #                p-value = < 0.001

        #Overdispersion detected.
#dealing with overdispersion
Fire_clim_mod1<-glmer.nb(Fire_frequency~scale(rainfall)+scale(rainfall_seasonality)+scale(temperature) +  (1|Plot_size), data)
summary(Fire_clim_mod1)

library(remotes)
#install_github("nyiuab/NBZIMM", force=T, build_vignettes=F)
library(NBZIMM)
Fire_clim_mod2<-NBZIMM::glmm.nb(Fire_frequency~scale(rainfall)+scale(rainfall_seasonality)+scale(temperature), rand=~1|Plot_size, data)
summary(Fire_clim_mod2)

Fire_clim_mod.ac<-update(Fire_clim_mod1,correlation = corExp(form = ~ Longitude+Latitude))## Error in lme4::glmer(formula = Fire_frequency ~ scale(rainfall) + scale(rainfall_seasonality) +  : 
                                                                                            ##unused argument (correlation = corExp(form = ~Longitude + Latitude))
Fire_clim_mod.ac<-update(Fire_clim_mod2,correlation = corExp(form = ~ Longitude+Latitude))

Fire_clim_mod2.resid<-as.data.frame(Fire_clim_mod2$residuals)
Fire_clim_mod.ac.resid<-as.data.frame(Fire_clim_mod.ac$residuals)

Moran.I(Fire_clim_mod2.resid$fixed, Longlatdist.inv)
Moran.I(Fire_clim_mod.ac.resid$fixed, Longlatdist.inv)

summary(Fire_clim_mod1) ############ Table S3
car::vif(Fire_clim_mod1)############ Table S3
r.squaredGLMM(Fire_clim_mod1)

theme_set(theme_sjplot2(base_size = 12))
a<-plot_model(Fire_clim_mod1, type='pred',show.data = F, dot.size=1.5, mdrt.values="meansd",
              terms=c("rainfall"),axis.title = c("Mean annual rainfall (mm)","Fire frequency"),title="",colors = "gs",
              legend.title = "", line.size = 1, axis.lim=c(0,16));a

b<-plot_model(Fire_clim_mod1, type='pred',show.data = F, dot.size=1.5, mdrt.values="meansd",
              terms=c("rainfall_seasonality"),axis.title = c("Rainfall seasonality (%)",""),title="",colors = "gs",
              legend.title = "", line.size = 1);b

c<-plot_model(Fire_clim_mod1, type='pred',show.data = F, dot.size=1.5, mdrt.values="meansd",
              terms=c("temperature"),axis.title = c("Mean annual temperature (°C)",""),title="",colors = "gs",
              legend.title = "", line.size = 1, axis.lim=c(0,16));c

cowplot::plot_grid(a,b,c,ncol=3, labels ="auto", align="hv", label_y = 0.9, label_x=0.01,label_size = 10)
grid.text("p = 0.706", x=0.25, y=0.850, gp=gpar(cex=0.8, font=4,col = "darkblue"))# Fig 3
grid.text("p = 0.006", x=0.60, y=0.850, gp=gpar(cex=0.8, font=4,col = "darkblue"))# Fig 3
grid.text("p = 0.980", x=0.90, y=0.850, gp=gpar(cex=0.8, font=4,col = "darkblue"))# Fig 3

dev.off()


# Supplementary Method 2: diversity and structural attributes influence on AGC (see Supplementary Method 2 and Tables S4-S7 for more information)

### Exploring correlation between  variables

data <- read.csv("fireplotdata.csv")
names(data)
selected<-c("Fire_frequency","rainfall" ,"rainfall_seasonality" ,"temperature" ,"Total_N" ,"clay_silt" ,  "Species_richness", "FDis" ,"sesMNTD", "sesMPD","cvDBH", "skwDBH",
            "Tree_density","LargeTrees_density","RemTrees_density", "HLorey" )
selected<-data.frame(data[selected])
names(selected)<-c("Fire_freq","rainfall" ,"rainfall_seas" ,"temp" , "Total_N" ,"clay_silt" ,  "Spe_rich", "FDis" ,"sesMNTD", "sesMPD","cvDBH", "skwDBH",
                   "Tree_density","LT_density","RemT_density", "HLorey" )
library(GGally)
ggcorr(selected,high = "#3B9AB2",
       mid = "#EEEEEE",
       low = "#F21A00",
       label = TRUE,label_round = 2,legend.position = "",
       label_size = 5,
       color = "black")### Fig S6

dev.off()



#################################################################################################################################################
#########   Based on correlation matrix between the predictors (Figure S6), we excluded overall tree density and density of                      
#########   95% remaining trees because they were highly correlated (r=1), and were also strongly correlated with species richness (r >0.8).
#########   In addition, because sesMPD and sesMNTD were also strongly correlated, we excluded one variable (sesMPD).                           
#########   For climate, we used rainfall seasonality only because it stood as the most important variable driving fire frequency               
#########   (see Supplementary Method 1 and Table S3). With the remaining candidate variables, we performed a mixed-effect model to determine 
########     how AGC related with fire, climate, soil (physical and chemical properties), species diversity 
########     (phylogenetic, taxonomic and functional trait attributes) and stand structural attributes
################################################################################################################################################

###  Performing the mixed-effect model with AGC as the response

data <- read.csv("fireplotdata.csv")
nrow(data)
data<-na.omit(data);nrow(data);# removing plots (4) with low species richness, where sesMNTD could not be computed

all_variables<-names(data[c(7:23)])
# Standardize predictors
data <- data %>%
  mutate(across(all_of(all_variables), ~scale(.)[,1]))
names(data)

AGC_mod<-lme(AGC~Fire_frequency+rainfall_seasonality+clay_silt+Total_N+Species_richness+FDis+sesMNTD+cvDBH+skwDBH+LargeTrees_density,
           random= list (~1|Country, ~1|Plot_size), data)
AGC_mod.ac<-update(AGC_mod,correlation = corExp(form = ~ Longitude+Latitude))
AGC_mod.resid<-as.data.frame(AGC_mod$residuals)
AGC_mod.ac.resid<-as.data.frame(AGC_mod.ac$residuals)

Longlatdist <- as.matrix(dist(cbind(data$Longitude, data$Latitude)))
Longlatdist.inv <- 1/Longlatdist
diag(Longlatdist.inv) <- 0

Moran.I(AGC_mod.resid$fixed, Longlatdist.inv)
Moran.I(AGC_mod.ac.resid$fixed, Longlatdist.inv)
shapiro.test(AGC_mod.resid$fixed)
shapiro.test(AGC_mod.ac.resid$fixed)
anova(AGC_mod,AGC_mod.ac)# Table S4
summary(AGC_mod) # Table S5
#summary(AGC_mod.ac)
cbind(car::vif(AGC_mod))
r.squaredGLMM(AGC_mod)

library(MuMIn)
AGC_mod_mumin <- MuMIn::dredge(AGC_mod,beta="sd",trace = T, evaluate = TRUE, rank = "AICc")

summary(model.avg(AGC_mod_mumin, fit = TRUE))
final.mod<-model.avg(AGC_mod_mumin, fit = TRUE, delta<2)
summary(final.mod)  # Table S6 
final.mod<-get.models(AGC_mod_mumin, 3)[[1]]
summary(final.mod)
r.squaredGLMM(final.mod)
cbind(car::vif(final.mod))


### Plotting RVI (Figure 4)
cbind(sw(AGC_mod_mumin))
db.RVI <- data.frame(cbind(sw(AGC_mod_mumin)))

db.RVI$Variables<-row.names(db.RVI)
db.RVI$Model <-  c("Structure",'Diversity','Diversity',"Climate","Disturbance",'Diversity','Diversity',"Soil","Soil",'Diversity')
colnames(db.RVI)<-c("RVI","Variables","Model")

db.RVI$Variables <- factor(db.RVI$Variables,levels = c
                           ("rainfall_seasonality","Fire_frequency",
                             "clay_silt","Total_N",
                             "Species_richness","sesMNTD", 'FDis',"skwDBH","cvDBH",
                             "LargeTrees_density"), ordered=F)
db.RVI$Model <- factor(db.RVI$Model,levels = 
                         c("Climate","Disturbance","Soil", 'Diversity',"Structure"))

db.RVI$rel.sum<-db.RVI$RVI/sum(db.RVI$RVI)

RVI_plot<-ggplot(data = db.RVI, aes(x=Variables, y=rel.sum)) + 
  geom_bar(width=0.8,stat="identity",position=position_dodge(width=0.9),aes(fill=Variables))+
  xlab("")+ylab("Relative sum of weights")+ 
  #geom_text(aes(label=round(RVI,digits = 2)), hjust=-0.2, angle=90)+
  theme(legend.title = element_blank())+
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
  theme(legend.key.size = unit(0.55, 'cm'),legend.text= element_text(size = 9));RVI_plot


#### Piecewise SEM

data <- read.csv("fireplotdata.csv")
data<-na.omit(data)# removing plots (4) with low species richness, where sesMNTD could not be computed
data$LargeTrees_density2<-data$LargeTrees_density*data$Plot_size# convertng LargeTrees_density back to absolute density (to remove plot size effect) because plot size used as random later 

var_to_scale<-c("Fire_frequency","LargeTrees_density2","Species_richness", "rainfall_seasonality","sesMNTD","AGC")

# Standardize predictors
data <- data %>%
  mutate(across(all_of(var_to_scale), ~scale(.)[,1]))

names(data)

m0<-lmerTest::lmer(Fire_frequency~rainfall_seasonality+ (1|Plot_size), data)
summary(m0)

m1<-lmerTest::lmer(LargeTrees_density2~Fire_frequency+rainfall_seasonality+(1|Plot_size), data)
summary(m1)

m2<-lmerTest::lmer(Species_richness~Fire_frequency+rainfall_seasonality+(1|Plot_size), data)
summary(m2)

m3<-lmerTest::lmer(sesMNTD~Fire_frequency+rainfall_seasonality+ (1|Plot_size), data)
summary(m3)

m4<-lmerTest::lmer(AGC~Fire_frequency+rainfall_seasonality+LargeTrees_density2+Species_richness+sesMNTD+(1|Plot_size), data)
summary(m4)

library(piecewiseSEM)

# Create list of structural equations
sem0 <- psem(m0,m1,m2,m3,m4)
summary(sem0,conserve = T, .progressBar = F,data=data)
sem <- update(sem0, sesMNTD %~~% Species_richness)
summary(sem,conserve = T, .progressBar = F)
sem_summary <- summary(sem, conserve = TRUE, .progressBar = FALSE)

# Extract path coefficients table
coefs <- sem_summary$coefficients

dir.create("sem_outputs")
# Save to CSV
write.csv(coefs, "sem_outputs/sem_coefficients.csv", row.names = FALSE)



