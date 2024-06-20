################################ Soil Carbon, Nitrogen, C/N, SOM ##############################################

##Load packages
library(tidyverse) 
library(janitor) 
library(patchwork)
library(performance)
library(emmeans)
library(multcompView)
library(multcomp)
library(ARTool)
library (readxl)
library(ggpubr)
library(lmerTest)

#Import data

df_data <- readxl::read_xlsx("C:/Users/Greenhouse/Desktop/Data - R/C & N Greenhouse/C over N R/Carbon/Soil Data Behnaz - C&N.xlsx") %>% 
  clean_names() %>% 
  mutate(treatment = str_remove(sample, pattern= "^C-" )) %>% 
  mutate(treatment = str_remove(treatment, pattern= "-(H|L)$" )) %>% 
  mutate(treatment = as.factor(treatment),
         compsot = as.factor(compsot)) %>%
  mutate(c_n = c/n)
str(df_data)
print(df_data)


#Data exploration
boxplot(df_data$c ~ df_data$compsot)
boxplot(df_data$c ~ df_data$treatment*df_data$ratio*df_data$compsot)


mod1 <- art(c ~ treatment*compsot,
            data = df_data)

anova(mod1)

marginal <-  art.con(mod1, "treatment")
marginal1 <-  art.con(mod1, "compsot")
marginal2 <-  art.con(mod1, "treatment:compsot")

marginal2 %>% as.data.frame() %>% 
  arrange(p.value)

### mean values
df_data %>% 
  group_by(treatment,compsot) %>% 
  summarise(avgC = mean(c),
            avgN = mean(n),
            avgCNratio = mean(c/n),
            sq_rt = sqrt(avgCNratio))

#### Nitrogen
mod2 <- art(n ~ treatment*compsot,
            data = df_data)
anova(mod2)

marginal2 <-  art.con(mod2, "treatment")
marginal21 <-  art.con(mod2, "compsot")
marginal22 <-  art.con(mod2, "treatment:compsot")

#### carbon to nitrogen
mod3 <- art(c_n ~ treatment*compsot,
            data = df_data)
anova(mod3)

marginal3 <-  art.con(mod3, "treatment")
marginal31 <-  art.con(mod3, "compsot")
marginal32 <-  art.con(mod3, "treatment:compsot")

############################## PLOT Carbon #################################


c1 <- ggplot(df_data, aes(x = compsot, y = c )) +
  # create a boxplot with specified settings
  geom_boxplot(color="black", fill="#999999", alpha=1) +
  ylim(NA, 35)+
  labs(x = "Compost",
       y = ("Total carbon %")) +
  geom_bracket(xmin = "No C",
               xmax = "With C",
               y.position = 29,
               label = "***",
               label.size=12,
               size=1)+
  theme_bw() + 
  theme(axis.title = element_text(size = 20, face = "bold"),
        legend.position = "none",
        axis.text = element_text(size = 20, color = "black", face = "bold"))+
  scale_x_discrete(breaks=c("No C","With C"),
                   labels=c("No", "Yes"))+
  labs(title = "Fixed effect - Compost")+
  theme(
    plot.title = element_text(color="black", size=20, face="bold.italic"))
c1



c2 <- df_data %>% 
  filter(compsot == "With C") %>% 
  ggplot(., aes(x = treatment, y = c)) +
  geom_boxplot(color= "black", fill= c("#E8824D", "#1FCBD9", "#E0123C", "#3B874C"), alpha= 1)+
  geom_bracket(xmin = "L",
               xmax = "Z",
               y.position = 27,
               label = "**",
               label.size=12,
               size=1) +
  geom_bracket(xmin = "Z",
               xmax = "ZL",
               y.position = 29,
               label = "**",
               label.size=12,
               size=1) +
  labs(x = "Amendment treatment",
       y = ("\n\n\n\nTotal carbon %")) +
  ylim(NA , 35)+
  theme_bw()+
  theme(axis.title = element_text(size = 20, face = "bold"),
        legend.position = "none" ,
        axis.text = element_text(size = 20, color = "black"),
        text=element_text(size=20, face = "bold"))+
  labs(title = "Fixed effect - Amendments")+
  theme(
    plot.title = element_text(color="black", size=20, face="bold.italic"))
c2


carbon <- c1 + c2 +  plot_layout(guides = 'collect')

ggsave(carbon, filename = file.path("C:/Users/Greenhouse/Desktop/TC.jpg"),
       width = 14, height = 6, units = "in", dpi = 400)


########################### plot Nitrogen ####################################

n1 <- ggplot(df_data, aes(x = compsot, y = n )) +
  geom_boxplot(color="black", fill="#999999", alpha=1) +
  ylim(NA, 2)+
  labs(x = "Compost",
       y = ("Total nitrogen %")) +
  geom_bracket(xmin = "No C",
               xmax = "With C",
               y.position = 1.5,
               label = "***",
               label.size = 12,
               size = 1 )+
  theme_bw() + 
  theme(axis.title = element_text(size = 20, face = "bold"),
        legend.position = "none",
        axis.text = element_text(size = 20, color = "black", face = "bold"))+
  scale_x_discrete(breaks=c("No C","With C"),
                   labels=c("No", "Yes"))+
  labs(title = "Fixed effect - Compost")+
  theme(
    plot.title = element_text(color="black", size=20, face="bold.italic"))



n2 <- df_data %>% 
  ggplot(., aes(x = treatment, y = n)) +
  geom_boxplot(color= "black", fill= c("#E8824D", "#1FCBD9", "#E0123C", "#3B874C"), alpha= 1)+
  geom_bracket(xmin = "L",
               xmax = "Z",
               y.position = 1.5,
               label = "**",
               label.size = 12,
               size = 1 ) +
  labs(x = "Amendment treatment",
       y = ("\n\n\n\nTotal nitrogen %")) +
  ylim(NA , 2)+
  theme_bw()+
  theme(axis.title = element_text(size = 20, face = "bold"),
        legend.position = "none" ,
        axis.text = element_text(size = 20, color = "black", face = "bold"))+
  labs(title = "Fixed effect - Amendments")+
  theme(
    plot.title = element_text(color="black", size=20, face="bold.italic"))


nitrogen <- n1 + n2 +  plot_layout(guides = 'collect')

ggsave(nitrogen, filename = file.path("C:/Users/Greenhouse/Desktop/TN.jpg"),
       width = 14, height = 6, units = "in", dpi = 400)

#################################### Plot CN ##########################

cn1 <- ggplot(df_data, aes(x = compsot, y = c_n )) +
  # create a boxplot with specified settings
  geom_boxplot(color="black", fill="#999999", alpha=1) +
  ylim(NA, 35)+
  labs(x = "Compost",
       y = ("C/N ratio")) +
  geom_bracket(xmin = "No C",
               xmax = "With C",
               y.position = 32,
               label = "***",
               label.size = 12,
               size = 1 )+
  theme_bw() + 
  theme(axis.title = element_text(size = 20, face = "bold"),
        legend.position = "none",
        axis.text = element_text(size = 20, color = "black", face = "bold"))+
  scale_x_discrete(breaks=c("No C","With C"),
                   labels=c("No", "Yes"))+
  labs(title = "Fixed effect - Compost")+
  theme(
    plot.title = element_text(color="black", size=20, face="bold.italic"))
cn1



cn2 <- df_data %>% 
  filter(compsot == "With C") %>% 
  ggplot(., aes(x = treatment, y = c_n)) +
  geom_boxplot(color= "black", fill= c("#E8824D", "#1FCBD9", "#E0123C", "#3B874C"), alpha= 1)+
  geom_bracket(xmin = "L",
               xmax = "ZL",
               y.position = 34,
               label = "**",
               label.size = 12,
               size = 1) +
  
  geom_bracket(xmin = "Z",
               xmax = "ZL",
               y.position = 31,
               label = "**",
               label.size = 12,
               size = 1) +
  labs(x = "Amendment treatment",
       y = ("\n\n\n\nC/N ratio")) +
  ylim(NA , 36)+
  theme_bw()+
  theme(axis.title = element_text(size = 20, face = "bold"),
        legend.position = "none" ,
        axis.text = element_text(size = 20, color = "black"),
        text=element_text(size=20, face = "bold"))+
  labs(title = "Interaction effect - Amendments * Compost")+
  theme(
    plot.title = element_text(color="black", size=20, face="bold.italic"))+
  scale_x_discrete( breaks = c( "Control", "L", "Z", "ZL"),
                    labels = c("CControl", "CL", "CZ", "CZL"))
cn2


cn3 <- cn1 + cn2 +  plot_layout(guides = 'collect')

ggsave(cn3, filename = file.path("C:/Users/Greenhouse/Desktop/cCCng1.jpg"),
       width = 16, height = 6, units = "in", dpi = 400)

####################################### SOM ###############################
#Open directory first

df_data <- read_excel("C:/Users/Greenhouse/Desktop/Data - R/Organic Matter/Soil OM -Greenhouse (1).xlsx") %>%
  mutate(amendment_treatment = as.factor(amendment_treatment),
         compost = as.factor(compost))%>%
  clean_names()

mod01 <- art(organic_matter_percentage ~ amendment_treatment*compost, data = df_data) 
anova(mod01)

marginal4 <-  art.con(mod01, "amendment_treatment")
marginal41 <-  art.con(mod01, "compost")
marginal42 <-  art.con(mod01, "amendment_treatment:compost")%>%
  as.data.frame() %>% 
  filter(`p.value` < 0.05)

###### mean values
df_data %>% 
  group_by(compost, amendment_treatment) %>% 
  summarise(avgom = mean(organic_matter_percentage))


df_data %>% 
  group_by(amendment_treatment) %>% 
  summarise(avgom = mean(organic_matter_percentage))

df_data %>% 
  group_by(compost) %>% 
  summarise(avgom = mean(organic_matter_percentage))

########################################Plot SOM##############################


sm1 <- ggplot(df_data, aes(x = compost, y = organic_matter_percentage )) +
  geom_boxplot(color="black", fill="#999999", alpha=1) +
  ylim(NA, 75)+
  labs(x = "Compost",
       y = ("SOM %")) +
  geom_bracket(xmin = "No C",
               xmax = "With C",
               y.position = 68,
               label = "***",
               label.size =12,
               size=1)+
  theme_bw() + 
  theme(axis.title = element_text(size = 20, face = "bold"),
        legend.position = "none",
        axis.text = element_text(size = 20, color = "black", face = "bold"))+
  scale_x_discrete(breaks=c("No C","With C"),
                   labels=c("No", "Yes"))+
  labs(title = "Fixed effect - Compost")+
  theme(
    plot.title = element_text(color="black", size=20, face="bold.italic"))

sm1

sm2 <- df_data %>% 
  #filter(compost == "With C") %>% 
  ggplot(., aes(x = amendment_treatment, y = organic_matter_percentage)) +
  geom_boxplot(color= "black", fill= c("#E8824D", "#1FCBD9", "#E0123C", "#3B874C"), alpha= 1)+
  geom_bracket(xmin = "L",
               xmax = "Z",
               y.position = 67,
               label = "**",
               label.size =12,
               size=1)+
  labs(x = "Amendment treatment",
       y = ("\n\n\n\nSOM %")) +
  ylim(NA , 75)+
  theme_bw()+
  theme(axis.title = element_text(size = 20, face = "bold"),
        legend.position = "none" ,
        axis.text = element_text(size = 20, color = "black", face = "bold"))+
  labs(title = "Fixed effect - Amendments")+
  theme(
    plot.title = element_text(color="black", size=20, face="bold.italic"))
sm2



OMG <- sm1 + sm2 +  plot_layout(guides = 'collect')

ggsave(OMG, filename = file.path("C:/Users/Greenhouse/Desktop/OMG.jpg"),
       width = 14, height = 6, units = "in", dpi = 1200)

############################## BIOMASS #######################################

library(tidyverse)
library(janitor)   
library(nlme)      
library(lmerTest)  
library(ggpubr)    
library(performance)
library(rstatix)
library(ggpubr)
library(patchwork)

#Import data and adjust names
df_data <- readxl::read_xlsx("C:/Users/Greenhouse/Desktop/Data - R/Biomass - SUM- greenhouse/Soil Data Behnaz - SUM1.xlsx") %>% 
  clean_names() %>% 
  mutate(treatment = str_remove(sample, pattern= "^C-" )) %>% 
  mutate(treatment = str_remove(treatment, pattern= "-(H|L)$" )) %>% 
  mutate(row = str_extract(rows, pattern = "^(R|L)"))

print(df_data)

######## Total Biomass
#Data exploration
boxplot(df_data$total_biomass ~ df_data$compsot)
boxplot(df_data$total_biomass ~ df_data$treatment*df_data$ratio*df_data$compsot)

mod <- (lm(sqrt(total_biomass) ~ treatment*compsot*ratio,
           data = df_data))
mod6<- lm(sqrt(total_biomass) ~ treatment*compsot,
          data = df_data)

AIC(mod, mod6) 

anova(mod)

hist(resid(mod)) 
plot(mod) 
shapiro.test(resid(mod)) 

df_data %>%
  levene_test(sqrt(total_biomass) ~ treatment*compsot*ratio)


######### Shoot Biomass
mod0 <- (lm(sqrt(biomass_shoot_mg) ~ treatment*compsot*ratio,
            data = df_data))
mod5 <- (lm(sqrt(biomass_shoot_mg) ~ treatment*compsot,
            data = df_data))

AIC (mod0, mod5)
anova(mod0)


hist(resid(mod0)) 
plot(mod0) 
shapiro.test(resid(mod0)) 

df_data %>%
  levene_test(sqrt(biomass_shoot_mg) ~ treatment*compsot*ratio)


######### Root Biomass
mod01 <- (lm(sqrt(biomass_root_mg) ~ treatment*compsot*ratio,
           data = df_data))

mod7 <- (lm(sqrt(biomass_root_mg) ~ treatment*compsot,
           data = df_data))

AIC (mod01, mod7)
anova(mod7)


hist(resid(mod01)) 
plot(mod01) 
shapiro.test(resid(mod01)) 

df_data %>%
  levene_test(sqrt(biomass_root_mg) ~ treatment*compsot*ratio)

################ mean values ###############
df_data %>% 
  group_by(compsot, treatment) %>% 
  summarise(avgtotal = mean(total_biomass),
            shoot = mean(biomass_shoot_mg),
            avgroot = mean(biomass_root_mg))


df_data %>% 
  group_by(treatment) %>% 
  summarise(avgtotal = mean(total_biomass),
            shoot = mean(biomass_shoot_mg),
            avgroot = mean(biomass_root_mg))

df_data %>% 
  group_by(compsot) %>% 
  summarise(avgtotal = mean(total_biomass),
            shoot = mean(biomass_shoot_mg),
            avgroot = mean(biomass_root_mg))

#############################################################

################################# main plot##############################
b1 <- ggplot(df_data, aes(x = compsot, y = total_g )) +
  geom_boxplot(color="black", fill="#999999", alpha=1) +
  ylim(NA, 3)+
  labs(x = "Compost",
       y = ("Total biomass (g)")) +
  geom_bracket(xmin = "No C",
               xmax = "With C",
               y.position = 2.35,
               label = "***",
               label.size= 12,
               size = 1) +
  theme_bw() + 
  theme(axis.title = element_text(size = 20, face = "bold"),
        legend.position = "none",
        axis.text = element_text(size = 20, color = "black", face = "bold"))+
  scale_x_discrete(breaks=c("No C","With C"),
                   labels=c("No", "Yes"))+
  labs(title = "Fixed effect - Compost")+
  theme(
    plot.title = element_text(color="black", size=20, face="bold.italic"))

b1


b2 <- df_data %>% 
  ggplot(., aes(x = treatment, y = total_g)) +
  geom_boxplot(color= "black", fill= c("#E8824D", "#1FCBD9", "#E0123C", "#3B874C"), alpha= 1)+
  labs(x = "Amendment treatment",
       y = ("\n\n\n\nTotal biomass (g)")) +
  ylim(NA , 3)+
  theme_bw()+
  theme(axis.title = element_text(size = 20, face = "bold"),
        legend.position = "none" ,
        axis.text = element_text(size = 20, color = "black"),
        text=element_text(size=20, face = "bold"))+
  labs(title = "Interaction effect - Amendments * Compost")+
  theme(
    plot.title = element_text(color="black", size=20, face="bold.italic"))+
  scale_x_discrete( breaks = c( "Control", "L", "Z", "ZL"),
                    labels = c("CControl", "CL", "CZ", "CZL"))
b2


b3 <- b1 + b2 +  plot_layout(guides = 'collect')

ggsave(b3, filename = file.path("C:/Users/Greenhouse/Desktop/CCTB2.jpg"),
       width = 14, height = 6, units = "in", dpi = 400)

ggsave(b3, filename = file.path("C:/Users/Greenhouse/Desktop/presentTB2.jpg"),
       width = 14, height = 4, units = "in", dpi = 1200)

################################################ SHOOT #######################

bs1 <- ggplot(df_data, aes(x = compsot, y = shoot_g )) +
  # create a boxplot with specified settings
  geom_boxplot(color="black", fill="#999999", alpha=1) +
  ylim(NA, 3)+
  
  # scale_fill_manual(name= "Compost",
  #                   values=c("#999999","#E69F00" ),
  #                   labels=c("No", "Yes"))+
  labs(x = "Compost",
       y = ("Shoot biomass (g)")) +
  geom_bracket(xmin = "No C",
               xmax = "With C",
               y.position = 2.35,
               label = "***",
               label.size = 12,
               size= 1)+
  # customize the plot theme
  theme_bw() + 
  theme(axis.title = element_text(size = 20, face = "bold"),
        legend.position = "none",
        axis.text = element_text(size = 20, color = "black", face = "bold"))+
  # axis.text.x=element_blank(),
  # axis.ticks.x=element_blank())
  scale_x_discrete(breaks=c("No C","With C"),
                   labels=c("No", "Yes"))+
  labs(title = "Fixed effect - Compost")+
  theme(
    plot.title = element_text(color="black", size=20, face="bold.italic"))


bs1



bs2 <- df_data %>% 
  #filter(compsot == "With C") %>% 
  ggplot(., aes(x = treatment, y = shoot_g)) +
  geom_boxplot(color= "black", fill= c("#E8824D", "#1FCBD9", "#E0123C", "#3B874C"), alpha= 1)+
  labs(x = "Amendment treatment",
       y = ("\n\n\n\nShoot biomass (g)")) +
  ylim(NA , 3)+
  theme_bw()+
  theme(axis.title = element_text(size = 20, face = "bold"),
        legend.position = "none" ,
        axis.text = element_text(size = 20, color = "black"),
        text=element_text(size=20, face = "bold"))+
  labs(title = "Interaction effect - Amendments * Compost")+
  theme(
    plot.title = element_text(color="black", size=20, face="bold.italic"))+
  scale_x_discrete( breaks = c( "Control", "L", "Z", "ZL"),
                    labels = c("CControl", "CL", "CZ", "CZL"))
bs2


bs3 <- bs1 + bs2 +  plot_layout(guides = 'collect')

ggsave(bs3, filename = file.path("C:/Users/Greenhouse/Desktop/CTsh.jpg"),
       width = 14, height =4 , units = "in", dpi = 400)


############################# root ######################################

br1 <- ggplot(df_data, aes(x = compsot, y = root_g )) +

  geom_boxplot(color="black", fill="#999999", alpha=1) +
  ylim(NA, 3)+
  labs(x = "Compost",
       y = ("Root biomass (g)")) +
  theme_bw() + 
  theme(axis.title = element_text(size = 20, face = "bold"),
        legend.position = "none",
        axis.text = element_text(size = 20, color = "black", face = "bold"))+

  scale_x_discrete(breaks=c("No C","With C"),
                   labels=c("No", "Yes"))+
  labs(title = "Fixed effect - Compost")+
  theme(
    plot.title = element_text(color="black", size=20, face="bold.italic"))

br1



br2 <- df_data %>% 

  ggplot(., aes(x = treatment, y = root_g)) +
  geom_boxplot(color= "black", fill= c("#E8824D", "#1FCBD9", "#E0123C", "#3B874C"), alpha= 1)+
  labs(x = "Amendment treatment",
       y = ("\n\n\n\nRoot biomass (g)")) +
  ylim(NA , 3)+
  theme_bw()+
  theme(axis.title = element_text(size = 20, face = "bold"),
        legend.position = "none" ,
        axis.text = element_text(size = 20, color = "black"),
        text=element_text(size= 20, face = "bold"))+
  labs(title = "Interaction effect - Amendments * Compost")+
  theme(
    plot.title = element_text(color="black", size=20, face="bold.italic"))+
  scale_x_discrete( breaks = c( "Control", "L", "Z", "ZL"),
                    labels = c("CControl", "CL", "CZ", "CZL"))

br2


br3 <- br1 + br2 +  plot_layout(guides = 'collect')

ggsave(br3, filename = file.path("C:/Users/Greenhouse/Desktop/CTRg.jpg"),
       width = 14, height = 4, units = "in", dpi = 400)


######################################## ROOT TO SHOOT ####################

library(tidyverse)
library(janitor)  
library(ggpubr)    
library(patchwork)

df_data <- readxl::read_xlsx("C:/Users/Greenhouse/Desktop/Data - R/1.Greenhouse R/New Analysis biomass- carbon/Soil Data Behnaz - C&N OLD.xlsx") %>% 
  clean_names() %>% 
  mutate(amendment_treatment = str_remove(amendment_treatment, pattern= "^C-" )) %>% 
  mutate(amendment_treatment = str_remove(amendment_treatment, pattern= "-(H|L)$" ),
         amendment_treatment = gsub(amendment_treatment,  pattern = "C$", replacement = "Control"))


df_cn <- df_data %>% 
  mutate(cn_ratio = c/n,
         root_shoot = biomass_root_mg/biomass_shoot_mg)

##########

m2 <- lm(log(root_shoot) ~ amendment_treatment* compsot*ratio, 
         df_cn)
m02 <- lm(log(root_shoot) ~ amendment_treatment* compsot, 
         df_cn)

AIC (m2, m02)
anova(m02)
summary(m02)
shapiro.test(resid(m02))

library(emmeans)
posthoc <- emmeans(m02, list(pairwise ~ amendment_treatment*compsot)) %>% 
  multcomp::cld(Letter = LETTERS)


plot(posthoc)+ theme_bw()

#### mean value
df_data %>% 
  group_by(compsot) %>% 
  summarise(avgC = mean(root_shoot))

################################## Compost main plot #####################

df_data <- readxl::read_xlsx("C:/Users/Greenhouse/Desktop/Data - R/Biomass - SUM- greenhouse/Soil Data Behnaz - SUM1.xlsx") %>% 
  clean_names() %>% 
  mutate(treatment = str_remove(sample, pattern= "^C-" )) %>% 
  mutate(treatment = str_remove(treatment, pattern= "-(H|L)$" )) %>% 
  mutate(row = str_extract(rows, pattern = "^(R|L)"))%>%
  mutate(root_shoot = root_g/shoot_g)



p1 <- ggplot(df_data, aes(x = compsot, y = root_shoot))+
  geom_hline(yintercept = 1, linetype = "dotdash")+
  geom_boxplot(aes( fill = compsot), show.legend = F)+
  geom_jitter() +
  scale_fill_manual(values=c("#999999","#EDDE3E" )) +
  theme_bw()+
  labs(x = "Compost",
       y = ("Root/shoot biomass)") )+
  theme(axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20, color = "black", face = "bold"))+
  scale_y_continuous(name= "Root/shoot biomass", 1, limits= c(-5,10))+
  scale_x_discrete(breaks=c("No C","With C"),
                   labels=c("No", "Yes"))
p1

p2 <- ggplot(df_data, aes(x = root_g, y = shoot_g, shape = compsot, fill = compsot))+
  geom_abline(intercept = 0, linetype = "dotdash")+
  geom_point(size = 4)+
  guides(shape = guide_legend(override.aes = list(size = 5)))+
  scale_fill_manual(name= "Compost",
                    values=c("#999999","#EDDE3E" ),
                    labels=c("No", "Yes")) +
  scale_shape_manual(name= "Compost",
                     values = c(23,24),
                     labels=c("No", "Yes"))+
  theme_bw()+
  labs(x = "Root biomass (g)",
       y = ("\n\n\n\nShoot biomass (g)") )+
  theme(axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20, color = "black", face = "bold"),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20, face = "bold"),
        legend.spacing.y = unit(0.5, 'cm')) +
  guides(fill = guide_legend(byrow = TRUE))


p2 

rs <- p1 + p2 +  plot_layout(guides = 'collect')

ggsave(rs, filename = file.path("C:/Users/Greenhouse/Desktop/rs.jpg"),
       width = 14, height = 6, units = "in", dpi = 1200)

########################################### Amendments Plot##################

df_data <- readxl::read_xlsx("C:/Users/Greenhouse/Desktop/Data - R/Biomass - SUM- greenhouse/Soil Data Behnaz - SUM1.xlsx") %>% 
  clean_names() %>% 
  mutate(treatment = str_remove(sample, pattern= "^C-" )) %>% 
  mutate(treatment = str_remove(treatment, pattern= "-(H|L)$" )) %>% 
  mutate(row = str_extract(rows, pattern = "^(R|L)"))%>%
  mutate(root_shoot = root_g/shoot_g)

df_data %>% 
  group_by(treatment) %>% 
  summarise(avgC = mean(root_shoot))

supp.labs <- c("No compost", "With compost")
names(supp.labs)<- c("No C", "With C")


comp1 <- list(c("Control", "L"),
              c("Control", "Z"),
              c("Control", "ZL"))

p1 <- ggplot(df_cn, aes( x = amendment_treatment,  y = log(root_shoot)))+
  facet_grid(~ compsot, labeller =labeller(compsot= supp.labs))+
geom_hline(yintercept = 1, linetype = "dotdash")+
  geom_boxplot(aes( fill = amendment_treatment), show.legend = F)+
  geom_jitter() +
  geom_pwc(label = "p.signif",
           p.adjust.method = "none",
           hide.ns = TRUE,
           label.size= 8, 
           size = 0.7,
           bracket.nudge.y = 0.05,
           step.increase = 0.12,
           vjust = 0.6) +
  scale_fill_manual(values=c("#E8824D", "#1FCBD9", "#E0123C", "#3B874C")) +
  theme_bw()+
  labs(x = "Amendment treatment",
       y = ("Root/shoot biomass)") )+
  theme(axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 16, color = "black", face = "bold"),
        strip.text = element_text(face = "bold", size = 14))+
  scale_y_continuous(name= "Root/shoot biomass", 1)

p1

ggsave(p1, filename = file.path("C:/Users/Greenhouse/Desktop/amendment rootshoot.jpg"),
       width = 12, height = 6, units = "in", dpi = 1200)


