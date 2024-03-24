### Run analysis on ANOVAs

install.packages("https://github.com/awalker89/openxlsx")
library(openxlsx)
library("lme4", lib.loc="~/R/win-library/3.3")
library(lme4)
library(lsmeans)
library(data.table)
library(lmerTest)

PH60 <- read.xlsx("TraitsAveraged2.xlsx", sheet = "PH60w_outNA's")
PH30 <- read.xlsx("TraitsAveraged2.xlsx", sheet = "PH30w_outNA's")
PH15 <- read.xlsx("TraitsAveraged2.xlsx", sheet = "PH15w_outNA's")
DTF <- read.xlsx("TraitsAveraged2.xlsx", sheet = "DTFw_outNA's")
DTG <- read.xlsx("TraitsAveraged2.xlsx", sheet = "DTGw_outNA's")
NT <- read.xlsx("TraitsAveraged2.xlsx", sheet = "NTw_outNA's")
PV <- read.xlsx("PollenViability.xlsx", sheet = "ForAnalysis")
AGB <- read.xlsx("above_ground_biomass.xlsx", sheet = "ForAnalysis")
Con <- read.xlsx("ConductanceForAnalysis.xlsx", sheet = "ConForAnalysis")
SW <- read.xlsx("ConductanceForAnalysis.xlsx", sheet = "SWForAnalysis")

# Remove El Grullo from Analysis
DTG_rmG <- DTG[!(DTG$Pop=="G"), ]
AGB_rmG <- AGB[!(AGB$Pop=="G"), ]
Con_rmG <- Con[!(Con$Pop=="G"), ]
SW_rmG <- SW[!(SW$Pop=="G"), ]
PV_rmG <- PV[!(PV$Pop=="G"), ]

AGB_nG <- read.xlsx("Test_AGB.xlsx", sheet = "AGB_nG")
all_AGB_wG <- read.xlsx("Test_AGB.xlsx", sheet = "AGB_wG")

Con_NoNAG <- Con_rmG[!(Con_rmG$Pop=="NA"), ]

# ANOVA
#Population fixed effect
#Sampling Site and Block randomized
#Sampling site nested within population
#Population nested within block
# lmer(PH15 ~ Pop + (1 | Pop:SS) + (1 | Block:Pop), data=PH15)

PH60_lmer <- lmer(PH60 ~ Pop + (1 | Pop:SS) + (1 | Block:Pop), data = PH60) 
aov_PH60 <- anova(PH60_lmer)
summary(aov_PH60)
summary(PH60_lmer)
aov_PH60

write.table(aov_PH60, "aov_PH60_rmG.txt", sep="\t")

PH30_lmer <- lmer(PH30 ~ Pop + (1 | Pop:SS) + (1 | Block:Pop), data = PH30) 
aov_PH30_rmG <- anova(PH30_lmer)

write.table(aov_PH30_rmG, "aov_PH30_rmG.txt", sep="\t")

PH15_lmer <- lmer(PH15 ~ Pop + (1 | Pop:SS) + (1 | Block:Pop), data = PH15) 
anova(PH15_lmer)

aov_PH15_rmG <- anova(PH15_lmer)

write.table(aov_PH15_rmG, "aov_PH15_rmG.txt", sep="\t")

#DTF_lmer <- lmer(DTF ~ Pop + (1 | Pop:SS) + (1 | Block:Pop), data = DTF) 
#anova(DTF_lmer)

## Transformed DTF
DTF_lmerT <- lmer((1/DTF) ~ Pop + (1 | Pop:SS) + (1 | Block:Pop), data = DTF) 
anova(DTF_lmerT)

aov_DTF_rmG <- anova(DTF_lmerT)
write.table(aov_DTF_rmG, "aov_DTF_rmG.txt", sep="\t")

#DTG_lmer <- lmer((DTG) ~ Pop + (1 | Pop:SS) + (1 | Block:Pop), data = DTG_rmG) 
#anova(DTG_lmer)

## Transformed DTG

DTG_lmerT <- lmer((1/DTG) ~ Pop + (1 | Pop:SS) + (1 | Block:Pop), data = DTG_rmG) 
anova(DTG_lmerT)

aov_DTG_rmG <- anova(DTG_lmerT)
write.table(aov_DTG_rmG, "aov_DTG_rmG.txt", sep="\t")

NT_lmer <- lmer(NT ~ Pop + (1 | Pop:SS) + (1 | Block:Pop), data = NT) 
anova(NT_lmer)

aov_NT_rmG <- anova(NT_lmer)
write.table(aov_NT_rmG, "aov_NT_rmG.txt", sep="\t")

PV_lmer <- lmer(PV ~ Pop + (1 | Pop:SS) + (1 | Block:Pop), data = PV_rmG) 
anova(PV_lmer)

PV_lmer2 <- lmer(PV ~ Pop + (1 | SS) + (1 | Block:Pop), data=PV_rmG)
anova(PV_lmer2)
aov_PV_rmG <- anova(PV_lmer2)
write.table(aov_PV_rmG, "aov_PV_rmG.txt", sep="\t")

#AGB_lmer <- lmer(Mass ~ Pop + (1 | Pop:SS) +  (1 | Block:Pop), data=AGB_rmG)
#anova(AGB_lmer)

## Tranformed AGB
AGB_lmerT <- lmer((sqrt(Mass)) ~ Pop + (1 | Pop:SS) +  (1 | Block:Pop), data=AGB_rmG)
anova(AGB_lmerT)

aov_AGB_rmG <- anova(AGB_lmerT)
write.table(aov_AGB_rmG, "aov_AGB_rmG.txt", sep="\t")

#Con_lmer <- lmer(CON ~ Pop + (1 | Pop:SS) + (1 | Block:Pop), data=Con_rmG)
#anova(Con_lmer)

## Transformed Con

Con_lmerT <- lmer((sqrt(CON)) ~ Pop + (1 | Pop:SS) + (1 | Block:Pop), data=Con_rmG)
anova(Con_lmerT)
aov_Con_rmG <- anova(Con_lmerT)
write.table(aov_Con_rmG, "aov_Con_rmG.txt", sep="\t")

SW_lmer <- lmer(SW ~ Pop + (1 | Pop:Sample) + (1 | Block:Pop), data=SW_rmG)
anova(SW_lmer)
aov_SW_rmG <- anova(SW_lmer)
write.table(aov_SW_rmG, "aov_SW_rmG.txt", sep="\t")

## Data Visualization

install.packages("https://CRAN.R-project.org/package=devtools")
library(devtools)
devtools::install_github("goodekat/ggResidpanel")
library(ggResidpanel)

resid_panel(resid(PH15_lmer), fitted(PH15_lmer), bins = 20)
resid_panel(resid(PH30_lmer), fitted(PH30_lmer), bins = 20)
resid_panel(resid(PH60_lmer), fitted(PH60_lmer), bins = 20) #Maybe transform this one, looks good for the most part
resid_panel(resid(DTF_lmer), fitted(DTF_lmer), bins = 20) #Transform
resid_panel(resid(DTG_lmer), fitted(DTG_lmer), bins = 20) #Definitely transform this one
resid_panel(resid(NT_lmer), fitted(NT_lmer), bins = 20)
resid_panel(resid(PV_lmer2), fitted(PV_lmer2), bins = 20)
resid_panel(resid(AGB_lmer), fitted(AGB_lmer), bins = 20) #transform
resid_panel(resid(Con_lmer), fitted(Con_lmer), bins = 20) #maybe?
resid_panel(resid(SW_lmer), fitted(SW_lmer), bins = 20)

# Check transformed resids
resid_panel(resid(DTF_lmerT), fitted(DTF_lmer), bins = 20) 
resid_panel(resid(DTG_lmerT), fitted(DTG_lmer), bins = 20)
resid_panel(resid(AGB_lmerT), fitted(AGB_lmer), bins = 20) 
resid_panel(resid(Con_lmerT), fitted(Con_lmer), bins = 20)

# lsmeans

#lsmeansLT(model, test.effs = NULL, ddf="Satterthwaite")

lsmeansLT(PH15_lmer, test.effs = NULL, ddf="Satterthwaite")
lsmeansLT(PH30_lmer, test.effs = NULL, ddf="Satterthwaite")
lsmeansLT(PH60_lmer, test.effs = NULL, ddf="Satterthwaite")
lsmeansLT(DTF_lmerT, test.effs = NULL, ddf="Satterthwaite")
lsmeansLT(DTG_lmerT, test.effs = NULL, ddf="Satterthwaite")
lsmeansLT(NT_lmer, test.effs = NULL, ddf="Satterthwaite")
lsmeansLT(PV_lmer2, test.effs = NULL, ddf="Satterthwaite")
lsmeansLT(AGB_lmerT, test.effs = NULL, ddf="Satterthwaite")
lsmeansLT(Con_lmerT, test.effs = NULL, ddf="Satterthwaite")
lsmeansLT(SW_lmer, test.effs = NULL, ddf="Satterthwaite")

library(ggplot2)
library(ggridges)

#Box plots

fill <- "#4271AE"
line <- "#1F3552"

ggplot(PH60, aes(x = Pop, y = PH60)) + geom_boxplot() + ylab("Plant height") + xlab("Population") + ggtitle("Plant height at 60 days") + geom_boxplot(fill = c( "green4", "olivedrab3", "darkorange2", "firebrick2" ), colour = line)
ggplot(PH30, aes(x = Pop, y = PH30)) + geom_boxplot() + ylab("Plant height") + xlab("Population") + ggtitle("Plant height at 30 days") + geom_boxplot(fill = c( "green4", "olivedrab3", "darkorange2", "firebrick2" ), colour = line)
ggplot(PH15, aes(x = Pop, y = PH15)) + geom_boxplot() + ylab("Plant height") + xlab("Population") + ggtitle("Plant height at 15 days") + geom_boxplot(fill = c( "green4", "olivedrab3", "darkorange2", "firebrick2" ), colour = line)

ggplot(DTG_rmG, aes(x = Pop, y = DTG)) + geom_boxplot() + ylab("Days") + xlab("Population") + geom_boxplot(fill = c( "green4", "olivedrab3", "darkorange2", "firebrick2" ), colour = line)
ggplot(PV_rmG, aes(x = Pop, y = PV)) + geom_boxplot() + ylab("Viability") + xlab("Population") + geom_boxplot(fill = c( "green4", "olivedrab3", "darkorange2", "firebrick2" ), colour = line)
ggplot(AGB_rmG, aes(x = Pop, y = AGB_rmG$Mass)) + geom_boxplot() + ylab("Mass (kg)") + xlab("Population") + geom_boxplot(fill = c( "green4", "olivedrab3", "darkorange2", "firebrick2" ), colour = line)
ggplot(SW_rmG, aes(x = Pop, y = SW)) + geom_boxplot() + ylab("Mass (g)") + xlab("Population") + geom_boxplot(fill = c( "green4", "olivedrab3", "darkorange2", "firebrick2" ), colour = line)

ggplot(PH15, aes(x = Pop, y = PH15)) + geom_boxplot() + ylab("Plant height") + xlab("Population") + ggtitle("Plant height at 15 days") + geom_boxplot(fill = c( "skyblue", "steelblue2", "royalblue1", "royalblue3" ), colour = line)
ggplot(DTF, aes(x = Pop, y = DTF)) + geom_boxplot() + ylab("Days") + xlab("Population") + ggtitle("Days to Flowering") + geom_boxplot(fill = fill, colour = line)
ggplot(DTG, aes(x = Pop, y = DTG)) + geom_boxplot() + ylab("Days") + xlab("Population") + ggtitle("Days to Germination") + geom_boxplot(fill = fill, colour = line)
ggplot(NT, aes(x = Pop, y = NT)) + geom_boxplot() + ylab("Tillers") + ggtitle("Number of Tillers") + geom_boxplot(fill = fill, colour = line)
ggplot(PV, aes(x = Pop, y = PV)) + geom_boxplot() + ylab("Pollen Viability") + xlab("Population") + ggtitle("Pollen Viability") + geom_boxplot(fill = fill, colour = line)
ggplot(AGB, aes(x = Pop, y = Mass)) + geom_boxplot() + ylab("AGB (kg)") + xlab("Population") + ggtitle("Total Above Ground Biomass") + geom_boxplot(fill = fill, colour = line)
ggplot(Con, aes(x = Pop, y = CON)) + geom_boxplot() + ylab("Conductance") + xlab("Population") + ggtitle("Stomatal Conductance") + geom_boxplot(fill = c("skyblue","skyblue","skyblue", "royalblue1","royalblue1"), colour = line) + theme_bw()
ggplot(SW, aes(x = Pop, y = SW)) + geom_boxplot() + ylab("Weight (g)") + xlab("Population") + ggtitle("Seed Weight") + geom_boxplot(fill = fill, colour = line)

# Ridgeline "#4040B0", "#9090F0"

ggplot(PH15_wG, aes(x = Pop, y = PH15)) + geom_boxplot() + ylab("Weight (g)") + xlab("Population") + ggtitle("Seed Weight") + geom_boxplot(fill = fill, colour = line)

ggplot(PH15, aes(x = PH15, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0), labels = NULL) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Plant Height at 15 Days') + xlab("Plant Height (cm)") + ylab(NULL) + scale_fill_discrete(labels = c("EJUA", "EJUB", "MSA", "SLO"))

ggplot(PH30, aes(x = PH30, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0)) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Plant Height at 30 Days') + xlab("Plant Height (cm)") + ylab("Population") + scale_fill_cyclical(values = c("skyblue", "steelblue2", "royalblue1", "royalblue3"))
ggplot(PH60, aes(x = PH60, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0)) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Plant Height at 60 Days') + xlab("Plant Height (cm)") + ylab("Population") + scale_fill_cyclical(values = c("skyblue", "steelblue2", "royalblue1", "royalblue3"))
ggplot(DTF, aes(x = DTF, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0)) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Days until Flowering') + xlab("Days") + ylab("Population") + scale_fill_cyclical(values = c("skyblue", "steelblue2", "royalblue1", "royalblue3"))
ggplot(DTG, aes(x = DTG, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0)) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Number of Days until Germination') + xlab("Days") + ylab("Population") + scale_fill_cyclical(values = c("skyblue", "steelblue2", "royalblue1", "royalblue3"))
ggplot(NT, aes(x = NT, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0)) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Number of Tillers per Plant') + xlab("Tillers") + ylab("Population") + scale_fill_cyclical(values = c("skyblue", "steelblue2", "royalblue1", "royalblue3"))
ggplot(PV, aes(x = PV, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0)) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Pollen Viability') + xlab("PV") + ylab("Population") + scale_fill_cyclical(values = c("skyblue", "steelblue2", "royalblue1", "royalblue3"))
ggplot(AGB_rmG, aes(x = Mass, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0)) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Total Above Ground Biomass') + xlab("Mass (kg)") + ylab("Population") + scale_fill_cyclical(values = c("skyblue", "steelblue2", "royalblue1", "royalblue3"))
ggplot(Con_rmG, aes(x = CON, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0)) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Stomatal Conductance') + xlab("Stomatal Conductance") + ylab("Population") + scale_fill_cyclical(values = c("skyblue", "steelblue2", "royalblue1", "royalblue3"))
SW_R <- ggplot(SW_rmG, aes(x = SW, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0)) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Seed Weight') + xlab("Mass (g)") + ylab("Population") + scale_fill_cyclical(values = c("skyblue", "steelblue2", "royalblue1", "royalblue3"))
ggplot(Con, aes(x = CON, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0)) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Stomatal Conductance') + xlab("Stomatal Conductance") + ylab("Population") + scale_fill_cyclical(values = c("skyblue", "steelblue2", "royalblue1", "royalblue3"))

## Manuscript Figures

ggplot(PH15, aes(x = PH15, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0), labels = NULL) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Plant Height at 15 Days') + xlab("Plant Height (cm)") + ylab(NULL) + scale_fill_discrete(labels = c("EJUA", "EJUB", "MSA", "SLO"))

ggplot(PH30, aes(x = PH30, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0), labels = NULL) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Plant Height at 30 Days') + xlab("Plant Height (cm)") + ylab(NULL) + scale_fill_discrete(labels = c("EJUA", "EJUB", "MSA", "SLO"))
ggplot(PH60, aes(x = PH60, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0), labels = NULL) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Plant Height at 60 Days') + xlab("Plant Height (cm)") + ylab(NULL) + scale_fill_discrete(labels = c("EJUA", "EJUB", "MSA", "SLO"))
ggplot(DTF, aes(x = DTF, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0), labels = NULL) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Days until Flowering') + xlab("Days") + ylab(NULL) + scale_fill_discrete(labels = c("EJUA", "EJUB", "MSA", "SLO"))
ggplot(DTG_rmG, aes(x = DTG, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0), labels = NULL) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Number of Days until Germination') + xlab("Days") + ylab(NULL) + scale_fill_discrete(labels = c("EJUA", "EJUB", "MSA", "SLO"))
ggplot(NT, aes(x = NT, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0), labels = NULL) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Number of Tillers per Plant') + xlab("Tillers") + ylab(NULL) + scale_fill_discrete(labels = c("EJUA", "EJUB", "MSA", "SLO"))
ggplot(PV_rmG, aes(x = PV, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0), labels = NULL) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Pollen Viability') + xlab("PV") + ylab(NULL) + scale_fill_discrete(labels = c("EJUA", "EJUB", "MSA", "SLO"))
ggplot(AGB_rmG, aes(x = Mass, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0), labels = NULL) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Total Above Ground Biomass') + xlab("Mass (kg)") + ylab(NULL) + scale_fill_discrete(labels = c("EJUA", "EJUB", "MSA", "SLO"))
ggplot(Con_rmG, aes(x = CON, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0), labels = NULL) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Stomatal Conductance') + xlab("Stomatal Conductance") + ylab(NULL) + scale_fill_discrete(labels = c("EJUA", "EJUB", "MSA", "SLO"))
ggplot(SW_rmG, aes(x = SW, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0), labels = NULL) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Seed Weight') + xlab("Mass (g)") + ylab(NULL) + scale_fill_discrete(labels = c("EJUA", "EJUB", "MSA", "SLO"))
ggplot(Con, aes(x = CON, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0), labels = NULL) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Stomatal Conductance') + xlab("Stomatal Conductance") + ylab(NULL) + scale_fill_discrete(labels = c("EJUA", "EJUB", "MSA", "SLO"))

library(cowplot)
DTF_plot <- ggplot(DTF, aes(x = DTF, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0), labels = NULL) + scale_x_continuous(expand = c(0.01, 0))  +  xlab("Days to Flowering") + ylab(NULL) + theme_bw() + theme(legend.position = "none")# + scale_fill_discrete(labels = c("EJUA", "EJUB", "MSA", "SLO"))
Con_plot <- ggplot(Con_rmG, aes(x = CON, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0), labels = NULL) + scale_x_continuous(expand = c(0.01, 0)) + xlab("Stomatal Conductance") + ylab(NULL) + scale_fill_discrete(labels = c("EJUA", "EJUB", "MSA", "SLO"), name = "Population") + theme_bw()
AGB_plot <- ggplot(AGB_rmG, aes(x = Mass, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0), labels = NULL) + scale_x_continuous(expand = c(0.01, 0)) + xlab("Total Aboveground Biomass (kg)") + ylab("Population") + theme_bw() + theme(legend.position = "none")# + ylab(NULL) + scale_fill_discrete(labels = c("EJUA", "EJUB", "MSA", "SLO"))

plot_grid(AGB_plot, DTF_plot, Con_plot, labels = "AUTO", cols = 3)

## Matt Tenure
MattTenureDTF <- ggplot(DTF, aes(x = DTF, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0), labels=c("A" = "Ejutla A", "B" = "Ejutla B", "M" = "La Mesa", "S" = "San Lorenzo")) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Days until Flowering') + xlab("Days") + ylab("Population") + scale_fill_cyclical(values = c("gray19", "firebrick3")) 
MattTenureDTF

ggplot(Con_rmG, aes(x = CON, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0), labels=c("A" = "Ejutla A", "B" = "Ejutla B", "M" = "La Mesa", "S" = "San Lorenzo")) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Stomatal Conductance') + xlab("Conductance") + ylab("Population") + scale_fill_cyclical(values = c("gray19", "firebrick3"))



ggplot(DTF, aes(x = DTF, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0)) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Number of Days until Flowering') + xlab("Number of Days") + ylab("Population") + scale_fill_cyclical(values = c("green4", "olivedrab3", "darkorange2", "firebrick2"))
ggplot(DTG, aes(x = DTG, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0)) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Number of Days until Germination') + xlab("Number of Days") + ylab("Population") + scale_fill_cyclical(values = c("green4", "olivedrab3", "darkorange2", "firebrick2"))
ggplot(NT, aes(x = NT, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0)) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Number of Tillers per Plant') + xlab("Number of Tillers") + ylab("Population") + scale_fill_cyclical(values = c("green4", "olivedrab3", "darkorange2", "firebrick2"))
ggplot(PV, aes(x = PV, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0)) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Pollen Viability') + xlab("PV") + ylab("Population") + scale_fill_cyclical(values = c("skyblue", "steelblue2", "royalblue1", "royalblue3"))
ggplot(AGB_rmG, aes(x = Mass, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0)) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Total Above Ground Biomass') + xlab("Mass (kg)") + ylab("Population") + scale_fill_cyclical(values = c("skyblue", "steelblue2", "royalblue1", "royalblue3"))
ggplot(Con_rmG, aes(x = CON, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0)) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Stomatal Conductance') + xlab("Conductance") + ylab("Population") + scale_fill_cyclical(values = c("green4", "olivedrab3", "darkorange2", "firebrick2"))
ggplot(SW_rmG, aes(x = SW, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0)) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Seed Weight') + xlab("Mass (g)") + ylab("Population") + scale_fill_cyclical(values = c("green4", "olivedrab3", "darkorange2", "firebrick2"))
ggplot(Con, aes(x = CON, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0)) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Stomatal Conductance') + xlab("Conductance") + ylab("Population") + scale_fill_cyclical(values = c("skyblue", "steelblue2", "royalblue1", "royalblue3"))

ggplot(all_AGB_wG, aes(x = Mass, y = Pop, fill = Pop)) + geom_density_ridges(rel_min_height = 0.005) + scale_y_discrete(expand = c(0.01, 0)) + scale_x_continuous(expand = c(0.01, 0)) + theme_ridges() + labs(title = 'Total Above Ground Biomass') + xlab("Mass (kg)") + ylab("Population") + scale_fill_cyclical(values = c("skyblue", "steelblue2", "royalblue1", "royalblue3"))



PH15_R
PH30_R
PH60_R
DTF_R
DTG_R
NT_R
PV_R
AGB_R
Con_R
SW_R


##Next step ridgeline with G

# Means of Populations
aggregate(PH15[,4], list(PH15$Pop), mean)
aggregate(PH30[,4], list(PH30$Pop), mean)
aggregate(PH60[,4], list(PH60$Pop), mean)
aggregate(DTF[,4], list(DTF$Pop), mean)
aggregate(DTG[,4], list(DTG$Pop), mean)
aggregate(NT[,4], list(NT$Pop), mean)
aggregate(PV_rmG[,4], list(PV_rmG$Pop), mean)
aggregate(AGB_rmG[,4], list(AGB_rmG$Pop), mean)
aggregate(Con_NoNAG[,5], list(Con_NoNAG$Pop), mean)
aggregate(SW_rmG[,4], list(SW_rmG$Pop), mean)

aggregate(Con_wG[,3], list(Con_wG$Pop), mean)
aggregate(PH60_wG[,3], list(PH60_wG$Pop), mean)
aggregate(AGB_wG[,2], list(AGB_wG$Pop), mean)
aggregate(DTG_wG[,3], list(DTG_wG$Pop), mean)
aggregate(DTF_wG[,3], list(DTF_wG$Pop), mean)

aggregate(NT_wG[,3], list(NT_wG$Pop), mean)
aggregate(AGB[,4], list(AGB$Pop), mean)
avePV_wG <- aggregate(PV[,4], list(PV$Pop), mean)


aggregate(PH15_wG[,3], list(PH15_wG$Pop), mean)
aggregate(PH30_wG[,3], list(PH30_wG$Pop), mean)
## Input data for El Grullo, compare averages for test
## See about having to take the average of all three replicates, or whether it can be left unaveraged...shouldn't change anything, right?

PH60_wG <- read.xlsx("TraitsAveraged2.xlsx", sheet = "PH60_wG")
PH30_wG <- read.xlsx("TraitsAveraged2.xlsx", sheet = "PH30_wG")
PH15_wG <- read.xlsx("TraitsAveraged2.xlsx", sheet = "PH15_wG")
DTF_wG <- read.xlsx("TraitsAveraged2.xlsx", sheet = "DTF_wG")
DTG_wG <- read.xlsx("TraitsAveraged2.xlsx", sheet = "DTG_wG")
NT_wG <- read.xlsx("TraitsAveraged2.xlsx", sheet = "NT_wG")
PV_wG <- read.xlsx("PollenViability.xlsx", sheet = "PV_wG")
AGB_wG <- read.xlsx("above_ground_biomass.xlsx", sheet = "AGB_wG")
Con_wG <- read.xlsx("ConductanceForAnalysis.xlsx", sheet = "Con_wG")
SW_wG <- read.xlsx("ConductanceForAnalysis.xlsx", sheet = "SW_wG")

PV_wG <- read.xlsx("PollenViability2.xlsx", sheet = "PV_wG")
AGB_wG <- read.xlsx("Test_AGB.xlsx", sheet = "Mean_AGB_wG")

# Blocks random, populations fixed
# lmer(PH15 ~ Pop + (1 | Block:Pop), data=PH15_wG)
PH15_wG.lmer <- lmer(PH15 ~ Pop + (1 | Block), data=PH15_wG)
PH30_wG.lmer <- lmer(PH30 ~ Pop + (1 | Block), data=PH30_wG)
PH60_wG.lmer <- lmer(PH60 ~ Pop + (1 | Block), data=PH60_wG)
DTF_wG.lmer <- lmer(DTF ~ Pop + (1 | Block), data=DTF_wG)
DTG_wG.lmer <- lmer(DTG ~ Pop + (1 | Block), data=DTG_wG)
NT_wG.lmer <- lmer(NT ~ Pop + (1 | Block), data=NT_wG)
Con_wG.lmer <- lmer(Con ~ Pop + (1 | Block), data=Con_wG)
PV_wG.lmer <- lmer(PV ~ Pop + (1 | Block), data=PV_wG)
AGB_wG.lmer <- lmer(Mass ~ Pop + (1 | Block), data=AGB_wG)

# Others need to figure out blocks for G????

## RESID PANELS
resid_panel(resid(PH15_wG.lmer), fitted(PH15_wG.lmer), bins = 20)
resid_panel(resid(PH30_wG.lmer), fitted(PH30_wG.lmer), bins = 20)
resid_panel(resid(PH60_wG.lmer), fitted(PH60_wG.lmer), bins = 20)
resid_panel(resid(DTF_wG.lmer), fitted(DTF_wG.lmer), bins = 20) # Transform
resid_panel(resid(DTG_wG.lmer), fitted(DTG_wG.lmer), bins = 20) # Transform
resid_panel(resid(NT_wG.lmer), fitted(NT_wG.lmer), bins = 20) # Transform
resid_panel(resid(Con_wG.lmer), fitted(Con_wG.lmer), bins = 20)
resid_panel(resid(PV_wG.lmer), fitted(PV_wG.lmer), bins = 20)
resid_panel(resid(AGB_wG.lmer), fitted(AGB_wG.lmer), bins = 20)

#TRANSFORMED
DTF_wG.lmerT <- lmer((log(DTF)) ~ Pop + (1 | Block), data=DTF_wG)
DTG_wG.lmerT <- lmer((log(DTG)) ~ Pop + (1 | Block), data=DTG_wG)
NT_wG.lmerT <- lmer((log(NT)) ~ Pop + (1 | Block), data=NT_wG)
AGB_wG.lmerT <- lmer((log(Mass)) ~ Pop + (1 | Block), data=AGB_wG)

# REVIEW RESIDS
resid_panel(resid(DTF_wG.lmerT), fitted(DTF_wG.lmerT), bins = 20) # Transform
resid_panel(resid(DTG_wG.lmerT), fitted(DTG_wG.lmerT), bins = 20) # Transform
resid_panel(resid(NT_wG.lmerT), fitted(NT_wG.lmerT), bins = 20) # Transform
resid_panel(resid(AGB_wG.lmerT), fitted(AGB_wG.lmerT), bins = 20)

#ANOVA
anova(PH15_wG.lmer) #2.198e-07 ***
anova(PH30_wG.lmer) #2.367e-08 ***                  
anova(PH60_wG.lmer) #1.631e-06 ***
anova(DTF_wG.lmerT) #1.257e-13 ***
anova(DTG_wG.lmerT) #0.01078 *
anova(NT_wG.lmer) #7.549e-07 ***
anova(Con_wG.lmer) #1.902e-08 ***
anova(PV_wG.lmer) #0.1021 
anova(AGB_wG.lmer) #1.426e-07 ***

#lsmeans
lsmeansLT(PH15_wG.lmer, test.effs = NULL, ddf="Satterthwaite")
lsmeansLT(PH30_wG.lmer, test.effs = NULL, ddf="Satterthwaite")
lsmeansLT(PH60_wG.lmer, test.effs = NULL, ddf="Satterthwaite")
lsmeansLT(DTG_wG.lmer, test.effs = NULL, ddf="Satterthwaite")
lsmeansLT(DTF_wG.lmer, test.effs = NULL, ddf="Satterthwaite")
lsmeansLT(NT_wG.lmer, test.effs = NULL, ddf="Satterthwaite")
lsmeansLT(Con_wG.lmer, test.effs = NULL, ddf="Satterthwaite")
lsmeansLT(PV_wG.lmer, test.effs = NULL, ddf="Satterthwaite")
lsmeansLT(AGB_wG.lmer, test.effs = NULL, ddf="Satterthwaite")

## Run Levenes Test
str(PH60_wG)
PH60_wG$PH60 <- as.numeric(PH60_wG$PH60)
PH60_wG$Pop <- as.factor(PH60_wG$Pop)
PH60_wG$Block <- as.numeric(PH60_wG$Block)

leveneTest(PH60 ~ Pop, data = PH60_wG) #0.725
leveneTest(PH30 ~ Pop, data = PH30_wG) #0.9937
leveneTest(PH15 ~ Pop, data = PH15_wG) #0.8843
leveneTest(DTF ~ Pop, data = DTF_wG) #0.563
leveneTest(DTG ~ Pop, data = DTG_wG) #0.915
leveneTest(NT ~ Pop, data = NT_wG) #0.7919
leveneTest(Con ~ Pop, data = Con_wG) #0.9471
leveneTest(PV ~ Pop, data = PV_wG) #0.436
leveneTest(Mass ~ Pop, data = AGB_wG) #0.0744

#Tukey Test
#Setup
aovPH15 <- aov(PH15 ~ Pop + (1 | Block), data=PH15_wG)
aovPH30 <- aov(PH30 ~ Pop + (1 | Block), data=PH30_wG)
aovPH60 <- aov(PH60 ~ Pop + (1 | Block), data=PH60_wG)
#aovDTF <- aov(DTF ~ Pop + (1 | Block), data=DTF_wG)
#aovDTG <- aov(DTG ~ Pop + (1 | Block), data=DTG_wG)
#aovNT <- aov(NT ~ Pop + (1 | Block), data=NT_wG)
aovCON <- aov(Con ~ Pop + (1 | Block), data=Con_wG)
aovPV <- aov(PV ~ Pop + (1 | Block), data=PV_wG)
aovMass <- aov(Mass ~ Pop + (1 | Block), data=AGB_wG)
aovDTF <- aov((log(DTF)) ~ Pop + (1 | Block), data=DTF_wG)
aovDTG <- aov((log(DTG)) ~ Pop + (1 | Block), data=DTG_wG)
aovNT<- aov((log(NT)) ~ Pop + (1 | Block), data=NT_wG)

aovPV <- aov(PV ~ Pop + Block, data=PV_wG)
TukeyHSD(aovPV, "Pop")

## Comparison of Traits w/Heterozygosity

AveTraits <- read.xlsx("ForJeff_wG.xlsx")
AveTraits_nG <- read.xlsx("ForJeff_nG.xlsx")

lm(AveTraits_nG$Ave.PH60, AveTraits_nG$Ave.Heterozygosity)



ggplot(data=AveTraits, aes(x=Ave.PH60, y=Ave.Heterozygosity, group=Population)) +
  geom_point()

ggplot(AveTraits, aes(x=Ave.PH60, y=Ave.Heterozygosity, fill=Population)) + geom_dotplot(binaxis='y', stackdir='center') + xlab("Average Plant Height at 60 Days") + ylab("Population Heterozygosity")
ggplot(AveTraits, aes(x=Ave.Con, y=Ave.Heterozygosity, fill=Population)) + geom_dotplot(binaxis='y', stackdir='center') + xlab("Average Stomatal Conductance") + ylab("Population Heterozygosity")
ggplot(AveTraits, aes(x=Ave.AGB, y=Ave.Heterozygosity, fill=Population)) + geom_dotplot(binaxis='y', stackdir='center') + xlab("Average Above Ground Biomass") + ylab("Population Heterozygosity")
ggplot(AveTraits, aes(x=Ave.DTF, y=Ave.Heterozygosity, fill=Population)) + geom_dotplot(binaxis='y', stackdir='center') + xlab("Average Days to Flowering") + ylab("Population Heterozygosity")
ggplot(AveTraits, aes(x=Ave.DTG, y=Ave.Heterozygosity, fill=Population)) + geom_dotplot(binaxis='y', stackdir='center') + xlab("Average Days to Germination") + ylab("Population Heterozygosity")
ggplot(AveTraits, aes(x=Ave.SW, y=Ave.Heterozygosity, fill=Population)) + geom_dotplot(binaxis='y', stackdir='center') + xlab("Average Seed Weight") + ylab("Population Heterozygosity")
ggplot(AveTraits, aes(x=Ave.NT, y=Ave.Heterozygosity, fill=Population)) + geom_dotplot(binaxis='y', stackdir='center') + xlab("Average Number of Tillers") + ylab("Population Heterozygosity")

ggplot(AveTraits, aes(y= as.numeric(Ave.PH60), x= as.numeric(Homozygosity))) + geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + ylab("Average Plant Height at 60 Days (cm)") + xlab("Homozygosity") + geom_point(size=3.5) + theme_bw()+ theme(legend.position="none") + theme(axis.text=element_text(size=14)) + theme(axis.title.x = element_text(size=16)) + theme(axis.title.y = element_text(size=16))
ggplot(AveTraits_nG, aes(y= as.numeric(Ave.PH60), x= as.numeric(Ave.Heterozygosity))) + geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + ylab("Plant Height") + xlab("Genetic Diversity") + geom_point(size=3.5) + theme_bw()+ theme(legend.position="none") + theme(axis.text=element_text(size=14)) + theme(axis.title.x = element_text(size=16)) + theme(axis.title.y = element_text(size=16))

# Scaled average traits compared with heterozygosity to get R2
ScaledAveTraits <- read.xlsx("ScaledTraitAves.xlsx")

ggplot(ScaledAveTraits, aes(y=Ave.PH15, x=Ave.Heterozygosity)) + geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + ylab("PH15") + xlab("Genetic Diversity") + geom_point(size=3.5) + theme_bw() + theme(legend.position="none")
ggplot(ScaledAveTraits, aes(y= as.numeric(Ave.PH30), x= as.numeric(Ave.Heterozygosity))) + geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + ylab("PH30") + xlab("Genetic Diversity") + geom_point(size=3.5) + theme_bw()+ theme(legend.position="none")
ggplot(ScaledAveTraits, aes(y= as.numeric(Ave.PH60), x= as.numeric(Ave.Heterozygosity))) + geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + ylab("PH60") + xlab("Genetic Diversity") + geom_point(size=3.5) + theme_bw()+ theme(legend.position="none") 
ggplot(ScaledAveTraits, aes(y= as.numeric(Ave.Con), x= as.numeric(Ave.Heterozygosity))) + geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + ylab("CON") + xlab("Genetic Diversity") + geom_point(size=3.5) + theme_bw()+ theme(legend.position="none") 
ggplot(ScaledAveTraits, aes(y= as.numeric(Ave.AGB), x= as.numeric(Ave.Heterozygosity))) + geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + ylab("AGB") + xlab("Genetic Diversity") + geom_point(size=3.5) + theme_bw() + theme(legend.position="none") 
ggplot(ScaledAveTraits, aes(y= as.numeric(Ave.DTF), x= as.numeric(Ave.Heterozygosity))) + geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + ylab("DTF") + xlab("Genetic Diversity") + geom_point(size=3.5) + theme_bw()+ theme(legend.position="none") 
ggplot(ScaledAveTraits, aes(y= as.numeric(Ave.DTG), x= as.numeric(Ave.Heterozygosity))) + geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + ylab("DTG") + xlab("Genetic Diversity") + geom_point(size=3.5) + theme_bw()+ theme(legend.position="none") 
ggplot(ScaledAveTraits, aes(y= as.numeric(Ave.SW), x= as.numeric(Ave.Heterozygosity))) + geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + ylab("SW") + xlab("Genetic Diversity") + geom_point(size=3.5) + theme_bw()+ theme(legend.position="none") 
ggplot(ScaledAveTraits, aes(y= as.numeric(Ave.NT), x= as.numeric(Ave.Heterozygosity))) + geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + ylab("NT") + xlab("Genetic Diversity") + geom_point(size=3.5) + theme_bw()+ theme(legend.position="none") 
ggplot(ScaledAveTraits, aes(y= as.numeric(Ave.PV), x= as.numeric(Ave.Heterozygosity))) + geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + ylab("PV") + xlab("Genetic Diversity") + geom_point(size=3.5) + theme_bw()+ theme(legend.position="none") 


ggplot(ScaledAveTraits, aes(y= as.numeric(Ave.AGB), x= as.numeric(Ave.Heterozygosity))) + geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + ylab("AGB") + xlab("Genetic Diversity") + geom_point(size=3.5) + theme_bw() + theme(legend.position="none") 

#For paper

#mylabel = bquote(italic(R)^2 == .(format(summary(PH60.lm)$r.squared, digits = 3)))

ggplot(ScaledAveTraits, aes(y= as.numeric(Ave.PH60), x= as.numeric(Ave.Heterozygosity), color = Population)) + 
  geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + 
  ylab("PH60") + xlab("Genetic Diversity") + 
  geom_point(size=3.5) + 
  theme_bw() + 
  scale_color_manual(values = c("#F8766D","#7CAE00","black","#00BFC4","#C77CFF"), labels = c("EJUA","EJUB","ELG","MSA","SLO")) + 
  annotate(geom = "text", x = 0.25, y = 0.99, label = "R-squared = 0.891")


# Supplmental
plot_PH15<- ggplot(ScaledAveTraits, aes(y= Ave.PH15, x= as.numeric(Ave.Heterozygosity), color = Population)) + 
  geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + 
  ylab("PH15") + xlab("Genetic Diversity") + 
  geom_point(size=3.5) + 
  theme_bw() + 
  scale_color_manual(values = c("#F8766D","#7CAE00","black","#00BFC4","#C77CFF"), labels = c("EJUA","EJUB","ELG","MSA","SLO"))+ theme(legend.position = "none")
plot_PH30 <- ggplot(ScaledAveTraits, aes(y= as.numeric(Ave.PH30), x= as.numeric(Ave.Heterozygosity), color = Population)) + 
  geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + 
  ylab("PH30") + xlab("Genetic Diversity") + 
  geom_point(size=3.5) + 
  theme_bw() + 
  scale_color_manual(values = c("#F8766D","#7CAE00","black","#00BFC4","#C77CFF"), labels = c("EJUA","EJUB","ELG","MSA","SLO"))+ theme(legend.position = "none")
plot_PH60 <- ggplot(ScaledAveTraits, aes(y= as.numeric(Ave.PH60), x= as.numeric(Ave.Heterozygosity), color = Population)) + 
  geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + 
  ylab("PH60") + xlab("Genetic Diversity") + 
  geom_point(size=3.5) + 
  theme_bw() + 
  scale_color_manual(values = c("#F8766D","#7CAE00","black","#00BFC4","#C77CFF"), labels = c("EJUA","EJUB","ELG","MSA","SLO"))+ theme(legend.position = "none")
plot_Con <- ggplot(ScaledAveTraits, aes(y= as.numeric(Ave.Con), x= as.numeric(Ave.Heterozygosity), color = Population)) + 
  geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + 
  ylab("Con") + xlab("Genetic Diversity") + 
  geom_point(size=3.5) + 
  theme_bw() + 
  scale_color_manual(values = c("#F8766D","#7CAE00","black","#00BFC4","#C77CFF"), labels = c("EJUA","EJUB","ELG","MSA","SLO"))+ theme(legend.position = "none")
plot_AGB <- ggplot(ScaledAveTraits, aes(y= as.numeric(Ave.AGB), x= as.numeric(Ave.Heterozygosity), color = Population)) + 
  geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + 
  ylab("AGB") + xlab("Genetic Diversity") + 
  geom_point(size=3.5) + 
  theme_bw() + 
  scale_color_manual(values = c("#F8766D","#7CAE00","black","#00BFC4","#C77CFF"), labels = c("EJUA","EJUB","ELG","MSA","SLO"))+ theme(legend.position = "none")
plot_DTF <- ggplot(ScaledAveTraits, aes(y= as.numeric(Ave.DTF), x= as.numeric(Ave.Heterozygosity), color = Population)) + 
  geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + 
  ylab("DTF") + xlab("Genetic Diversity") + 
  geom_point(size=3.5) + 
  theme_bw() + 
  scale_color_manual(values = c("#F8766D","#7CAE00","black","#00BFC4","#C77CFF"), labels = c("EJUA","EJUB","ELG","MSA","SLO"))+ theme(legend.position = "none")
plot_DTG <- ggplot(ScaledAveTraits, aes(y= as.numeric(Ave.DTG), x= as.numeric(Ave.Heterozygosity), color = Population)) + 
  geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + 
  ylab("DTG") + xlab("Genetic Diversity") + 
  geom_point(size=3.5) + 
  theme_bw() + 
  scale_color_manual(values = c("#F8766D","#7CAE00","black","#00BFC4","#C77CFF"), labels = c("EJUA","EJUB","ELG","MSA","SLO"))+ theme(legend.position = "none")
plot_SW <- ggplot(ScaledAveTraits, aes(y= as.numeric(Ave.SW), x= as.numeric(Ave.Heterozygosity), color = Population)) + 
  geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + 
  ylab("SW") + xlab("Genetic Diversity") + 
  geom_point(size=3.5) + 
  theme_bw() + 
  scale_color_manual(values = c("#F8766D","#7CAE00","black","#00BFC4","#C77CFF"), labels = c("EJUA","EJUB","ELG","MSA","SLO")) + theme(legend.position = "none")
plot_NT <- ggplot(ScaledAveTraits, aes(y= as.numeric(Ave.NT), x= as.numeric(Ave.Heterozygosity), color = Population)) + 
  geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + 
  ylab("NT") + xlab("Genetic Diversity") + 
  geom_point(size=3.5) + 
  theme_bw() + 
  scale_color_manual(values = c("#F8766D","#7CAE00","black","#00BFC4","#C77CFF"), labels = c("EJUA","EJUB","ELG","MSA","SLO")) + theme(legend.position = "none")
plot_PV <- ggplot(ScaledAveTraits, aes(y= as.numeric(Ave.PV), x= as.numeric(Ave.Heterozygosity), color = Population)) + 
  geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + 
  ylab("PV") + xlab("Genetic Diversity") + 
  geom_point(size=3.5) + 
  theme_bw() + 
  scale_color_manual(values = c("#F8766D","#7CAE00","black","#00BFC4","#C77CFF"), labels = c("EJUA","EJUB","ELG","MSA","SLO"))

plot_grid(plot_PH15,plot_PH30,plot_PH60,plot_Con, plot_AGB,plot_DTF,plot_DTG,plot_SW,plot_NT,plot_PV, labels = "AUTO")

#+ theme(legend.position="none") 




#Visual without G
ggplot(ScaledAveTraits, aes(y= as.numeric(Ave.DTF.NG), x= as.numeric(Ave.Heterozygosity))) + geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + ylab("DTF") + xlab("Genetic Diversity") + geom_point(size=3.5) + theme_bw()+ theme(legend.position="none") 
ggplot(ScaledAveTraits, aes(y= as.numeric(Ave.PV.NG), x= as.numeric(Ave.Heterozygosity))) + geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + ylab("PV") + xlab("Genetic Diversity") + geom_point(size=3.5) + theme_bw()+ theme(legend.position="none") 
ggplot(ScaledAveTraits, aes(y= as.numeric(Ave.DTG.NG), x= as.numeric(Ave.Heterozygosity))) + geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + ylab("DTG") + xlab("Genetic Diversity") + geom_point(size=3.5) + theme_bw()+ theme(legend.position="none") 
#ggplot(ScaledAveTraits, aes(y= as.numeric(Ave.AGB.NG), x= as.numeric(Ave.Heterozygosity))) + geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + ylab("DTG") + xlab("Genetic Diversity") + geom_point(size=3.5) + theme_bw()+ theme(legend.position="none") 

plot_DTF_nG <- ggplot(ScaledAveTraits, aes(y= as.numeric(Ave.DTF.NG), x= as.numeric(Ave.Heterozygosity), color = Population)) + 
  geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + 
  ylab("DTF") + xlab("Genetic Diversity") + 
  geom_point(size=3.5) + 
  theme_bw() + 
  scale_color_manual(values = c("#F8766D","#7CAE00","black","#00BFC4","#C77CFF"), labels = c("EJUA","EJUB","ELG","MSA","SLO")) + theme(legend.position = "none")

plot_PV_nG <- ggplot(ScaledAveTraits, aes(y= as.numeric(Ave.PV.NG), x= as.numeric(Ave.Heterozygosity), color = Population)) + 
  geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + 
  ylab("PV") + xlab("Genetic Diversity") + 
  geom_point(size=3.5) + 
  theme_bw() + 
  scale_color_manual(values = c("#F8766D","#7CAE00","black","#00BFC4","#C77CFF"), labels = c("EJUA","EJUB","ELG","MSA","SLO")) + theme(legend.position = "none")

plot_DTG_nG <- ggplot(ScaledAveTraits, aes(y= as.numeric(Ave.DTG.NG), x= as.numeric(Ave.Heterozygosity), color = Population)) + 
  geom_smooth(method = lm, se = FALSE, color = "grey", linetype="dashed") + 
  ylab("DTG") + xlab("Genetic Diversity") + 
  geom_point(size=3.5) + 
  theme_bw() + 
  scale_color_manual(values = c("#F8766D","#7CAE00","black","#00BFC4","#C77CFF"), labels = c("EJUA","EJUB","ELG","MSA","SLO")) + theme(legend.position = "none")

plot_grid(plot_DTF, plot_DTF_nG, plot_DTG, plot_DTF_nG, plot_PV, plot_PV_nG, cols = 2)

# r2 values
PH15.lm <- lm(Ave.PH15 ~ Ave.Heterozygosity, data=ScaledAveTraits)
summary(PH15.lm)$r.squared #0.5984029 NEW 0.6114565

PH30.lm <- lm(Ave.PH30 ~ Ave.Heterozygosity, data=ScaledAveTraits)
summary(PH30.lm)$r.squared #0.6620095 NEW 0.6525754

PH60.lm <- lm(Ave.PH60 ~ Ave.Heterozygosity, data=ScaledAveTraits)
summary(PH60.lm)$r.squared #0.9000648 NEW 0.8908136



Con.lm <- lm(Ave.Con ~ Ave.Heterozygosity, data=ScaledAveTraits)
summary(Con.lm)$r.squared #0.1039563 NEW 0.132321 ####### Do without G



AGB.lm <- lm(Ave.AGB ~ Ave.Heterozygosity, data=ScaledAveTraits)
summary(AGB.lm)$r.squared #0.8459944 NEW 0.8603378

DTF.lm <- lm(Ave.DTF ~ Ave.Heterozygosity, data=ScaledAveTraits)
summary(DTF.lm)$r.squared #0.04733033 NEW 0.06680049 ###### Do without G

DTG.lm <- lm(Ave.DTG ~ Ave.Heterozygosity, data=ScaledAveTraits)
summary(DTG.lm)$r.squared #0.3240982 NEW 0.3432531 ####### Do without G??

SW.lm <- lm(Ave.SW ~ Ave.Heterozygosity, data=ScaledAveTraits)
summary(SW.lm)$r.squared #0.6007554 NEW 0.5598908

NT.lm <- lm(Ave.NT ~ Ave.Heterozygosity, data=ScaledAveTraits)
summary(NT.lm)$r.squared #0.856041 NEW 0.8690212

PV.lm <- lm(Ave.PV ~ Ave.Heterozygosity, data=ScaledAveTraits)
summary(PV.lm)$r.squared #0.04842574 NEW 0.06590394 ##### Do without G

### Try DTF, PV, and NT without G
DTF_nG.lm <- lm(Ave.DTF.NG ~ Ave.Heterozygosity, data=ScaledAveTraits)
summary(DTF_nG.lm)$r.squared #0.9848446 NEW 0.9929779

Con_nG.lm <- lm(Ave.CON.NG ~ Ave.Heterozygosity, data=ScaledAveTraits)
summary(Con_nG.lm)$r.squared #0.899518 NEW 0.9273407

PV_nG.lm <- lm(Ave.PV.NG ~ Ave.Heterozygosity, data=ScaledAveTraits)
summary(PV_nG.lm)$r.squared #0.9454458 NEW 0.9344799

DTG_nG.lm <- lm(Ave.DTG.NG ~ Ave.Heterozygosity, data=ScaledAveTraits)
summary(DTG_nG.lm)$r.squared #0.6932581 NEW 0.6480552
