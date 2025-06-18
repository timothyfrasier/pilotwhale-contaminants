######################################
#  CODE FOR ANALYSIS OF PILOT WHALE  #
#  CONTAMINANT DATA.                 #
#                                    #
#         Last updated:              #
#          18-Jun-2025               #
#                                    #
#             by                     #
#         Tim Frasier                #
######################################

#-----------------------------#
#  LOAD APPROPRIATE PACKAGES  #
#-----------------------------#
library(ggplot2)
library(cowplot)


#----------------------------#
#      LOAD THE DATA         #
#----------------------------#
data <- read.table("../data/contaminants.csv", header = TRUE, sep = ",")


#----------------------------#
#    VISUALIZE THE DATA      #
#----------------------------#

#--- Boxplots of all measurements for males versus females ---#

# Hg Skin and Blubber
skin <- ggplot(data) +
  theme_bw() +
  geom_boxplot(aes(x = Sex, y = Hg_skin_ng_g), fill = "chartreuse", alpha = 0.5) +
  geom_jitter(aes(x = Sex, y = Hg_skin_ng_g), color = "dodgerblue4", height = 0, width = 0.35, alpha = 0.6) +
  labs(
    x = "",
    y = "Hg in skin (ng/g)"
  )

# Hg Blubber
blubber <- ggplot(data) +
  theme_bw() +
  geom_boxplot(aes(x = Sex, y = Hg_blubber_ng_g), fill = "chartreuse", alpha = 0.5) +
  geom_jitter(aes(x = Sex, y = Hg_blubber_ng_g), color = "dodgerblue4", height = 0, width = 0.35, alpha = 0.6) +
  labs(
    x = "",
    y = "Hg in blubber (ng/g)"
  )

# Combined
tiff("../results/Hg_plots.tiff", width = 7, height = 3, units = "in", res = 600)
plot_grid(blubber, skin, nrow = 1, ncol = 2, labels = c('(a)', '(b)'), label_size = 12)
dev.off()
#pdf("../results/Hg_plots.pdf", width = 7, height = 3)
#plot_grid(blubber, skin, nrow = 1, ncol = 2, labels = c('A', 'B'), label_size = 12)
#dev.off()

#-------------------------#
#  Statistical Analyses   #
#-------------------------#
Hg_skin_female <- na.omit(data[data$Sex == "Female", 3])
Hg_skin_male <- na.omit(data[data$Sex == "Male", 3])

Hg_blubber_female <- na.omit(data[data$Sex == "Female", 4])
Hg_blubber_male <- na.omit(data[data$Sex == "Male", 4])


#--- Sex differences in skin ---#
t.test(Hg_skin_female, Hg_skin_male)

#--- Sex differences in blubber ---#
t.test(Hg_blubber_female, Hg_blubber_male)

#--- Sex differences between skin and blubber ---#
# Females
t.test(Hg_skin_female, Hg_blubber_female)

# Males
t.test(Hg_skin_male, Hg_blubber_male)


#--- Cd ---#
Cd <- ggplot(data) +
  theme_bw() +
  geom_boxplot(aes(x = Sex, y = Cd_blubber_ug_g), fill = "chartreuse", alpha = 0.7) +
  geom_jitter(aes(x = Sex, y = Cd_blubber_ug_g), color = "dodgerblue4", height = 0, width = 0.35, alpha = 0.6) +
  labs(
    title = "Cadmium",
    x = "",
    y = bquote("Cd in blubber"~(mu*g/g))
  )

# Stats
Cd_female <- na.omit(data[data$Sex == "Female", 5])
Cd_male <- na.omit(data[data$Sex == "Male", 5])
t.test(Cd_female, Cd_male)


#--- Cr ---#
Cr <- ggplot(data) +
  theme_bw() +
  geom_boxplot(aes(x = Sex, y = Cr_blubber_ug_g), fill = "chartreuse", alpha = 0.7) +
  geom_jitter(aes(x = Sex, y = Cr_blubber_ug_g), color = "dodgerblue4", height = 0, width = 0.35, alpha = 0.6) +
  labs(
    title = "Chromium",
    x = "",
    y = bquote("Cr in blubber"~(mu*g/g))
  )

# Stats
Cr_female <- na.omit(data[data$Sex == "Female", 6])
Cr_male <- na.omit(data[data$Sex == "Male", 6])
t.test(Cr_female, Cr_male)


#--- Fe ---#
Fe <- ggplot(data) +
  theme_bw() +
  geom_boxplot(aes(x = Sex, y = Fe_blubber_ug_g), fill = "chartreuse", alpha = 0.7) +
  geom_jitter(aes(x = Sex, y = Fe_blubber_ug_g), color = "dodgerblue4", height = 0, width = 0.35, alpha = 0.6) +
  labs(
    title = "Iron",
    x = "",
    y = bquote("Fe in blubber"~(mu*g/g))
  )

# Stats
Fe_female <- na.omit(data[data$Sex == "Female", 7])
Fe_male <- na.omit(data[data$Sex == "Male", 7])
t.test(Fe_female, Fe_male)


#--- Mg ---#
Mg <- ggplot(data) +
  theme_bw() +
  geom_boxplot(aes(x = Sex, y = Mg_blubber_ug_g), fill = "chartreuse", alpha = 0.7) +
  geom_jitter(aes(x = Sex, y = Mg_blubber_ug_g), color = "dodgerblue4", height = 0, width = 0.35, alpha = 0.6) +
  labs(
    title = "Magnesium",
    x = "",
    y = bquote("Mg in blubber"~(mu*g/g))
  )

# Stats
Mg_female <- na.omit(data[data$Sex == "Female", 8])
Mg_male <- na.omit(data[data$Sex == "Male", 8])
t.test(Mg_female, Mg_male)

#--- Ni ---#
Ni <- ggplot(data) +
  theme_bw() +
  geom_boxplot(aes(x = Sex, y = Ni_blubber_ug_g), fill = "chartreuse", alpha = 0.7) +
  geom_jitter(aes(x = Sex, y = Ni_blubber_ug_g), color = "dodgerblue4", height = 0, width = 0.35, alpha = 0.6) +
  labs(
    title = "Nickel",
    x = "",
    y = bquote("Ni in blubber"~(mu*g/g))
  )

# Stats
Ni_female <- na.omit(data[data$Sex == "Female", 9])
Ni_male <- na.omit(data[data$Sex == "Male", 9])
t.test(Ni_female, Ni_male)


#--- K ---#
K <- ggplot(data) +
  theme_bw() +
  geom_boxplot(aes(x = Sex, y = K_blubber_ug_g), fill = "chartreuse", alpha = 0.7) +
  geom_jitter(aes(x = Sex, y = K_blubber_ug_g), color = "dodgerblue4", height = 0, width = 0.35, alpha = 0.6) +
  labs(
    title = "Potassium",
    x = "",
    y = bquote("K in blubber"~(mu*g/g))
  )

# Stats
K_female <- na.omit(data[data$Sex == "Female", 10])
K_male <- na.omit(data[data$Sex == "Male", 10])
t.test(K_female, K_male)


#--- Se ---#
Se <- ggplot(data) +
  theme_bw() +
  geom_boxplot(aes(x = Sex, y = Se_blubber_ug_g), fill = "chartreuse", alpha = 0.7) +
  geom_jitter(aes(x = Sex, y = Se_blubber_ug_g), color = "dodgerblue4", height = 0, width = 0.35, alpha = 0.6) +
  labs(
    title = "Selenium",
    x = "",
    y = bquote("Se in blubber"~(mu*g/g))
  )

# Stats
Se_female <- na.omit(data[data$Sex == "Female", 11])
Se_male <- na.omit(data[data$Sex == "Male", 11])
t.test(Se_female, Se_male)


#--- Ag ---#
ggplot(data) +
  theme_bw() +
  geom_boxplot(aes(x = Sex, y = Ag_blubber_ug_g), fill = "chartreuse", alpha = 0.7) +
  geom_jitter(aes(x = Sex, y = Ag_blubber_ug_g), color = "dodgerblue4", height = 0, width = 0.35, alpha = 0.6) +
  labs(
    title = "Silver",
    x = "",
    y = "Ag in blubber (\u00B5g/g)"
  )

# Stats
Ag_female <- na.omit(data[data$Sex == "Female", 12])
Ag_male <- na.omit(data[data$Sex == "Male", 12])
t.test(Ag_female, Ag_male)


# Use log values to show data better
data$Ag_new <- log(data$Ag_blubber_ug_g)
Ag <- ggplot(data) +
  theme_bw() +
  geom_boxplot(aes(x = Sex, y = Ag_new), fill = "chartreuse", alpha = 0.7) +
  geom_jitter(aes(x = Sex, y = Ag_new), color = "dodgerblue4", height = 0, width = 0.35, alpha = 0.6) +
  labs(
    title = "log(Silver)",
    x = "",
    y = bquote("log(Ag) in blubber"~(mu*g/g))
  )

#--- Na ---#
Na <- ggplot(data) +
  theme_bw() +
  geom_boxplot(aes(x = Sex, y = Na_blubber_ug_g), fill = "chartreuse", alpha = 0.7) +
  geom_jitter(aes(x = Sex, y = Na_blubber_ug_g), color = "dodgerblue4", height = 0, width = 0.35, alpha = 0.6) +
  labs(
    title = "Sodium",
    x = "",
    y = bquote("Na in blubber"~(mu*g/g))
  )

# Stats
Na_female <- na.omit(data[data$Sex == "Female", 13])
Na_male <- na.omit(data[data$Sex == "Male", 13])
t.test(Na_female, Na_male)


#--- Sr ---#
Sr <- ggplot(data) +
  theme_bw() +
  geom_boxplot(aes(x = Sex, y = Sr_blubber_ug_g), fill = "chartreuse", alpha = 0.7) +
  geom_jitter(aes(x = Sex, y = Sr_blubber_ug_g), color = "dodgerblue4", height = 0, width = 0.35, alpha = 0.6) +
  labs(
    title = "Strontium",
    x = "",
    y = bquote("Sr in blubber"~(mu*g/g))
  )

# Stats
Sr_female <- na.omit(data[data$Sex == "Female", 14])
Sr_male <- na.omit(data[data$Sex == "Male", 14])
t.test(Sr_female, Sr_male)

# Combined
pdf("../results/Blubber_plots1.pdf", width = 7, height = 10)
plot_grid(Cd, Cr, Fe, Mg, Ni, K, nrow = 3, ncol = 2, labels = c('A', 'B', 'C', 'D', 'E', 'F'), label_size = 12)
dev.off()

pdf("../results/Blubber_plots2.pdf", width = 7, height = 7)
plot_grid(Se, Ag, Na, Sr, nrow = 2, ncol = 2, labels = c('G', 'H', 'I', 'J'), label_size = 12)
dev.off()


#-----------------------------------------#
#    Comparing Hg in Blubber v Skin       #
#-----------------------------------------#
blubber_v_skin <- ggplot(data) +
  theme_bw() +
  geom_point(aes(y = Hg_skin_ng_g, x = Hg_blubber_ng_g, colour = Sex), size = 2, alpha = 0.7) +
  geom_smooth(aes(y = Hg_skin_ng_g, x = Hg_blubber_ng_g, colour = Sex), method = "lm", alpha = 0.6) +
  scale_color_manual(values = c("coral", "dodgerblue4")) +
  labs(
    x = "Hg in blubber (ng/g)",
    y = "Hg in skin (ng/g)"
  )

tiff("../results/Hg-blubber_v_Hg-skin.tiff", width = 4, height = 2.5, units = "in", res = 600)
plot(blubber_v_skin)
dev.off()

#pdf("../results/Hg-blubber_v_Hg-skin.pdf", width = 4.0, height = 2.5)
#plot(blubber_v_skin)
#dev.off()

#--- Statistics of skin vs blubber ---#
# Female
female_Hg <- data[data$Sex == "Female", 3:4]
female_Hg <- female_Hg[complete.cases(female_Hg), ]
model1 <- lm(female_Hg[, 1] ~ female_Hg[, 2])
summary(model1)

# Male
male_Hg <- data[data$Sex == "Male", 3:4]
male_Hg <- male_Hg[complete.cases(male_Hg), ]
model2 <- lm(male_Hg[, 1] ~ male_Hg[, 2])
summary(model2)
