## Lake Washington 2022 ecotype analysis
#Packages
library(ggplot2)

#Read in csv of lake washington data
onerka<-read.csv("Onerka_LWA.csv")
onerka<-onerka[1:75,1:13]

onerka$length <- as.numeric(onerka$length)
#sets color palette
colors<-c("darkorange", "firebrick", "darkblue", "black")

# Plotting life history groups (anadromous and non-anadromous/resident)

ggplot(onerka, aes(x = d13C, y = d15N, color = lifehistory)) +
  geom_point(size = 2) +
  stat_ellipse() +
  scale_color_manual(values = colors) +
  theme_classic() +
  labs(
    x = expression(delta^{13}*C),
    y = expression(delta^{15}*N),
    color = "Life history"
  )


# plotting genetically-determined ecotypes
onerka_geno <- subset(
  onerka,
  !is.na(genotype))

figure2<-ggplot(
  onerka_geno,
  aes(x = d13C, y = d15N, color = genotype)
) +
  geom_point(size = 1) +
  stat_ellipse(na.rm = TRUE) +
  scale_color_manual(values = colors) +
  theme_classic() +
  labs(
    x = expression(delta^{13}*C),
    y = expression(delta^{15}*N),
    color = "Genotype"
  )

ggsave("figure2.jpg", width = 6, height = 4, dpi = 300)

onerka_geno <- subset(
  onerka,
  !is.na(genotype))

onerka_geno_length <- subset(
  onerka_geno,
  !is.na(length))
# plotting against length
figure3<-ggplot(
  onerka_geno_length,
  aes(x = length, y = d15N, color = genotype)
) +
  geom_point(size = 1) +
  stat_ellipse(na.rm = TRUE) +
  scale_color_manual(values = colors) +
  theme_classic() +
  labs(
    x = "Length (mm)",
    y = expression(delta^{15}*N),
    color = "Genotype"
  )

ggsave("figure3.jpg", width = 6, height = 4, dpi = 300)
# subset anadromous fish to calculate statistics
anad <- subset(onerka, lifehistory == "Anadromous")

resident <- subset(onerka, lifehistory == "Resident")

resident$length <- as.numeric(resident$length)
resident$site <- factor(resident$site)


kokanee <- subset(
  onerka,
  genotype == "Kokanee")

resid <- subset(
  onerka,
  genotype == "Residual")

sockeye <- subset(
  onerka,
  genotype == "Sockeye")

# summary stats
mean(kokanee$d15N)
sd(kokanee$d15N)
mean(kokanee$d13C)
sd(kokanee$d13C)

mean(kokanee$d15N)
sd(kokanee$d15N)
mean(kokanee$d13C)
sd(kokanee$d13C)

mean(resid$d15N)
sd(resid$d15N)
mean(resid$d13C)
sd(resid$d13C)

mean(anad$d15N)
sd(anad$d15N)
mean(anad$d13C)
sd(anad$d13C)
# t-tests comparing isotope signatures

# Kokanee vs Residual
t.test(kokanee$d15N, resid$d15N)
t.test(kokanee$d13C, resid$d13C)

# Sockeye vs Residual
t.test(sockeye$d15N, resid$d15N)
t.test(sockeye$d13C, resid$d13C)


resident$length <- as.numeric(resident$length)


ggplot(onerka,
       aes(x = length,
           y = d15N,
           color = genotype)) +
  geom_point(size = 2) +
  stat_ellipse() +
  scale_color_manual(values = colors) +
  theme_classic() +
  labs(
    x = "Length (mm)",
    y = expression(delta^{15}*N),
    color = "Ecotype"
  )

#=========================================================
# Linear models
#=========================================================

onerka$length  <- as.numeric(onerka$length)
onerka$site    <- factor(onerka$site)
onerka$ecotype <- factor(onerka$genotype)

# Carbon model
mod.C <- lm(d13C ~ length + ecotype + site, data = onerka)
anova(mod.C)

# Nitrogen model
mod.N <- lm(d15N ~ length + ecotype + site, data = onerka)
anova(mod.N)

# Linear models too look at source of variation in d15N for non-anad (resident) fish
mod_N_resident <- lm(
  d15N ~ length + site-1,
  data = resident
)

summary(mod_N_resident)
AIC(mod_N_resident)

mod_N_resident2 <- lm(
  d15N ~ length,
  data = resident
)
AIC(mod_N_resident2)

mod_N_resident3 <- lm(
  d15N ~ site-1,
  data = resident
)
AIC(mod_N_resident3)
