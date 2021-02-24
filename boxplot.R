library(ggplot2)
library(gridExtra)
library(ggpubr)
library(reshape2)


## 1. IF X, Y and Fill aren't different ----

Frag3 = melt(Frag_no_year, id.vars = "Direction")
Frag2 = melt(Frag_no_direction, id.vars = "Distance")

# Boxplot by Direction
ggplot(data = Frag3, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill = Direction)) +
  facet_wrap( ~ variable, scales="free") +
  scale_x_discrete(name="") + scale_y_continuous(name="") +
  theme(strip.text.x = element_text(size = 10, colour = "black", font = "bold"))

# Boxplot by Distance
ggplot(data = Frag2, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill = Distance)) +
  facet_wrap( ~ variable, scales="free")

## 2. Connecting Year with one Fill - Distance or Direction ----

## Creating Theme

myTheme = theme(#axis.text = element_text(size = 10, colour = "black"),
                axis.text.y = element_text(size = 14, colour = "black", face = "bold"),
                axis.text.x = element_text(angle = -90, hjust = 1, vjust = 0.12, colour = "black", size = 14),
                axis.ticks.x = element_blank(),
                #axis.text.x = element_blank(),
                #axis.title = element_text(size = 12, colour = "black", face = "bold"),
                axis.title.y  = element_text(size = 16, colour = "black", face = "bold"),
                axis.title.x  = element_blank(),
                panel.background = element_blank(),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black"),
                panel.spacing = unit(1.5, "lines"),
                panel.border = element_rect(colour = "black", fill=NA, size=1))


## Subsetting Year

Historical = c("1975", "1980", "1990", "1995", "2000", "2005", "2010", "2015", "2020")
Predicted = c("2025", "2030", "2040", "2050", "2060", "2070")

## Creating X axis tick labels

f = c(1975, '', 1990, '', 2000, '', 2010, '', 2020)
m = c('', 1980, '', 1995, '', 2005, '', 2015, '')
l = c(2025, 2030, 2040, 2050, 2060, 2070)
all = c(1975, '', 1990, '', 2000, '', 2010, '', 2020, '', 2030, '', 2050, '', 2070,
        '', 1980, '', 1995, '', 2005, '', 2015, '', 2025, '', 2040, '', 2060, '')
yearseq = rep(c(f,m), 4) # For historical
yearseq = rep(l, 8) # For predicted

## Creating vlines
hist_intercept = c(seq(9.5, 63.5, 9))
Predict_intercept = c(seq(6.5, 42.5, 6))
all_intercept = c(seq(15.5, 105.5 , 15))


## Creating subplots

p1 = ggplot(data = subset(Frag_class_all, YEAR %in% Predicted), aes(x = interaction(YEAR, DIRECTION), y = CA, color = DIRECTION)) +
  geom_boxplot(outlier.shape = NA) + myTheme + geom_vline(xintercept = Predict_intercept, linetype = "dotted", colour = "black") +
  scale_x_discrete(labels = yearseq)

p2 = ggplot(data = subset(Frag_class, YEAR %in% Predicted), aes(x = interaction(YEAR, DIRECTION), y = PLAND, fill = DIRECTION)) +
  geom_boxplot() + myTheme + geom_vline(xintercept = Predict_intercept, linetype = "dotted", colour = "black") +
  scale_x_discrete(labels = yearseq)

p3 = ggplot(data = subset(Frag_class, YEAR %in% Predicted), aes(x = interaction(YEAR, DIRECTION), y = NP, fill = DIRECTION)) +
  geom_boxplot() + myTheme + geom_vline(xintercept = Predict_intercept, linetype = "dotted", colour = "black") +
  scale_x_discrete(labels = yearseq)

p4 = ggplot(data = subset(Frag_class, YEAR %in% Predicted), aes(x = interaction(YEAR, DIRECTION), y = PD, fill = DIRECTION)) +
  geom_boxplot() + myTheme + geom_vline(xintercept = Predict_intercept, linetype = "dotted", colour = "black") + 
  scale_x_discrete(labels = yearseq)

p5 = ggplot(data = subset(Frag_class, YEAR %in% Predicted), aes(x = interaction(YEAR, DIRECTION), y = LPI, fill = DIRECTION)) +
  geom_boxplot() + myTheme + geom_vline(xintercept = Predict_intercept, linetype = "dotted", colour = "black") +
  scale_x_discrete(labels = yearseq)

p6 = ggplot(data = subset(Frag_class, YEAR %in% Predicted), aes(x = interaction(YEAR, DIRECTION), y = FRAC_AM, fill = DIRECTION)) + 
  geom_boxplot() + myTheme + geom_vline(xintercept = Predict_intercept, linetype = "dotted", colour = "black") +
  scale_x_discrete(labels = yearseq)

p7 = ggplot(data = subset(Frag_class, YEAR %in% Predicted), aes(x = interaction(YEAR, DIRECTION), y = CONTIG_AM, fill = DIRECTION)) +
  geom_boxplot() + myTheme + geom_vline(xintercept = Predict_intercept, linetype = "dotted", colour = "black") +
  scale_x_discrete(labels = yearseq)

p8 = ggplot(data = subset(Frag_class, YEAR %in% Predicted), aes(x = interaction(YEAR, DIRECTION), y = CLUMPY, fill = DIRECTION)) +
  geom_boxplot() + myTheme + geom_vline(xintercept = Predict_intercept, linetype = "dotted", colour = "black") +
  scale_x_discrete(labels = yearseq)

p9 = ggplot(data = subset(Frag_class, YEAR %in% Predicted), aes(x = interaction(YEAR, DIRECTION), y = COHESION, fill = DIRECTION)) +
  geom_boxplot() + myTheme + geom_vline(xintercept = Predict_intercept, linetype = "dotted", colour = "black") +
  scale_x_discrete(labels = yearseq)

p10 = ggplot(data = subset(Frag_class, YEAR %in% Predicted), aes(x = interaction(YEAR, DIRECTION), y = SPLIT, fill = DIRECTION)) +
  geom_boxplot() + myTheme + geom_vline(xintercept = Predict_intercept, linetype = "dotted", colour = "black") +
  scale_x_discrete(labels = yearseq)



## EITHER RUN THIS LONG CODE ...
# One of the plots must have a legend attached; By runnig the below code. 
## p6 = ggplot(data = Frag, aes(x = Year, y = CONTIG_AM, fill = Direction)) + 
## geom_boxplot() + theme(legend.position="bottom")
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(p6)

# Shown for two plots; insert and repeat for other plots too
p9 <- grid.arrange(arrangeGrob(p6 + theme(legend.position="none"),
                               p7 + theme(legend.position="none"),
                               ncol=2),
                   mylegend, nrow=2,heights=c(10, 1))

# .... OR RUN BELOW CODE
# For landscape metrics: 26 x 20; For class metric: 

tiff('Predicted_class_metric.tiff', units="in", width=30, height=20, res=300, compression = 'lzw')
ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, ncol = 2, nrow = 5, common.legend = TRUE, legend="bottom")
#grid.rect(width = 1, height = 1, gp = gpar(lwd = 2, col = "blue", fill = NA))
dev.off()



p4 = ggplot(data = Frag_class_all, aes(x = interaction(YEAR, CLASS), y = NP, fill = CLASS)) +
  geom_boxplot(na.rm = TRUE) + myTheme + 
  geom_vline(xintercept = c(seq(15.5, 60.5 , 15)), linetype = "dotted", colour = "black") + 
  scale_x_discrete(labels = yearseq) + theme(legend.position = c(0.5, 0.90)) + ylab("Number of Patches (NP)") +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, title.theme = element_text(size = 10),
                             label.theme = element_text(size = 10)))

# 2. YEAR and DIRECTION
p5 = ggplot(data = subset(Frag_class_all, CLASS %in% "URBAN"), aes(x = interaction(YEAR, DIRECTION), y = NP, fill = DIRECTION)) +
  geom_boxplot(na.rm = TRUE) + myTheme + 
  geom_vline(xintercept = c(seq(15.5, 105.5 , 15)), linetype = "dotted", colour = "black") + 
  scale_x_discrete(labels = yearseq) + theme(legend.position = c(0.5, 0.90)) + ylab("Number of Patches (NP)") +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, title.theme = element_text(size = 10),
                             label.theme = element_text(size = 10)))

# 3. YEAR and DISTANCE
p6 = ggplot(data = subset(Frag_class_all, CLASS %in% "URBAN"), aes(x = interaction(YEAR, DISTANCE), y = NP, fill = DISTANCE)) +
  geom_boxplot(na.rm = TRUE) + myTheme + theme(legend.position = c(0.5, 0.90)) + ylab("Number of Patches (NP)") +
  geom_vline(xintercept = c(seq(15.5, 135.5 , 15)), linetype = "dotted", colour = "black") + 
  scale_x_discrete(labels = yearseq) + 
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, title.theme = element_text(size = 10),
                             label.theme = element_text(size = 10)))

#
p7 = ggplot(data = Frag_class_all, aes(x = interaction(YEAR, CLASS), y = LPI, fill = CLASS)) +
  geom_boxplot(na.rm = TRUE) + myTheme + 
  geom_vline(xintercept = c(seq(15.5, 60.5 , 15)), linetype = "dotted", colour = "black") + 
  scale_x_discrete(labels = yearseq) + theme(legend.position = c(0.5, 0.92)) +
  ylab("Largest Patch Index (LPI)") +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, title.theme = element_text(size = 10),
                             label.theme = element_text(size = 10)))

# 2. YEAR and DIRECTION
p8 = ggplot(data = subset(Frag_class_all, CLASS %in% "URBAN"), aes(x = interaction(YEAR, DIRECTION), y = LPI, fill = DIRECTION)) +
  geom_boxplot(na.rm = TRUE) + myTheme + 
  geom_vline(xintercept = c(seq(15.5, 105.5 , 15)), linetype = "dotted", colour = "black") + 
  scale_x_discrete(labels = yearseq) + theme(legend.position = c(0.5, 0.90)) +
  ylab("Largest Patch Index (LPI)") +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, title.theme = element_text(size = 10),
                             label.theme = element_text(size = 10)))

# 3. YEAR and DISTANCE
p9 = ggplot(data = subset(Frag_class_all, CLASS %in% "URBAN"), aes(x = interaction(YEAR, DISTANCE), y = LPI, fill = DISTANCE)) +
  geom_boxplot(na.rm = TRUE) + myTheme + theme(legend.position = c(0.5, 0.90)) +
  geom_vline(xintercept = c(seq(15.5, 135.5 , 15)), linetype = "dotted", colour = "black") + 
  scale_x_discrete(labels = yearseq) + ylab("Largest Patch Index (LPI)") +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, title.theme = element_text(size = 10),
                             label.theme = element_text(size = 10)))

p10 = ggplot(data = Frag_class_all, aes(x = interaction(YEAR, CLASS), y = CA, fill = CLASS)) +
  geom_boxplot(na.rm = TRUE) + myTheme + 
  geom_vline(xintercept = c(seq(15.5, 60.5 , 15)), linetype = "dotted", colour = "black") + 
  scale_x_discrete(labels = yearseq) + theme(legend.position = c(0.5, 0.90)) + ylab("Class Area (in hectares)") +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, title.theme = element_text(size = 10),
                             label.theme = element_text(size = 10)))

# 2. YEAR and DIRECTION
p11 = ggplot(data = subset(Frag_class_all, CLASS %in% "URBAN"), aes(x = interaction(YEAR, DIRECTION), y = CA, fill = DIRECTION)) +
  geom_boxplot(na.rm = TRUE) + myTheme + 
  geom_vline(xintercept = c(seq(15.5, 105.5 , 15)), linetype = "dotted", colour = "black") + 
  scale_x_discrete(labels = yearseq) + theme(legend.position = c(0.5, 0.90)) + ylab("Class Area (in hectares)") +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, title.theme = element_text(size = 10),
                             label.theme = element_text(size = 10)))

# 3. YEAR and DISTANCE
p12 = ggplot(subset(Frag_class_all, CLASS %in% "URBAN"), aes(x = interaction(YEAR, DISTANCE), y = CA, fill = DISTANCE)) +
  geom_boxplot(na.rm = TRUE) + myTheme + theme(legend.position = c(0.5, 0.90)) + ylab("Class Area (in hectares)") +
  geom_vline(xintercept = c(seq(15.5, 135.5 , 15)), linetype = "dotted", colour = "black") + 
  scale_x_discrete(labels = yearseq) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, title.theme = element_text(size = 10),
                             label.theme = element_text(size = 10)))


#
p13 = ggplot(data = Frag_class_all, aes(x = interaction(YEAR, CLASS), y = CLUMPY, fill = CLASS)) +
  geom_boxplot(na.rm = TRUE) + myTheme + 
  geom_vline(xintercept = c(seq(15.5, 60.5 , 15)), linetype = "dotted", colour = "black") + 
  scale_x_discrete(labels = yearseq) + theme(legend.position = c(0.5, 0.1)) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, title.theme = element_text(size = 10),
                             label.theme = element_text(size = 10)))

# 2. YEAR and DIRECTION
p14 = ggplot(data = subset(Frag_class_all, CLASS %in% "URBAN"), aes(x = interaction(YEAR, DIRECTION), y = CLUMPY, fill = DIRECTION)) +
  geom_boxplot(na.rm = TRUE) + myTheme + 
  geom_vline(xintercept = c(seq(15.5, 105.5 , 15)), linetype = "dotted", colour = "black") + 
  scale_x_discrete(labels = yearseq) + theme(legend.position = c(0.5, 0.1)) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, title.theme = element_text(size = 10),
                             label.theme = element_text(size = 10)))

# 3. YEAR and DISTANCE
p15 = ggplot(data = subset(Frag_class_all, CLASS %in% "URBAN"), aes(x = interaction(YEAR, DISTANCE), y = CLUMPY, fill = DISTANCE)) +
  geom_boxplot(na.rm = TRUE) + myTheme + theme(legend.position = c(0.5, 0.1)) +
  geom_vline(xintercept = c(seq(15.5, 135.5 , 15)), linetype = "dotted", colour = "black") + 
  scale_x_discrete(labels = yearseq) + 
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, title.theme = element_text(size = 10),
                             label.theme = element_text(size = 10)))


#
p16 = ggplot(data = Frag_class_all, aes(x = interaction(YEAR, CLASS), y = COHESION, fill = CLASS)) +
  geom_boxplot(na.rm = TRUE) + myTheme + 
  geom_vline(xintercept = c(seq(15.5, 60.5 , 15)), linetype = "dotted", colour = "black") + 
  scale_x_discrete(labels = yearseq) + theme(legend.position = c(0.5, 0.1)) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, title.theme = element_text(size = 10),
                             label.theme = element_text(size = 10)))

# 2. YEAR and DIRECTION
p17 = ggplot(data = subset(Frag_class_all, CLASS %in% "URBAN"), aes(x = interaction(YEAR, DIRECTION), y = COHESION, fill = DIRECTION)) +
  geom_boxplot(na.rm = TRUE) + myTheme + 
  geom_vline(xintercept = c(seq(15.5, 105.5 , 15)), linetype = "dotted", colour = "black") + 
  scale_x_discrete(labels = yearseq) + theme(legend.position = c(0.5, 0.1)) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, title.theme = element_text(size = 10),
                             label.theme = element_text(size = 10)))

# 3. YEAR and DISTANCE
p18 = ggplot(data = subset(Frag_class_all, CLASS %in% "URBAN"), aes(x = interaction(YEAR, DISTANCE), y = COHESION, fill = DISTANCE)) +
  geom_boxplot(na.rm = TRUE) + myTheme + theme(legend.position = c(0.5, 0.1)) +
  geom_vline(xintercept = c(seq(15.5, 135.5 , 15)), linetype = "dotted", colour = "black") + 
  scale_x_discrete(labels = yearseq) + 
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, title.theme = element_text(size = 10),
                             label.theme = element_text(size = 10)))

#
p19 = ggplot(data = Frag_class_all, aes(x = interaction(YEAR, CLASS), y = CONTIG_AM, fill = CLASS)) +
  geom_boxplot(na.rm = TRUE) + myTheme + ylab("Area-weighted Mean Contagion index (in %)") +
  geom_vline(xintercept = c(seq(15.5, 60.5 , 15)), linetype = "dotted", colour = "black") + 
  scale_x_discrete(labels = yearseq) + theme(legend.position = c(0.5, 0.1)) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, title.theme = element_text(size = 10),
                             label.theme = element_text(size = 10)))

# 2. YEAR and DIRECTION
p20 = ggplot(data = subset(Frag_class_all, CLASS %in% "URBAN"), aes(x = interaction(YEAR, DIRECTION), y = CONTIG_AM, fill = DIRECTION)) +
  geom_boxplot(na.rm = TRUE) + myTheme + ylab("Area-weighted Mean Contagion index (in %)") +
  geom_vline(xintercept = c(seq(15.5, 105.5 , 15)), linetype = "dotted", colour = "black") + 
  scale_x_discrete(labels = yearseq) + theme(legend.position = c(0.5, 0.1)) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, title.theme = element_text(size = 10),
                             label.theme = element_text(size = 10)))

# 3. YEAR and DISTANCE
p21 = ggplot(data = subset(Frag_class_all, CLASS %in% "URBAN"), aes(x = interaction(YEAR, DISTANCE), y = CONTIG_AM, fill = DISTANCE)) +
  geom_boxplot(na.rm = TRUE) + myTheme + theme(legend.position = c(0.5, 0.1)) +
  geom_vline(xintercept = c(seq(15.5, 135.5 , 15)), linetype = "dotted", colour = "black") + 
  scale_x_discrete(labels = yearseq) + ylab("Area-weighted Mean Contagion index (in %)") +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, title.theme = element_text(size = 10),
                             label.theme = element_text(size = 10)))




#---
#Part 1:
Frag_class_all %>% 
  mutate(test = subset(CLASS))
test = melt(Frag_class_all, id.vars = YEAR)

df = melt(test, id.vars = YEAR)

ggplot(df, aes(YEAR, value)) + geom_point() + stat_smooth(method = "lm", se = FALSE) + facet_wrap(~ variable, ncol = 8)
