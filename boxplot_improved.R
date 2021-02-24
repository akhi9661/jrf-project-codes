library(ggplot2)
library(gridExtra)
library(ggpubr)
library(reshape2)


# theme
myTheme = theme(#axis.text = element_text(size = 10, colour = "black"),
  axis.text.y = element_text(size = 16, colour = "black", face = "bold"),
  axis.text.x = element_text(angle = -90, hjust = 1, vjust = 0.12, colour = "black", size = 16),
  axis.ticks.x = element_blank(),
  axis.title.y.right = element_text(size = 20, colour = "black", face = "bold"),
  axis.title.y.left = element_blank(),
  axis.title.x  = element_blank(),
  panel.background = element_blank(),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  axis.line = element_line(colour = "black"),
  panel.spacing = unit(1.5, "lines"),
  panel.border = element_rect(colour = "black", fill=NA, size=1),
  legend.key.size = unit(1, "cm"), legend.justification = "center", legend.direction = "horizontal",
  legend.background = element_rect(size = 0.5, linetype = "solid", colour = "grey63"))
  #guide_legend = (title.position = "top", title.hjust = 0.5, title.vjust = 0.5)


# For landscape level

p1 = ggplot(data = Frag, aes(x = interaction(YEAR, DIRECTION), y = COHESION, fill = DIRECTION)) +
  geom_boxplot() + myTheme + geom_vline(xintercept = all_intercept, linetype = "dotted", colour = "black") + 
  annotate("text", x = 100, y = 93, label = 'atop(bold("EACH BOXPLOT REPRESENTS AN YEAR"))', size = 5, parse = TRUE)


# For class level

# 1. YEAR and CLASS
p1 = ggplot(data = Frag_class_all, aes(x = interaction(YEAR, CLASS), y = FRAC_AM, fill = CLASS)) +
  geom_boxplot(na.rm = TRUE, outlier.shape = NA, alpha = 0.2) + myTheme + 
  geom_vline(xintercept = c(seq(15.5, 60.5 , 15)), linetype = "dotted", colour = "black") + 
  scale_x_discrete(labels = rep(all, 4)) + theme(legend.position = c(0.5, 0.90)) + 
  ylab("Area-weighted Mean Fractal Dimension (FRAC_AM)") + 
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, title.theme = element_text(size = 10),
                             label.theme = element_text(size = 10)))

# 2. YEAR and DIRECTION
p2 = ggplot(data = subset(Frag_class_all1, CLASS %in% "URBAN"), aes(x = interaction(YEAR, DIRECTION), y = FRAC_AM, color = DIRECTION)) +
  geom_boxplot(na.rm = TRUE, outlier.shape = NA) + myTheme + 
  geom_smooth(data = subset(Frag_class_all1, CLASS %in% "URBAN"), aes(as.numeric(YEAR), FRAC_AM, color = 'red' ), se = FALSE) +
  geom_vline(xintercept = c(seq(15.5, 105.5 , 15)), linetype = "dotted", colour = "black") + 
  scale_x_discrete(labels = rep(all, 4)) + theme(legend.position = c(0.5, 0.90)) +
  ylab("Area-weighted Mean Fractal Dimension (FRAC_AM)") + 
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, title.theme = element_text(size = 10),
                             label.theme = element_text(size = 10)))

# 3. YEAR and DISTANCE
p3 = ggplot(data = subset(Frag_class_all, CLASS %in% "URBAN"), aes(x = interaction(YEAR, DISTANCE), y = FRAC_AM, fill = DISTANCE)) +
  geom_boxplot(na.rm = TRUE) + myTheme + theme(legend.position = c(0.5, 0.90)) +
  geom_vline(xintercept = c(seq(15.5, 135.5 , 15)), linetype = "dotted", colour = "black") + 
  scale_x_discrete(labels = yearseq) + ylab("Area-weighted Mean Fractal Dimension (FRAC_AM)") + 
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, title.theme = element_text(size = 16),
                             label.theme = element_text(size = 16)))


# Export
tiff('CLUMPY_and_COHESION_class_600dpi.tiff', units="in", width=30, height=20, res=600, compression = 'lzw')
ggarrange(p13, p16, p14, p17, p15, p18, ncol = 2, nrow = 3, legend = "bottom")
dev.off()



## ----

p6 = ggplot(data = Frag_class_all1, aes(x = interaction(YEAR, CLASS), y = CONTIG_AM, fill = CLASS)) +
  geom_boxplot(na.rm = TRUE, alpha = 1) + myTheme + geom_vline(xintercept = c(seq(15.5, 60.5 , 15)), 
                                                                 linetype = "dotted", colour = "black") + 
  scale_y_continuous(position = 'right', sec.axis = dup_axis()) + scale_x_discrete(labels = rep(all, 4)) + 
  theme(axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank()) + 
  ylab("Contagion Index\n") + scale_fill_brewer(palette = "Paired") +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, title.theme = element_text(size = 10, face = "bold"),
                             label.theme = element_text(size = 10, face = "bold")))
