library(ggplot2)
library(reshape2)
library(ggpubr)
library(plyr)

myTheme = theme(axis.text.y = element_text(size = 20, colour = "black", face = "bold"),
                axis.text.x = element_text(colour = "black", size = 20, angle = 90, face = "bold"),
                axis.ticks.x = element_blank(),
                axis.title.x  = element_blank(),
                axis.title.y.right = element_text(size = 24, colour = "black", face = "bold"),
                axis.title.y.left = element_blank(),
                panel.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black"),
                panel.spacing = unit(1.5, "lines"),
                panel.border = element_rect(colour = "black", fill=NA, size=1.3),
                strip.background = element_rect(fill = "white", colour = "black"),
                strip.text = element_text(face = "bold", size = 25),
                legend.key.size = unit(1.5, "cm"), legend.justification = "center", legend.direction = "horizontal",
                legend.background = element_rect(size = 1, linetype = "solid", colour = "black"),
                legend.text = element_text(size = 24, face = "bold"))

#
yearscale = c(1975, '', 1990, '', 2000, '', 2010, '', 2020, '', 2030, '', 2050, '', 2070)
yearscale1 = c('', 1980, '', '', '', 2005, '', '', '', 2025, '', '', '', 2060, '')
distscale = seq(5, 50, 5)
distscale = c('', 10, '', 20, '', 30, '', 40, '', 50)
col_dist = c('YEAR', rep(c('INDEX', seq(5, 50, 5)), 9))
colnames(Frag_distance_year) <- col_dist
#Frag_distance_year = Frag_distance_year[-1,]
#Frag_distance_year = Frag_distance_year[,-2]
#distance_year: 



## 1. Trying geom_col ----
Frag_directional_year1 = Frag_directional_year
Frag_directional_year1$YEAR = as.factor(Frag_directional_year1$YEAR)

#CA
test = Frag_directional_year1[c(1:9)]
df = melt(test, id.vars = 'YEAR')

c1 = ggplot(df, aes(YEAR, value, fill = variable)) + geom_col(width = 0.8) + 
  geom_smooth(aes(as.numeric(YEAR), value), colour = 'black', se = FALSE) + 
  scale_y_continuous(position = 'right', sec.axis = dup_axis(), expand = expansion(mult = c(0, .1))) +
  facet_wrap(~ variable, ncol = 8) + myTheme + 
  theme(axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank()) +
  ylab("Class Area (in ha)\n") + scale_fill_brewer(palette = "Paired", name = "variable") +
  scale_x_discrete(labels = yearscale1) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, title = "DIRECTION",
                             title.theme = element_text(size = 24, face = "bold"),
                             label.theme = element_text(size = 24, face = "bold"), nrow = 1))

#PLAND
test = Frag_directional_year1[c(1, 11:18)]
df = melt(test, id.vars = 'YEAR')

c9 = ggplot(df, aes(YEAR, value, fill = variable)) + geom_col(width = 0.8) + 
  geom_smooth(aes(as.numeric(YEAR), value), colour = 'black', se = FALSE) + 
  scale_y_continuous(position = 'right', sec.axis = dup_axis(), expand = expansion(mult = c(0, .1))) +
  facet_wrap(~ variable, ncol = 8) + myTheme + 
  theme(strip.text = element_blank(), axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank()) +
  ylab("PLAND (in %)\n") + scale_fill_brewer(palette = "Paired", name = "variable") +
  scale_x_discrete(labels = yearscale1)

#NP
test = Frag_directional_year1[c(1, 20:27)]
df = melt(test, id.vars = 'YEAR')

c2 = ggplot(df, aes(YEAR, value, fill = variable)) + geom_col(width = 0.8) + 
  geom_smooth(aes(as.numeric(YEAR), value), colour = 'black', se = FALSE) + 
  scale_y_continuous(position = 'right', sec.axis = dup_axis(), expand = expansion(mult = c(0, .1))) +
  facet_wrap(~ variable, ncol = 8) + myTheme + 
  theme(strip.text = element_blank(), axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank()) +
  ylab("No. of Patches\n") + scale_fill_brewer(palette = "Paired", name = "variable") +
  scale_x_discrete(labels = yearscale1)

#LPI
test = Frag_directional_year1[c(1, 38:45)]
df = melt(test, id.vars = 'YEAR')

c3 = ggplot(df, aes(YEAR, value, fill = variable)) + geom_col(width = 0.8) + 
  geom_smooth(aes(as.numeric(YEAR), value), colour = 'black', se = FALSE) + 
  scale_y_continuous(position = 'right', sec.axis = dup_axis(), expand = expansion(mult = c(0, .1))) +
  facet_wrap(~ variable, ncol = 8) + myTheme + 
  theme(strip.text = element_blank(), axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank()) +
  ylab("Largest Patch Index\n") + scale_fill_brewer(palette = "Paired", name = "variable") +
  scale_x_discrete(labels = yearscale1)

#FRAC_AM
test = Frag_directional_year1[c(1, 47:54)]
df = melt(test, id.vars = 'YEAR')

c4 = ggplot(df, aes(YEAR, value, fill = variable)) + geom_col(width = 0.8) + 
  geom_smooth(aes(as.numeric(YEAR), value), colour = 'black', se = FALSE) + 
  scale_y_continuous(position = 'right', sec.axis = dup_axis(), expand = expansion(mult = c(0, .1))) +
  facet_wrap(~ variable, ncol = 8) + myTheme + 
  theme(strip.text = element_blank(), axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank()) +
  ylab("Fractal Dimension\n") + scale_fill_brewer(palette = "Paired", name = "variable") +
  scale_x_discrete(labels = yearscale1)

#CONTIG_AM
test = Frag_directional_year1[c(1, 56:63)]
df = melt(test, id.vars = 'YEAR')

c5 = ggplot(df, aes(YEAR, value, fill = variable)) + geom_col(width = 0.8) + 
  geom_smooth(aes(as.numeric(YEAR), value), colour = 'black', se = FALSE) + 
  scale_y_continuous(position = 'right', sec.axis = dup_axis(), expand = expansion(mult = c(0, .1))) +
  facet_wrap(~ variable, ncol = 8) + myTheme + 
  theme(strip.text = element_blank(), axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank()) +
  ylab("Contagion Index\n") + scale_fill_brewer(palette = "Paired", name = "variable") +
  scale_x_discrete(labels = yearscale1)

#CLUMPY
test = Frag_directional_year1[c(1, 65:72)]
df = melt(test, id.vars = 'YEAR')

c6 = ggplot(df, aes(YEAR, value, fill = variable)) + geom_col(width = 0.8) + 
  geom_smooth(aes(as.numeric(YEAR), value), colour = 'black', se = FALSE) + 
  scale_y_continuous(position = 'right', sec.axis = dup_axis(), expand = expansion(mult = c(0, .1))) +
  facet_wrap(~ variable, ncol = 8) + myTheme + 
  theme(strip.text = element_blank(), axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank()) +
  ylab("Clumpiness Index\n") + scale_fill_brewer(palette = "Paired", name = "variable") +
  scale_x_discrete(labels = yearscale1)

#COHESION
test = Frag_directional_year1[c(1, 74:81)]
df = melt(test, id.vars = 'YEAR')

c7 = ggplot(df, aes(YEAR, value, fill = variable)) + geom_col(width = 0.8) + 
  geom_smooth(aes(as.numeric(YEAR), value), colour = 'black', se = FALSE) + 
  scale_y_continuous(position = 'right', sec.axis = dup_axis(), expand = expansion(mult = c(0, .1))) +
  facet_wrap(~ variable, ncol = 8) + myTheme + 
  theme(strip.text = element_blank(), axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank()) +
  ylab("Cohesion Index\n") + scale_fill_brewer(palette = "Paired", name = "variable") +
  scale_x_discrete(labels = yearscale1)

#SPLIT
test = Frag_directional_year1[c(1, 83:90)]
df = melt(test, id.vars = 'YEAR')

c8 = ggplot(df, aes(YEAR, value, fill = variable)) + geom_col(width = 0.8) + 
  geom_smooth(aes(as.numeric(YEAR), value), colour = 'black', se = FALSE) + 
  scale_y_continuous(position = 'right', sec.axis = dup_axis(), expand = expansion(mult = c(0, .1))) +
  facet_wrap(~ variable, ncol = 8) + myTheme + 
  theme(strip.text = element_blank(), axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank()) +
  ylab("Split ( x10^7)\n") + scale_fill_brewer(palette = "Paired", name = "variable") +
  scale_x_discrete(labels = yearscale1)
#Export

tiff('dir_year_300dpi.tiff', units="in", width=35, height=35, res=300, compression = 'lzw')
ggarrange(c1, c9, c2, c3, c4, c5, c6, c7, c8, nrow = 9, ncol = 1, common.legend = TRUE, legend = "bottom")
dev.off()


## 2. scatter ----

#CA
test = Frag_directional_year[c(1:9)]
df = melt(test, id.vars = 'YEAR')

c1 = ggplot(df, aes(YEAR,value)) + geom_point(size = 2) +
  geom_line(aes(color = "red"), size = 1.2) + geom_smooth(aes(color = 'blue'), se = FALSE) + 
  facet_wrap(~ variable, ncol = 8) + myTheme + theme(legend.position = c(0.8,0.8)) +
  scale_x_continuous(n.breaks = 7) +
  scale_colour_manual(values =c('red'='red','blue'='blue'), labels = c('Temporal variation','Trendline'))

#PLAND
test = Frag_directional_year[c(1, 11:18)]
df = melt(test, id.vars = 'YEAR')

c9 = ggplot(df, aes(YEAR,value)) + geom_point(size = 2) +
  geom_line(aes(color = "red"), size = 1.2) + geom_smooth(aes(color = 'blue'), se = FALSE) + 
  facet_wrap(~ variable, ncol = 8) + myTheme + theme(strip.text = element_blank(), legend.position = c(0.8,0.8)) +
  scale_x_continuous(n.breaks = 7) +
  scale_colour_manual(values =c('red'='red','blue'='blue'), labels = c('Temporal variation','Trendline'))

#NP
test = Frag_directional_year[c(1, 20:27)]
df = melt(test, id.vars = 'YEAR')

c2 = ggplot(df, aes(YEAR,value)) + geom_point(size = 2) +
  geom_line(aes(color = "red"), size = 1.2) + geom_smooth(aes(color = 'blue'), se = FALSE) + 
  facet_wrap(~ variable, ncol = 8) + myTheme + theme(strip.text = element_blank(), legend.position = c(0.8,0.8)) +
  scale_x_continuous(n.breaks = 7) +
  scale_colour_manual(values =c('red'='red','blue'='blue'), labels = c('Temporal variation','Trendline'))

#LPI
test = Frag_directional_year[c(1, 38:45)]
df = melt(test, id.vars = 'YEAR')

c3 = ggplot(df, aes(YEAR,value)) + geom_point(size = 2) +
  geom_line(aes(color = "red"), size = 1.2) + geom_smooth(aes(color = 'blue'), se = FALSE) + 
  facet_wrap(~ variable, ncol = 8) + myTheme + theme(strip.text = element_blank(), legend.position = c(0.8,0.8)) +
  scale_x_continuous(n.breaks = 7) +
  scale_colour_manual(values =c('red'='red','blue'='blue'), labels = c('Temporal variation','Trendline'))

#FRAC_AM
test = Frag_directional_year[c(1, 47:54)]
df = melt(test, id.vars = 'YEAR')

c4 = ggplot(df, aes(YEAR,value)) + geom_point(size = 2) +
  geom_line(aes(color = "red"), size = 1.2) + geom_smooth(aes(color = 'blue'), se = FALSE) + 
  facet_wrap(~ variable, ncol = 8) + myTheme + theme(strip.text = element_blank(), legend.position = c(0.8,0.8)) +
  scale_x_continuous(n.breaks = 7) +
  scale_colour_manual(values =c('red'='red','blue'='blue'), labels = c('Temporal variation','Trendline'))

#CONTIG_AM
test = Frag_directional_year[c(1, 56:63)]
df = melt(test, id.vars = 'YEAR')

c5 = ggplot(df, aes(YEAR,value)) + geom_point(size = 2) +
  geom_line(aes(color = "red"), size = 1.2) + geom_smooth(aes(color = 'blue'), se = FALSE) + 
  facet_wrap(~ variable, ncol = 8) + myTheme + theme(strip.text = element_blank(), legend.position = c(0.8,0.8)) +
  scale_x_continuous(n.breaks = 7) +
  scale_colour_manual(values =c('red'='red','blue'='blue'), labels = c('Temporal variation','Trendline'))

#CLUMPY
test = Frag_directional_year[c(1, 65:72)]
df = melt(test, id.vars = 'YEAR')

c6 = ggplot(df, aes(YEAR,value)) + geom_point(size = 2) +
  geom_line(aes(color = "red"), size = 1.2) + geom_smooth(aes(color = 'blue'), se = FALSE) + 
  facet_wrap(~ variable, ncol = 8) + myTheme + theme(strip.text = element_blank(), legend.position = c(0.8,0.8)) +
  scale_x_continuous(n.breaks = 7) +
  scale_colour_manual(values =c('red'='red','blue'='blue'), labels = c('Temporal variation','Trendline'))

#COHESION
test = Frag_directional_year[c(1, 74:81)]
df = melt(test, id.vars = 'YEAR')

c7 = ggplot(df, aes(YEAR,value)) + geom_point(size = 2) +
  geom_line(aes(color = "red"), size = 1.2) + geom_smooth(aes(color = 'blue'), se = FALSE) + 
  facet_wrap(~ variable, ncol = 8) + myTheme + theme(strip.text = element_blank(), legend.position = c(0.8,0.8)) +
  scale_x_continuous(n.breaks = 7) +
  scale_colour_manual(values =c('red'='red','blue'='blue'), labels = c('Temporal variation','Trendline'))

#SPLIT
test = Frag_directional_year[c(1, 83:90)]
df = melt(test, id.vars = 'YEAR')

c8 = ggplot(df, aes(YEAR,value)) + geom_point(size = 2) +
  geom_line(aes(color = "red"), size = 1.2) + geom_smooth(aes(color = 'blue'), se = FALSE) + 
  facet_wrap(~ variable, ncol = 8) + myTheme + theme(strip.text = element_blank(), legend.position = c(0.8,0.8)) +
  scale_x_continuous(n.breaks = 7) +
  scale_colour_manual(values =c('red'='red','blue'='blue'), labels = c('Temporal variation','Trendline'))

#Export

tiff('directional_year_300dpi.tiff', units="in", width=30, height=33, res=300, compression = 'lzw')
ggarrange(annotate_figure(c1, right = text_grob("Class Area (in ha)", size = 18, rot = 90, face = "bold")),
          annotate_figure(c9, right = text_grob("PLAND (in %)", size = 18, rot = 90, face = "bold")),
          annotate_figure(c2, right = text_grob("Number of Patches", size = 18, rot = 90, face = "bold")),
          annotate_figure(c3, right = text_grob("Largest Patch Index", size = 18, rot = 90, face = "bold")), 
          annotate_figure(c4, right = text_grob("Fractal Dimension", size = 18, 
                                                rot = 90, face = "bold")), 
          annotate_figure(c5, right = text_grob("Contagion Index", size = 18, 
                                                rot = 90, face = "bold")), 
          annotate_figure(c6, right = text_grob("Clumpiness Index", size = 18, rot = 90, face = "bold")), 
          annotate_figure(c7, right = text_grob("Cohesion Index (in %)", size = 18, rot = 90, face = "bold")), 
          annotate_figure(c8, right = text_grob("Split ( x10^7)", size = 18, rot = 90, face = "bold")),
          nrow = 9, ncol = 1, align = "hv", common.legend = TRUE)

#title <- expression(atop(bold("Temporal variation in FRAGSTAT metrics in different directions"), scriptstyle("Test")))
#annotate_figure(all.plots, top=text_grob(title))
dev.off()




