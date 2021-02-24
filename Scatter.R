library(ggplot2)
library(reshape2)
library(ggpubr)
library(plyr)

#lst_col = V3, V10, V17, V24, V31, V38, V45
# 1990 - V2:V8; 1995 - V9:V15; 2000 - V16:V22; 2005 - V23:V29; 2010 - V30:V36; 2015 - V37:V43; 2020 - V44:V50
#c('LST', 'Y_1990', 'LST_1995', 'Y_1995', 'LST_2000', 'Y_2000', 'LST_2005', 'Y_2005', 'LST_2010', 'Y_2010', 'LST_2015', 'Y_2015', 'LST_2020', 'Y_2020')


col_names = c("DBI", "EBBI", "LST", "NBUI", "NDBaI", "NDISI", "STTCI",
              "DBI_1995", "EBBI_1995", "LST_1995", "NBUI_1995", "NDBaI_1995", "NDISI_1995", "STTCI_1995",
              "DBI_2000", "EBBI_2000", "LST_2000", "NBUI_2000", "NDBaI_2000", "NDISI_2000", "STTCI_2000",
              "DBI_2005", "EBBI_2005", "LST_2005", "NBUI_2005", "NDBaI_2005", "NDISI_2005", "STTCI_2005",
              "DBI_2010", "EBBI_2010", "LST_2010", "NBUI_2010", "NDBaI_2010", "NDISI_2010", "STTCI_2010",
              "DBI_2015", "EBBI_2015", "LST_2015", "NBUI_2015", "NDBaI_2015", "NDISI_2015", "STTCI_2015",
              "DBI_2020", "EBBI_2020", "LST_2020", "NBUI_2020", "NDBaI_2020", "NDISI_2020", "STTCI_2020")


#
HDA_all = HDA_all[-1,] # for deleting first row
HDA_all[] <- lapply(HDA_all, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
}) #convert to numeric
colnames(KMA_all) = col_names 


#Define Theme
myTheme = theme(axis.text.y = element_text(size = 14, colour = "black", face = "bold"),
                axis.text.x = element_text(colour = "black", size = 14),
                axis.ticks.x = element_blank(),
                axis.title.x  = element_blank(),
                axis.title.y = element_blank(),
                panel.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black"),
                panel.spacing = unit(1.5, "lines"),
                panel.border = element_rect(colour = "black", fill=NA, size=1),
                strip.background = element_rect(fill = "white", colour = "black"),
                strip.text = element_text(face = "bold", size = 15))


#years = list(c(1:7, 8:14, 15:21, 22:28, 29:35, 36:42, 43:49))
# plotting
Test = KMA_all[1:7]
df.m = melt(Test, id.vars = 'LST')

# Calculate correlation for each group
cors <- ddply(na.omit(df.m), .(variable), summarise, cor = round(cor(LST, value), 3))


# Separate plots
# With labels: Run it for the first year only
c1 = ggplot(df.m, aes(LST,value)) + 
  geom_point() + 
  stat_smooth(method = 'lm', se = FALSE) +
  facet_wrap(~ variable, scales = "free", ncol = 6) + ylim(c(-2.5, 4)) +
  xlim(c(10,25)) + myTheme + scale_y_continuous(labels = scales::number_format(accuracy = 0.001, decimal.mark = '.')) +
  geom_text(data=cors, aes(label=paste("R = ", cor, sep="")), x=-Inf, y=Inf, hjust=-1.0, vjust=5, size = 5.5)

# Without labels
Test = KMA_all[43:49]
df.m = melt(Test, id.vars = 'LST_2020')

# Calculate correlation for each group
cors <- ddply(na.omit(df.m), .(variable), summarise, cor = round(cor(LST_2020, value), 3))

c7 = ggplot(df.m, aes(LST_2020,value)) + 
  geom_point() + 
  stat_smooth(method = 'lm', se = FALSE) +
  facet_wrap(~ variable, scales = "free", ncol = 6) +
  xlim(c(10,25)) + myTheme + theme(strip.text = element_blank()) + ylim(c(-1.2, 4)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.001, decimal.mark = '.')) +
  geom_text(data=cors, aes(label=paste("R = ", cor, sep="")), x=-Inf, y=Inf, hjust=-1.0, vjust=5, size = 5.5)


##----------
Test = KMA_all[8:14]
df.m = melt(Test, id.vars = 'LST_1995')

# Calculate correlation for each group
cors <- ddply(na.omit(df.m), .(variable), summarise, cor = round(cor(LST_1995, value), 3))

c2 = ggplot(df.m, aes(LST_1995,value)) + 
  geom_point() + 
  stat_smooth(method = 'lm', se = FALSE) +
  facet_wrap(~ variable, scales = "free", ncol = 6) + ylim(c(-1.2, 4)) +
  xlim(c(10,25)) + myTheme + theme(strip.text = element_blank()) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.001, decimal.mark = '.')) +
  geom_text(data=cors, aes(label=paste("R = ", cor, sep="")), x=-Inf, y=Inf, hjust=-1.0, vjust=5, size = 5.5)


Test = KMA_all[15:21]
df.m = melt(Test, id.vars = 'LST_2000')

# Calculate correlation for each group
cors <- ddply(na.omit(df.m), .(variable), summarise, cor = round(cor(LST_2000, value), 3))

c3 = ggplot(df.m, aes(LST_2000,value)) + 
  geom_point() + 
  stat_smooth(method = 'lm', se = FALSE) +
  facet_wrap(~ variable, scales = "free", ncol = 6) + ylim(c(-1.2, 4)) +
  xlim(c(10,25)) + myTheme + theme(strip.text = element_blank()) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.001, decimal.mark = '.')) +
  geom_text(data=cors, aes(label=paste("R = ", cor, sep="")), x=-Inf, y=Inf, hjust=-1.0, vjust=5, size = 5.5)


Test = KMA_all[22:28]
df.m = melt(Test, id.vars = 'LST_2005')

# Calculate correlation for each group
cors <- ddply(na.omit(df.m), .(variable), summarise, cor = round(cor(LST_2005, value), 3))

c4 = ggplot(df.m, aes(LST_2005,value)) + 
  geom_point() + 
  stat_smooth(method = 'lm', se = FALSE) +
  facet_wrap(~ variable, scales = "free", ncol = 6) + ylim(c(-1.2, 4)) +
  xlim(c(10,25)) + myTheme + theme(strip.text = element_blank()) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.001, decimal.mark = '.')) +
  geom_text(data=cors, aes(label=paste("R = ", cor, sep="")), x=-Inf, y=Inf, hjust=-1.0, vjust=5, size = 5.5)


Test = KMA_all[29:35]
df.m = melt(Test, id.vars = 'LST_2010')

# Calculate correlation for each group
cors <- ddply(na.omit(df.m), .(variable), summarise, cor = round(cor(LST_2010, value), 3))

c5 = ggplot(df.m, aes(LST_2010,value)) + 
  geom_point() + 
  stat_smooth(method = 'lm', se = FALSE) +
  facet_wrap(~ variable, scales = "free", ncol = 6) + ylim(c(-1.2, 4)) +
  xlim(c(10,25)) + myTheme + theme(strip.text = element_blank()) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.001, decimal.mark = '.')) +
  geom_text(data=cors, aes(label=paste("R = ", cor, sep="")), x=-Inf, y=Inf, hjust=-1.0, vjust=5, size = 5.5)


Test = KMA_all[36:42]
df.m = melt(Test, id.vars = 'LST_2015')

# Calculate correlation for each group
cors <- ddply(na.omit(df.m), .(variable), summarise, cor = round(cor(LST_2015, value), 3))
#cors = setNames(data.frame(c("DBI_2015", "EBBI_2015", "NBUI_2015", "NDBAI_2015", "NDISI_2015", "STTCI_2015"), 
#                           c(0.615, 0.776, 0.607, 0.163, -0.034, 0.879)), c("variable", "cor"))

c6 = ggplot(df.m, aes(LST_2015,value)) + 
  geom_point() + 
  stat_smooth(method = 'lm', se = FALSE) +
  facet_wrap(~ variable, scales = "free", ncol = 6) + ylim(c(-1.2, 4)) +
  xlim(c(10,25)) + myTheme + theme(strip.text = element_blank()) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.001, decimal.mark = '.')) +
  geom_text(data=cors, aes(label=paste("R = ", cor, sep="")), x=-Inf, y=Inf, hjust=-1.0, vjust=5, size = 5.5)



# Export
tiff('kma_lst_300dpi_test.tiff', units="in", width=30, height=20, res=300, compression = 'lzw')
ggarrange(annotate_figure(c1, right = text_grob("1990", size = 18, rot = 90, face = "bold")),
          annotate_figure(c2, right = text_grob("1995", size = 18, rot = 90, face = "bold")),
          annotate_figure(c3, right = text_grob("2000", size = 18, rot = 90, face = "bold")), 
          annotate_figure(c4, right = text_grob("2005", size = 18, rot = 90, face = "bold")), 
          annotate_figure(c5, right = text_grob("2010", size = 18, rot = 90, face = "bold")), 
          annotate_figure(c6, right = text_grob("2015", size = 18, rot = 90, face = "bold")), 
          annotate_figure(c7, right = text_grob("2020", size = 18, rot = 90, face = "bold")), 
          nrow = 7, ncol = 1, common.legend = FALSE, align = "hv")
dev.off()

# Alternate scatterplot
# change default ggplot2 color palette
opts <- options()  # save old options

plot_colr = colorRampPalette(c("#EE0000", "#0000FF", "#008B00", "#050505", "#8B008B", "#EE4000", 
                               "#BF3EFF", "#76EE00", "#FFA500", "#00BFFF"))

ggplot <- function(...) ggplot2::ggplot(...) + scale_color_brewer(palette="Set1")

col_names = c("DBI_1990", "DBI_1995", "DBI_2000", "DBI_2005", "DBI_2010", "DBI_2015", "DBI_2020",
              "EBBI_1990", "EBBI_1995", "EBBI_2000", "EBBI_2005", "EBBI_2010", "EBBI_2015", "EBBI_2020",
              "NBUI_1990", "NBUI_1995", "NBUI_2000", "NBUI_2005", "NBUI_2010", "NBUI_2015", "NBUI_2020",
              "LST_1990", "LST_1995", "LST_2000", "LST_2005", "LST_2010", "LST_2015", "LST_2020",
              "NDBaI_1990", "NDBaI_1995", "NDBaI_2000", "NDBaI_2005", "NDBaI_2010", "NDBaI_2015", "NDBaI_2020",
              "NDISI_1990", "NDISI_1995", "NDISI_2000", "NDISI_2005", "NDISI_2010", "NDISI_2015", "NDISI_2020",
              "STTCI_1990", "STTCI_1995", "STTCI_2000", "STTCI_2005", "STTCI_2010", "STTCI_2015", "STTCI_2020")


myTheme = theme(axis.text.y = element_text(size = 22, colour = "black", face = "bold"),
                axis.text.x = element_text(colour = "black", size = 22, face = "bold"),
                axis.ticks.x = element_blank(),
                axis.title.x  = element_blank(),
                axis.title.y = element_blank(),
                panel.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black"),
                panel.spacing = unit(1.5, "lines"),
                panel.border = element_rect(colour = "black", fill=NA, size=1.3),
                legend.key.size = unit(1, "cm"),
                legend.key = element_rect(fill = NA),
                legend.justification = "center", 
                legend.direction = "vertical",
                legend.background = element_rect(size = 1, linetype = "solid", colour = "black"),
                legend.position = c(0.21,0.5),
                legend.text = element_text(colour = "black", face = "bold", size = 22),
                legend.title = element_blank(),
                title = element_text(size = 22, face = "bold"), 
                plot.title = element_text(hjust = 0.5))

#show_col(hue_pal()(7))
# brewer.pal(7, "Set1") - show hex codes
# display.brewer.pal(9, "Dark2") - dispaly color palette
# in kma_all_1, LST - 22:28; STTCI - 43:49; DBI - 1:7; EBBI - 8:14; NBUI - 15:21; NDISI - 36:42; NDBaI - 29:35

sttci_kma = ggplot(Test[c()]) + 
  geom_point(aes(LST_1990, Y_1990, color = "1990,  R = 0.957"), size = 3) + 
  geom_point(aes(LST_1995, Y_1995, color = "1995,  R = 0.775"), size = 3) + 
  geom_point(aes(LST_2000, Y_2000, color = "2000,  R = 0.902"), size = 3) +
  geom_point(aes(LST_2005, Y_2005, color = "2005,  R = 0.885"), size = 3) +
  geom_point(aes(LST_2010, Y_2010, color = "2010,  R = 0.887"), size = 3) +
  geom_point(aes(LST_2015, Y_2015, color = "2015,  R = 0.843"), size = 3) +
  geom_point(aes(LST_2020, Y_2020, color = "2020,  R = 0.876"), size = 3) +
  geom_smooth(aes(LST_1990, Y_1990), se = FALSE, color = "#E41A1C", method = "lm") + 
  geom_smooth(aes(LST_1995, Y_1995), se = FALSE, color = "#377EB8", method = "lm") + 
  geom_smooth(aes(LST_2000, Y_2000), se = FALSE, color = "#4DAF4A", method = "lm") + 
  geom_smooth(aes(LST_2005, Y_2005), se = FALSE, color = "#984EA3", method = "lm") + 
  geom_smooth(aes(LST_2010, Y_2010), se = FALSE, color = "#FF7F00", method = "lm") + 
  geom_smooth(aes(LST_2015, Y_2015), se = FALSE, color = "#FFFF33", method = "lm") + 
  geom_smooth(aes(LST_2020, Y_2020), se = FALSE, color = "#A65628", method = "lm") + 
  xlim(10, 25) + myTheme + ggtitle(label = "KMA: STTCI vs LST")


# 1. Type 1
tiff('test1.tiff', units="in", width=33, height=25, res=300, compression = 'lzw')
ggarrange(dbi_kma, dbi_hda, ebbi_kma, ebbi_hda, 
          nbui_kma, nbui_hda, ndbai_kma, ndbai_hda,
          ndisi_kma, ndisi_hda, sttci_kma, sttci_hda, nrow = 3, ncol = 4)
dev.off()

# 2. Type 2
tiff('test2.tiff', units="in", width=33, height=30, res=300, compression = 'lzw')
ggarrange(dbi_kma, dbi_hda, ggparagraph(text = " ", color = "white"),
          ebbi_kma, ebbi_hda, 
          ggparagraph(text = " ", color = "white"), ggparagraph(text = " ", color = "white"), 
          ggparagraph(text = " ", color = "white"), 
          ggparagraph(text = " ", color = "white"), ggparagraph(text = " ", color = "white"),
          nbui_kma, nbui_hda, ggparagraph(text = " ", color = "white"),
          ndbai_kma, ndbai_hda, 
          ggparagraph(text = " ", color = "white"), ggparagraph(text = " ", color = "white"), 
          ggparagraph(text = " ", color = "white"), 
          ggparagraph(text = " ", color = "white"), ggparagraph(text = " ", color = "white"),
          ndisi_kma, ndisi_hda, ggparagraph(text = " ", color = "white"),
          sttci_kma, sttci_hda, nrow = 5, ncol = 5, 
          heights=c(1,0.15,1,0.15, 1), widths=c(1, 1, 0.15, 1, 1))
dev.off()


# 3. Type 3

tiff('test3.tiff', units="in", width=33, height=33, res=300, compression = 'lzw')
ggarrange(ggparagraph(text = "DBI", color = "black", size = 24), ggparagraph(text = " ", color = "white"), 
          ggparagraph(text = "EBBI", color = "black", size = 24),
          ggarrange(dbi_kma, dbi_hda), ggparagraph(text = " ", color = "white"),
          ggarrange(ebbi_kma, ebbi_hda), 
          
          ggparagraph(text = "NBUI", color = "black", size = 24), ggparagraph(text = " ", color = "white"), 
          ggparagraph(text = "NDBaI", color = "black", size = 24), 
          ggarrange(nbui_kma, nbui_hda), ggparagraph(text = " ", color = "white"),
          ggarrange(ndbai_kma, ndbai_hda), 
          
          ggparagraph(text = "NDISI", color = "black", size = 24), ggparagraph(text = " ", color = "white"), 
          ggparagraph(text = "STTCI", color = "black", size = 24), 
          ggarrange(ndisi_kma, ndisi_hda), ggparagraph(text = " ", color = "white"),
          ggarrange(sttci_kma, sttci_hda), nrow = 6, ncol = 3, 
          heights=c(0.15,1,0.15,1,0.15,1), widths=c(1, 0.15, 1))
dev.off()



##