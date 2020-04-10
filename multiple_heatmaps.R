# load and install necessary packages
install.packages("pacman")
library(pacman)
pacman::p_load(gridGraphics,grid,gridExtra,gplots,lubridate, install = TRUE)

file = "I:/SWAT_RUN_COMPARISON/heatmap-1982-2100.xlsx"

## load all sheets
sheets <- openxlsx::getSheetNames(file)
SheetList <- lapply(sheets,openxlsx::read.xlsx,xlsxFile=file)
names(SheetList) <- sheets


## create color ramp and colour breaks
col_breaks <- c(-2,-1.5,-1,0,1,1.5,2) #provide col breaks
my_palette<-colorRampPalette(c("red4","red1","darkorange2","gold2",
                               "yellow4","yellow",
                               "chartreuse3","green4")) #color ramp

#Define a new function to create Row labels
rname <- function(i){
  if (i <= 3)
    rname = format(ymd(seq(as.Date("1982-01-01"), as.Date("2017-12-01"), by = "months")), "%b-%Y")
  else
    rname = format(ymd(seq(as.Date("2021-01-01"), as.Date("2100-12-01"), by = "months")), "%b-%Y")
}

grab_grob <- function(){
  grid.echo()
  grid.grab()
}

g1 = lapply(1:6, function(i) {
  arr = as.data.frame(SheetList[i])
  heatmap.2(as.matrix(arr[2:7]),
            dendrogram ='none',
            Colv=FALSE, Rowv = FALSE, 
            key=FALSE, keysize=1.0, symkey=FALSE, density.info='none',
            trace='none',
            scale="none",cexRow=0.7,cexCol=0.9,
            breaks = col_breaks,col=my_palette,
            labRow = rname(i),
            labCol = c("spei-3", "spei-6", "spei-9", "spei-12", "spei-15", "spei-24"),
            colsep = 0:3, sepwidth = c(0.01),
            sepcolor = c("grey")
  )

  grab_grob()
}
)


grid.newpage()
title1 = textGrob("DROUGHT PROGRESSION", gp=gpar(fontface="bold", fontsize = 22), vjust = 0.7)
grid.arrange(grobs = g1, ncol = 3, clip = TRUE, top = title1)
