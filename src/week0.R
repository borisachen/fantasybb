
library(mclust)
library(ggplot2) 

thisweek = 0
download = TRUE

### make sure input / output directories exist

mkdir <- function(dir) system(paste("mkdir -p", dir))
datdir = "~/projects/fantasybb/dat/"; mkdir(datdir)
outputdir = paste("~/projects/fantasybb/out/week", thisweek, "/", sep=""); mkdir(outputdir)

### Curl data from fantasypros

pos.list = c('hitters', 'pitchers', '1b', '2b', '3b', 
             'ss', 'c', 'of', 'dh', 'sp', 'rp', 'overall')
             
if (download == TRUE) {
  for (mp in pos.list) {
    curlstr = paste('curl http://www.fantasypros.com/mlb/rankings/',mp,
                    '.php?export=xls > ~/projects/fantasybb/dat/week-', 
                    thisweek, '-',mp,'-raw.xls', sep="")
    system(curlstr); Sys.sleep(1)
    sedstr = paste("sed '1,4d' ~/projects/fantasybb/dat/week-", thisweek, '-',mp,'-raw.xls', 
                   ' > ~/projects/fantasybb/dat/week_', thisweek, '_', mp, '.tsv',sep="")
    system(sedstr); Sys.sleep(1)
  }	
}


### main plotting function

error.bar.plot <- function(pos="NA", low=1, high=24, k=8, format="NA", title="dummy", tpos="QB", dat) {
  title=paste("Pre-draft ",tpos," Tiers", ' - Ranks ',low,'-',high, sep="")
  this.pos = dat
  this.pos = this.pos[low:high,]
  this.pos$ADP <- this.pos$X <- NULL
  
  this.pos$position.rank <- low+c(1:nrow(this.pos))-1	
  this.pos$position.rank = -this.pos$position.rank
  
  # Find clusters!
  df = this.pos[,c(which(colnames(this.pos)=="Ave.Rank"))]
  mclust <- Mclust(df, G=k)
  this.pos$mcluster <-  mclust$class
  
  # Print out names!
  fileConn<-file(paste(outputdir,"text_",tpos,".txt",sep=""))
  tier.list = array("", k)
  for (i in 1:k) {
    foo <- this.pos[this.pos $cluster==i,]
    foo <- this.pos[this.pos $mcluster==i,]
    es = paste("Tier ",i,": ",sep="")
    for (j in 1:nrow(foo)) es = paste(es,foo$Player.Name[j], ", ", sep="")
    es = substring(es, 1, nchar(es)-2)
    print(es)
    tier.list[i] = es
  }
  writeLines(tier.list, fileConn)
  close(fileConn)
  this.pos$nchar = nchar(as.character(this.pos$Player.Name))
  this.pos$Tier = factor(this.pos$mcluster)
  Tier = factor(this.pos$mcluster)
  
  bigfont   = c("1B","2B","3B", 'SS', 'C', 'DH')
  smfont = c("RP")
  tinyfont  = c("SP", 'OVERALL', 'OF')
  
  if (tpos %in% bigfont) {font = 3.5; barsize=1.5;  dotsize=2;   }
  if (tpos %in% smfont)  {font = 3;   barsize=1.25; dotsize=1.5; }
  if (tpos %in% tinyfont){font = 2.5; barsize=1;    dotsize=1;   }
  
  p = ggplot(this.pos, aes(x=position.rank, y=Ave.Rank))
  p = p + ggtitle(title)
  p = p + geom_errorbar(aes(ymin=Ave.Rank-Std.Dev/2, ymax= Ave.Rank + Std.Dev/2, 
                            width=0.2, colour=Tier), size=barsize*0.8, alpha=0.4)
  p = p + geom_point(colour="grey20", size=dotsize) 
  p = p + coord_flip()
  p = p + annotate("text", x = Inf, y = -Inf, label = "www.borischen.co", hjust=1.1, 
                   vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
  if (tpos %in% bigfont)     			
    p = p + geom_text(aes(label=Player.Name, colour= Tier, y = Ave.Rank - nchar/6 - Std.Dev/1.4), size=font)
  if (tpos %in% smfont)     			
    p = p + geom_text(aes(label=Player.Name, colour= Tier, y = Ave.Rank - nchar/5 - Std.Dev/1.5), size=font) 
  if (tpos %in% tinyfont)     			
    p = p + geom_text(aes(label=Player.Name, colour=Tier, y = Ave.Rank - nchar/3 - Std.Dev/1.8), size=font) 
  p = p + scale_x_continuous("Weight Adjusted Expert Concensus Rank")
  p = p + ylab("Average Expert Rank")
  p = p + theme(legend.justification=c(1,1), legend.position=c(1,1))
  p = p + scale_colour_discrete(name="Tier")
  p = p + scale_colour_hue(l=55, h=c(0,280))
  maxy = max( abs(this.pos$Ave.Rank)+this.pos$Std.Dev/2) 
  p = p + ylim(low-high/10, maxy)
  outfile = paste(outputdir, "week-", thisweek, "-", tpos,'-',low,'-',high, ".png", sep="")
  p
  ggsave(file=outfile, width=9.5, height=8, dpi=100)
  return(p)
}

draw.tiers <- function(pos, low, high, k) {
  file = paste(datdir, "week_", thisweek, "_", pos, ".tsv",sep="")
  dat = read.delim(file, sep="\t")
  dat <- dat[!dat$Player.Name %in% injured,]
  tpos = toupper(pos); if(pos=="flex")tpos<-"Flex"
  error.bar.plot(low = low, high = high, k=k, tpos=tpos, dat=dat)
}

### if there are any injured playes to remove, list them here
injured <- c('')

draw.tiers("overall", 1, 50, 10)
draw.tiers("overall", 50, 100, 10)
draw.tiers("overall", 100, 150, 10)
draw.tiers("overall", 150, 200, 10)

draw.tiers("1b", 1, 30, 10)
draw.tiers("2b", 1, 30, 10)
draw.tiers("3b", 1, 30, 10)
draw.tiers("ss", 1, 30, 10)
draw.tiers("c", 1, 25, 8)
draw.tiers("of", 1, 60, 15)
draw.tiers("dh", 1, 20, 5)
draw.tiers("sp", 1, 60, 15)
draw.tiers("rp", 1, 50, 9)

