require(maptools)
require(sp)
require(raster)
require(rgeos)
require(rgdal)
library(argparse)
library(chron)
library(stringr)
library(fields)
library(RColorBrewer)
library(ggmap)

print("loaded packages")
#-------------- Functions -----------#
get_args <- function() {
  if (rstudioapi::isAvailable()) {
    sink()
    args <- list(
      "oakland_grid" = "input/oakland_grid_data.rds",
      "oakland_outline" = "input/oakland_outline.rds",
      "pp_predictions" = "output/predpol_drug_predictions.csv",
      "out_figure" = "output/results_figure.eps"
    )
    in_path <- "~/git/predictive-policing" #KL's Path
    setwd(in_path) #Make things easier to change later
    
  } else {
    #sink("output/plotResults.log")
    parser <- ArgumentParser()
    parser$add_argument("--oakland_grid", type="character", required=TRUE)
    parser$add_argument("--oakland_outline", type="character", required=TRUE)
    parser$add_argument("--pp_predictions", type="character", required=TRUE)
    parser$add_argument("--out_figure", type="character", required=TRUE)
  
    args <- parser$parse_args()
  }
  return(args)
}


args <- get_args()

print("loaded args")

# ----- Constants ----------------- #

ncolors <- 50
k <- 20
begin_predpol <- 0

# ----- Load Data ------------------ #
pp.results <- read.csv(args$pp_predictions, header=TRUE)

oak.shp <- readRDS(args$oakland_outline)
oakland.grid <- readRDS(args$oakland_grid)


# ----- Functions ------------------#
colorBins <- function(x, num.bins, scale=TRUE, breaks=NULL, alpha = .5){
  colors <- topo.colors(num.bins, alpha = alpha)
  if(scale){ breaks <- seq(0,1, length.out = num.bins-1)}
  if(is.null(breaks)){
    breaks <- num.bins -1
  }
  x.cut <- cut(x, breaks = breaks, ordered_result=TRUE)
  x.colors <- colors[as.factor(x.cut)]
  rng <- range(x, na.rm=TRUE)
  breaks <- rng[1] + seq(0, 1, length=num.bins)*(diff(rng))
  out <- list(colors=colors, x.colors=x.colors, breaks=breaks)
}

colorBinsRed <- function(x, num.bins, scale=TRUE, breaks=NULL){
  colors <- rgb(1, 0, 0, alpha =seq(0, 1, length.out=num.bins))

  if(scale){ breaks <- seq(0,1, length.out = num.bins-1)}
  if(is.null(breaks)){
    breaks <- num.bins -1
  }
  x.cut <- cut(x, breaks = breaks, ordered_result=TRUE)
  x.colors <- colors[as.factor(x.cut)]
  rng <- range(x, na.rm=TRUE)
  breaks <- rng[1] + seq(0, 1, length=num.bins)*(diff(rng))
  out <- list(colors=colors, x.colors=x.colors, breaks=breaks)
}

count.crimes <- function(x){
  if(is.null(x)){out <- 0}else{
    out <- nrow(x)
  }
  out
}

##--------- Make plots --------------##


##calculate which were the top k predicted bins for k (k=20)
monitored.bins <- apply(pp.results[,-1], 2, function(x){order(x, decreasing=TRUE)[1:k]})

##calculate number of days receiving targeted policing
monitored.bins.tab <- as.data.frame(table(c(monitored.bins, 1:length(oakland.grid)))-1) #adding on 1:num.bins so table includes value for every possible bin
targeted <- data.frame(days=monitored.bins.tab$Freq, id=names(oakland.grid))

##set up in format needed for use with ggmaps 
bb <- bbox(oak.shp)
grid <- lapply(oakland.grid@polygons, fortify)
grid <- do.call("rbind", grid)
grid2 <- merge(grid, targeted, by = 'id')

##make maps

map <- get_map(location=c(lon=mean(bb["x",]) + .02, lat=mean(bb["y",])+.042), zoom=12,
               maptype="roadmap")

#make plot of which locations had heaviest policing
map.targeted <- ggmap(map) + geom_polygon(aes(x = long,y = lat, group=group, fill=days, alpha = days), 
                          color=NA, data=grid2[grid2$days>0,]) + 
  scale_fill_gradient(low="white",high="red") + guides(alpha=FALSE) + 
  ggtitle("Number of Days with Targeted Policing") + xlab('') + ylab('')

pdf(file=args$out_figure)
print(map.targeted)
dev.off()

