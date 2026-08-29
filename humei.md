humei calls notebook
================

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you
execute code within the notebook, the results appear beneath the code.

Try executing this chunk by clicking the *Run* button within the chunk
or by placing your cursor inside it and pressing *Cmd+Shift+Enter*.

``` r
#options(warn = -1) this chunk commented out as ebd file had a token in it
#library(geodata)
library(sf)
```

    ## Linking to GEOS 3.13.0, GDAL 3.8.5, PROJ 9.5.1; sf_use_s2() is TRUE

``` r
library(raster)
```

    ## Loading required package: sp

``` r
library(auk) #ebird download
```

    ## auk 0.8.0 is designed for EBD files downloaded after 2024-10-29. 
    ## No EBD data directory set, see ?auk_set_ebd_path to set EBD_PATH 
    ## eBird taxonomy version:  2024

``` r
#ebd_df <- read_ebd("ebd_humwar1_smp_relSep-2025.txt")

   
#month_number <- format(ebd_df$observation_date, "%m")
#humei_winter<-ebd_df[which(as.integer(month_number)>9 | as.integer(month_number)<4), ]
#humei_winter<-subset(humei_winter, country=="Nepal" | country=="India" | country=="Bhutan"| country=="Pakistan")

#indiaextent<-c(68, 95, 5,  37)
#trees<-raster("Crowther_Nature_Biome_Revision_01_WGS84_GeoTiff.tif") #see readme for how to obtain
#humtrees<-crop(trees, indiaextent)
#humtrees <- aggregate(humtrees, fun=sum, fact=5) #each cell 0.086*0.086 degrees =28.1 km2
#humtrees[humtrees<11000]<-1
#humtrees[humtrees>=11000]<-2
#plot(humtrees, col=c("grey","green"))
#x<-sample(62230, 1000)#sample out of ebird
#points(humei_winter[x,]$latitude~humei_winter[x,]$longitude, pch=16, cex=0.2)
```

``` r
hum<-read.csv("humei_files/Phylloscopus_humei.csv")
alt <- geodata::elevation_global(2.5, getwd())
indiaextent<- c(69,90,6,37)
india<-crop(alt,indiaextent)
z<-gray.colors(20, start = 0.3, end = 0.9, gamma = 3, alpha = 0.5)
plot(india, col=z)

hum$loc <- as.integer(interaction(hum$longitude, hum$latitude, drop = TRUE))
hum$ba2000<-cut(hum$Year, c(0, 1999.5, 2030))
ygt2000<-subset(hum, Year>1999.5)
ylt2000<-subset(hum, Year<1999.5)


ygt2000collapse<-data.frame(tapply(ygt2000$longitude, ygt2000$loc, function(x) mean(x)), tapply(ygt2000$latitude, ygt2000$loc, function(x) mean(x)), tapply(ygt2000$loc, ygt2000$loc, function(x) sum(x>0, na.rm=T)))
```

    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA

``` r
ylt2000collapse<-data.frame(tapply(ylt2000$longitude, ylt2000$loc, function(x) mean(x)), tapply(ylt2000$latitude, ylt2000$loc, function(x) mean(x)), tapply(ylt2000$loc, ylt2000$loc, function(x) sum(x>0, na.rm=T)))
```

    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA
    ## Warning in mean.default(x): argument is not numeric or logical: returning NA

``` r
colnames(ygt2000collapse)<-c("long", "lat", "count")
colnames(ylt2000collapse)<-c("long", "lat", "count")



points(ylt2000collapse$long, ylt2000collapse$lat, pch=16, cex=ylt2000collapse$count2/3, col="#D55E0088")
points(ygt2000collapse$long, ygt2000collapse$lat, pch=16, cex=ygt2000collapse$count2/3, col="#0072B288")
```

![](humei_files/figure-gfm/recording%20and%20playback%20locations,%20figS1A,B,C-1.png)<!-- -->

``` r
pbklocations<-read.csv("playbacks/pbklocation.csv")
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on 'playbacks/pbklocation.csv'

``` r
plot(india, col=z, axes = FALSE, box = FALSE, legend = FALSE)
```

![](humei_files/figure-gfm/recording%20and%20playback%20locations,%20figS1A,B,C-2.png)<!-- -->

``` r
plot(india, col=z, xlim = c(xmin(india), xmax(india)), ylim = c(ymin(india), ymax(india)))
points(pbklocations[,3:2], pch=16, col=c("red", "red", "blue"))
text(pbklocations[,3:2], pbklocations[,1], pos=4)
```

![](humei_files/figure-gfm/recording%20and%20playback%20locations,%20figS1A,B,C-3.png)<!-- -->

``` r
library(raster)
library(sf)
addsaturation<-function(hex, saturation){
library(colorspace)
base_hex <- substr(hex, 1, 7)
# Convert to HLS
col <- hex2RGB(base_hex)
col_hls <- as(col, "HLS")
# Increase saturation by saturation, but not above 1
col_hls@coords[2] <- pmin(col_hls@coords[2] * saturation, 1)
new_col <- as(col_hls, "RGB")
return(hex(new_col))}

#Code for estimating population sizes 
trees<-raster("Crowther_Nature_Biome_Revision_01_WGS84_GeoTiff.tif") #see readme for how to obtain
#humei shapefile
#in the terminal we used gdal to extract the humei shapefile
#ogr2ogr -f GPKG output.gpkg BOTW_2025.gpkg -where " sci_name = 'Phylloscopus humei'"

humei<-st_read("species/output.gpkg")
```

    ## Reading layer `all_species' from data source 
    ##   `/Users/pricet/github/humei/species/output.gpkg' using driver `GPKG'
    ## Simple feature collection with 2 features and 17 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 68.83818 ymin: 15.11969 xmax: 111.8452 ymax: 53.58047
    ## Geodetic CRS:  WGS 84

``` r
#plot(humei, max.plot=1)
bbox <- c(xmin = 83, ymin = 17, xmax = 115, ymax = 41)

#humeiBdg<-subset(humei, SEASONAL==2)
#humeiWin<-subset(humei, SEASONAL==3)


alt <- geodata::elevation_global(2.5, getwd())
alt<-raster(alt)
humeiextent<-c(xmin=60, xmax=120, ymin=0,  ymax=54)
humalt<-crop(alt,humeiextent)
#plot(humalt, col=gray(seq(0, 1, length = 250), alpha = 0.2))

mandelli_bbox <- st_as_sfc(st_bbox(c(xmin=87, xmax=120, ymin=15, ymax=42)))
 st_crs(mandelli_bbox)<- st_crs(humei)

 humeiBdg_bbox<-st_as_sfc(st_bbox(c(xmin=80, xmax=120, ymin=15, ymax=42)))
  st_crs(humeiBdg_bbox)<- st_crs(humei)
humeimap<-st_difference(humei, mandelli_bbox)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout
    ## all geometries

``` r
#plot(humeimap, max.plot=1)
humeiBdg<-subset(humeimap, seasonal==2)
humeiBdg<-st_difference(humeiBdg, humeiBdg_bbox)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout
    ## all geometries

``` r
humeiwin<-subset(humeimap, seasonal==3, max.plot=1)
```

    ## Warning: In subset.data.frame(humeimap, seasonal == 3, max.plot = 1) :
    ##  extra argument 'max.plot' will be disregarded

``` r
#plot(humeiBdg, col="#99D7F6", add=T, border=NA)
#plot(humeiwin, col="#F5C0CA", add=T, border=NA)


humtrees<-crop(trees, humeiextent)
humtrees <- aggregate(humtrees, fun=sum, fact=5) #each cell ~28km2
round(extent(humtrees))
```

    ## class      : Extent 
    ## xmin       : 60 
    ## xmax       : 120 
    ## ymin       : 0 
    ## ymax       : 54

``` r
hum<- stack(humtrees, humalt)


#Code for winter

humeiextentwin<- c(69,82,17,37) #approx wintering distribution
data<-crop(hum, humeiextentwin) 
data_area<-area(data)
#data_area[data[[2]] > 300] <- NA #this was not included in final run
data_area[data[[1]]/(100*data_area) < 4] <- NA #any area less than 4 trees/ha
data_area[is.na(data[[2]])] <- NA #removes water
sum(data_area[], na.rm=T)
```

    ## [1] 915536

``` r
#Code for breeding

humalt<-crop(alt,humeiBdg) 
humtrees<-crop(trees, humalt)
humtrees<-aggregate(humtrees, 5, sum)
extent(humtrees)<-extent(humalt)
hum<-stack(humtrees, humalt) #note here we use worldclim for simplicity but in the paper, actually used strm

#(a)    Himalaya

himbdgext<- c(65,95,25,36)
data<-crop(hum, himbdgext) 
data<-crop(data, humeiBdg)
data_area<-area(data)
treedens<- data[[1]]/(100*data_area)
data_area[data[[2]] < 2800] <- NA #any area below 2800
data_area[data[[2]] > 3400] <- NA #any area above 3400
data_area[treedens < 4] <- NA #any area less than 4 trees/ha
sum(data_area[], na.rm=T)
```

    ## [1] 29788.91

``` r
#(b)    North of Himalaya
sibbdgext<- c(65,95,36,54)
data<-crop(hum, sibbdgext) 
data<-crop(data, humeiBdg) 
data_area<-area(data)
treedens<- data[[1]]/(100*data_area)
data_area[treedens < 4] <- NA #any area less than 4 trees/ha
sum(data_area[], na.rm=T)
```

    ## [1] 934537.2

``` r
library(lme4)
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'lme4'

    ## The following object is masked from 'package:raster':
    ## 
    ##     getData

``` r
playbk<-read.csv("playbacks/VCWinter.csv")
#recordings<-read.csv("humei_files/Phylloscopus_humei.csv")
recordings<-read.csv("playbacks/Playbackfilescallrateandnoise.csv")
#str(playbk)
#str(recordings)
recordings<-recordings[,c(2,4)]
#play<-merge(playbk,recordings)
#play<-play[!duplicated(play), ]
#play<-data.frame(play[,1:4], play$Year)
fitpoisson <- glmer(Total.score.out.of.three~Year+(1|Recording.ID.Number)+(1|Playback),data=playbk,family=poisson)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
summary(fitpoisson)
```

    ## Warning in vcov.merMod(object, use.hessian = use.hessian): variance-covariance matrix computed from finite-difference Hessian is
    ## not positive definite or contains NA values: falling back to var-cov estimated from RX

    ## Warning in vcov.merMod(object, correlation = correlation, sigm = sig): variance-covariance matrix computed from finite-difference Hessian is
    ## not positive definite or contains NA values: falling back to var-cov estimated from RX

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: poisson  ( log )
    ## Formula: Total.score.out.of.three ~ Year + (1 | Recording.ID.Number) +  
    ##     (1 | Playback)
    ##    Data: playbk
    ## 
    ##       AIC       BIC    logLik -2*log(L)  df.resid 
    ##     176.5     186.2     -84.3     168.5        78 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -0.9894 -0.5994 -0.3618  0.3225  5.7486 
    ## 
    ## Random effects:
    ##  Groups              Name        Variance Std.Dev.
    ##  Recording.ID.Number (Intercept) 0.0000   0.0000  
    ##  Playback            (Intercept) 0.4256   0.6524  
    ## Number of obs: 82, groups:  Recording.ID.Number, 46; Playback, 41
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -143.93741   38.13180  -3.775 0.000160 ***
    ## Year           0.07135    0.01895   3.764 0.000167 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##      (Intr)
    ## Year -1.000
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## boundary (singular) fit: see help('isSingular')

``` r
#confounds tests
fitpoisson <- glmer(Total.score.out.of.three~Year+Number.of.Calls+(1|Recording.ID.Number)+(1|Playback),data=playbk,family=poisson)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## unable to evaluate scaled gradient

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge: degenerate Hessian with 1 negative eigenvalues

``` r
playbk$rate<-playbk$Number.of.Calls/playbk$duration...seconds.
fitpoisson <- glmer(Total.score.out.of.three~Year+rate+(1|Recording.ID.Number)+(1|Playback),data=playbk,family=poisson)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## unable to evaluate scaled gradient
    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge: degenerate Hessian with 1 negative eigenvalues

``` r
fitpoisson <- glmer(Total.score.out.of.three~Year+Number.of.Calls/extranuous.sounds+(1|Recording.ID.Number)+(1|Playback),data=playbk,family=poisson)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
fitCall <- glmer(Aggressive.Calls~Year+(1|Recording.ID.Number)+(1|Playback),data=playbk,family=binomial)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.18867 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

``` r
fitfly <- glmer(Fly.overs~Year+(1|Recording.ID.Number)+(1|Playback),data=playbk,family=binomial)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model failed to converge with max|grad| = 0.38671 (tol = 0.002, component 1)
    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

``` r
fitappr <- glmer(Direct.approach.towards.the.speaker~Year+(1|Recording.ID.Number)+(1|Playback),data=playbk,family=binomial)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
capture.output(summary(fitappr), file = "model_summary.txt")
playbk$calls<-rowSums(playbk[,18:22])
capture.output(summary(lm(log(playbk$calls+1)~playbk$Year)), file = "model_summary.txt")
plot(playbk$calls~playbk$Year, las=1, main="total calls", ylab="Calls", xlab="Year", bty="l", xlim=c(1980,2022))
```

![](humei_files/figure-gfm/playbacks%20VC%20winter,%20Fig.%203-1.png)<!-- -->

``` r
#mean scores
#mean(play$Total.score.out.of.three[play$play.Year>2009 & play$play.Year<2015], na.rm=T)
#mean(play$Total.score.out.of.three[play$play.Year>2015], na.rm=T)

#chisqr tests
chisq<-read.csv("playbacks/Vscoresbyyear.csv")
summary(table(chisq$Total.score.out.of.three, chisq$cut))
```

    ## Number of cases in table: 107 
    ## Number of factors: 2 
    ## Test for independence of all factors:
    ##  Chisq = 38.24, df = 6, p-value = 1.01e-06
    ##  Chi-squared approximation may be incorrect

``` r
summary(table(chisq$Total.score.out.of.three,chisq$GT2009))
```

    ## Number of cases in table: 107 
    ## Number of factors: 2 
    ## Test for independence of all factors:
    ##  Chisq = 31.368, df = 3, p-value = 7.113e-07

``` r
#check on number of calls
numcalls<-read.csv("playbacks/nocall.csv")
```

``` r
playbk<-read.csv("playbacks/VCSummer.csv")
fitpoisson <- glmer(Total.score.out.of.three~Year+(1|Playback),data=playbk,family=poisson)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
summary(fitpoisson)
```

    ## Warning in vcov.merMod(object, use.hessian = use.hessian): variance-covariance matrix computed from finite-difference Hessian is
    ## not positive definite or contains NA values: falling back to var-cov estimated from RX

    ## Warning in vcov.merMod(object, correlation = correlation, sigm = sig): variance-covariance matrix computed from finite-difference Hessian is
    ## not positive definite or contains NA values: falling back to var-cov estimated from RX

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: poisson  ( log )
    ## Formula: Total.score.out.of.three ~ Year + (1 | Playback)
    ##    Data: playbk
    ## 
    ##       AIC       BIC    logLik -2*log(L)  df.resid 
    ##      53.3      56.9     -23.6      47.3        22 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.0436 -0.4654 -0.2436  0.3153  2.0322 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  Playback (Intercept) 0        0       
    ## Number of obs: 25, groups:  Playback, 14
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -209.23279   59.61455  -3.510 0.000449 ***
    ## Year           0.10393    0.02956   3.516 0.000438 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##      (Intr)
    ## Year -1.000
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## boundary (singular) fit: see help('isSingular')

``` r
playbk<-read.csv("playbacks/VCSummer.csv")
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 4.4.3

``` r
library(viridis)
```

    ## Loading required package: viridisLite

``` r
#for poisson distributed errors; files playbk
p1<-ggplot(playbk, aes(x=Year,y=Total.score.out.of.three))
p2<-p1+ geom_count(aes(fill="#A4A4A433", alpha=0.5), shape = 21,colour = "black") +stat_smooth(method = "glm", formula = y ~ x, 
  method.args = list(family = "poisson")) + xlim(1980, 2023)    
p2 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black"))+theme(legend.position = "none",
    axis.text.x  = element_text(size = 16),  # x tick labels
    axis.text.y  = element_text(size = 16))
```

![](humei_files/figure-gfm/playback_plots-1.png)<!-- -->

``` r
p2<-p2+ylim(0,5)
p3<-prettyplot(p2, "Year of recording", "Response")
p3
```

![](humei_files/figure-gfm/playback_plots-2.png)<!-- -->

``` r
#binomial fits to VC winter, Aggressive.Calls OR Fly.overs OR Direct.approach.towards.the.speaker; or soces on 1-3, with poisson fit, as shown here


playbk<-read.csv("playbacks/VCWinter.csv")
p1<-ggplot(playbk, aes(x=Year,y=Total.score.out.of.three))
p2<-p1+ geom_count(aes(fill="#A4A4A433", alpha=0.5), shape = 21,colour = "black") +stat_smooth(method = "glm", formula = y ~ x,method.args = list(family = "poisson"))
p2 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+theme(legend.position = "none",
    axis.text.x  = element_text(size = 16),  # x tick labels
    axis.text.y  = element_text(size = 16)   # y tick labels
    
  )
```

![](humei_files/figure-gfm/binomial%20fits%20Fig%20S3C,E,F-1.png)<!-- -->

``` r
p2<-p2+ylim(0,5)
p3<-prettyplot(p2, "Year of recording", "Response")
p3
```

![](humei_files/figure-gfm/binomial%20fits%20Fig%20S3C,E,F-2.png)<!-- -->

``` r
pratap<-read.csv("playbacks/PSWinter.csv")
p1<-ggplot(pratap, aes(x=Year,y=Response))
p2<-p1+ geom_count(aes(fill="#A4A4A433", alpha=0.5), shape = 21,colour = "black") +stat_smooth(method = "glm", formula = y ~ x,method.args = list(family = "binomial"))
p2 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+theme(legend.position = "none",
    axis.text.x  = element_text(size = 16),  # x tick labels
    axis.text.y  = element_text(size = 16)   # y tick labels
  )
```

![](humei_files/figure-gfm/PS%20preliminary%20Fig.%20S3A-1.png)<!-- -->

``` r
pratap$recording<-as.factor(pratap$Recording.ID)
fit_bin <- glmer(Response~Year+(1|recording)+(1|male),data=pratap,family=binomial)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.165134 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
summary(fit_bin)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Response ~ Year + (1 | recording) + (1 | male)
    ##    Data: pratap
    ## 
    ##       AIC       BIC    logLik -2*log(L)  df.resid 
    ##      51.5      59.9     -21.7      43.5        57 
    ## 
    ## Scaled residuals: 
    ##       Min        1Q    Median        3Q       Max 
    ## -0.093253 -0.000077  0.000000  0.000531  0.134174 
    ## 
    ## Random effects:
    ##  Groups    Name        Variance Std.Dev.
    ##  recording (Intercept) 2148.9   46.36   
    ##  male      (Intercept)  905.2   30.09   
    ## Number of obs: 61, groups:  recording, 48; male, 31
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error  z value Pr(>|z|)    
    ## (Intercept) -2.977e+03  1.761e-03 -1690900   <2e-16 ***
    ## Year         1.480e+00  9.053e-04     1635   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##      (Intr)
    ## Year -0.001
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## Model failed to converge with max|grad| = 0.165134 (tol = 0.002, component 1)
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
split4<-read.csv("songmetrics/4split.csv")
split4[,7:10]<-split4[,7:10]/1000
head(split4[,3:6])
```

    ##         P1      P2       P3       P4
    ## 1 4220.508 4909.57 4220.508 4306.641
    ## 2 4125.000 4312.50 4312.500 3750.000
    ## 3 4125.000 4312.50 4125.000 3937.500
    ## 4 3750.000 4500.00 4687.500 3937.500
    ## 5 3937.500 4687.50 3750.000 3750.000
    ## 6 3937.500 5437.50 5062.500 4312.500

``` r
x2<-split4$Year+0.2
x<-split4$Year-0.2
plot(split4$C1~x2, pch=16, cex=1.5, xlab="Year", ylab="Center Frequency, khz", col="#04bec3", bty="l", las=1, ylim=c(3,6.5), yaxt = "n", xaxt = "n")
lines(lowess(split4$C1~split4$Year), col="#04bec3", lwd=4)
points(split4$C4~x, cex=1.5, col="#f97870", pch=16)
lines(lowess(split4$C4~split4$Year), col="#f97870", lwd=4)
points((split4$C3+split4$C2)/2~split4$Year, cex=1.5, col="#D3D3D399", pch=16)
lines(lowess((split4$C3+split4$C2)/2~split4$Year), col="grey", lwd=4)
axis(side = 2, at = seq(3, 6, by = 1), lwd=2)
```

![](humei_files/figure-gfm/song%20frequency%20figure%202D-1.png)<!-- -->

``` r
summary(lm(split4$C1~split4$Year))
```

    ## 
    ## Call:
    ## lm(formula = split4$C1 ~ split4$Year)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.09946 -0.37020 -0.02926  0.36229  1.88602 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -78.645677   6.541736  -12.02   <2e-16 ***
    ## split4$Year   0.041525   0.003259   12.74   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5327 on 212 degrees of freedom
    ## Multiple R-squared:  0.4337, Adjusted R-squared:  0.431 
    ## F-statistic: 162.4 on 1 and 212 DF,  p-value: < 2.2e-16

``` r
whole<-read.csv("~/github/humei/songmetrics/HumeiWholesongs.csv")
whole<-subset(whole, Call.type!="xx")

p3<-ggplot(whole, aes(x=Year,y=Center.Freq..Hz.))
p4<-p3 + geom_smooth() +geom_point(aes(colour = Call.type), size=1.8)
p4 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), legend.position="none", axis.line = element_line(colour = "black"))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](humei_files/figure-gfm/fig%202C&D-1.png)<!-- -->

``` r
p3<-ggplot(whole, aes(x=Year,y=Delta.Time..s.))
p4<-p3 + geom_smooth() +geom_point(aes(colour = Call.type), size=1.8)
p4 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), legend.position="none", axis.line = element_line(colour = "black"))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](humei_files/figure-gfm/fig%202C&D-2.png)<!-- -->

``` r
p4
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](humei_files/figure-gfm/fig%202C&D-3.png)<!-- -->

``` r
#y is center freqy, x is time
conc_exc<-function(y){
 z<-y[2:length(y)]-y[1:(length(y)-1)]
 x1<-z[which(y[2:length(y)]-y[1:(length(y)-1)]!=0)]
 concavity<-0
 for(i in 2: length(x1)) if(x1[i-1]*x1[i]<0) concavity=concavity+1
 excursion<-sum(abs(y[1:(length(y)-1)]-y[2:(length(y))]), na.rm=T)
 return(data.frame(concavity, excursion))}
  

names<-list.files("concavity/Humei_milisecond_splits")
results<-as.data.frame(matrix(nrow=length(names), ncol=5))
colnames(results)<-c("name", "concavityraw", "excursion","concavityspline0.6", "time")


setwd("~/github/humei/concavity/Humei_milisecond_splits")
for (i in 1:length(names)) {file<-read.table(names[i], skip=1, col.names=c("Selection","View","View2","Channel","Begin Time (s)","End Time (s)","Low Freq (Hz)","High Freq (Hz)","Center Freq (Hz)","Peak Freq (Hz)"))
#plot(file$Begin.Time..s., file$Center.Freq..Hz., type="l")
res<-conc_exc(file$Center.Freq..Hz.)
model<-smooth.spline(file$Begin.Time..s., file$Center.Freq..Hz., spar=0.6)
resSpline<-conc_exc(model$y)[1]
tottime<-file$End.Time..s.[nrow(file)]-file$Begin.Time..s.[1]
results[i,]<- data.frame(names[i], res, resSpline, tottime)}

years<-read.csv("~/github/humei/concavity/Fileyears.csv")
results<-merge(results, years)

p1<-ggplot(results, aes(x=year,y=excursion))
p2<-p1+ geom_count(aes(fill="#A4A4A433", alpha=0.5), shape = 21,colour = "black") +geom_smooth(method = lm)
p3<-prettyplot(p2, "Year", "Excursion")

p1<-ggplot(results, aes(x=year,y=concavityspline0.6))
p2<-p1+ geom_count(aes(fill="#A4A4A433", alpha=0.5), shape = 21,colour = "black") +stat_smooth(formula = y ~ x)
p2 + theme(
    axis.text.x  = element_text(size = 24),  # x tick labels
    axis.text.y  = element_text(size = 24)   # y tick labels
  )
```

    ## `geom_smooth()` using method = 'loess'

![](humei_files/figure-gfm/excursion%20and%20concavity-1.png)<!-- -->

``` r
p3<-prettyplot(p2, "Year", "Concavity")

results$stdconcavity<-results$concavityspline0.6/results$time
results$stdconcavity[results$stdconcavity>60]<-"NA"
results$stdconcavity<-as.numeric(results$stdconcavity)
```

    ## Warning: NAs introduced by coercion

``` r
p1<-ggplot(results, aes(x=year,y=stdconcavity))
p2<-p1+ geom_count(aes(fill="#A4A4A433", alpha=0.5), shape = 21,colour = "black") +stat_smooth(formula = y ~ x)
p2 + theme(
    axis.text.x  = element_text(size = 24),  # x tick labels
    axis.text.y  = element_text(size = 24)   # y tick labels
  )
```

    ## Warning: Removed 2 rows containing non-finite outside the scale range
    ## (`stat_sum()`).

    ## `geom_smooth()` using method = 'loess'

    ## Warning: Removed 2 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

![](humei_files/figure-gfm/excursion%20and%20concavity-2.png)<!-- -->

``` r
p3<-prettyplot(p2, "Year", "Standardized Concavity")
```

``` call
twelve<-read.csv("songmetrics/12split.csv")
twelvemeans<-apply(twelve[,18:29], 1, mean)
twelve[,18:29]<-twelve[,18:29]-twelvemeans
res<-princomp(twelve[,18:29], cor=T)
out<-data.frame(res$scores[,1:2], twelve)

#str(out)
p3<-ggplot(out, aes(x=Comp.1,y=Comp.2,color= Decades))
p4<-p3 + geom_smooth(method = lm)
p4 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black"))
```
