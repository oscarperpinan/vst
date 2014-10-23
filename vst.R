
## Cargar en el orden correcto

  library(lattice)
  library(ggplot2)
  library(latticeExtra)
  library(zoo)

## Tema para =lattice=

  myTheme <- custom.theme.2(pch=19, cex=0.7,
                            region=rev(brewer.pal(9,
                                name = 'YlOrRd')),
                            symbol = brewer.pal(n=8,
                                name = "Dark2"))
  myTheme$strip.background$col='transparent'
  myTheme$strip.shingle$col='transparent'
  myTheme$strip.border$col='transparent'

## Escalas

  xscale.components.custom <- function(...){
      ans <- xscale.components.default(...)
      ans$top=FALSE
      ans}
  yscale.components.custom <- function(...){
      ans <- yscale.components.default(...)
      ans$right=FALSE
      ans}

## Establecemos opciones por defecto

  myArgs <- list(as.table=TRUE,
                 between=list(x=0.5, y=0.2),
                 xscale.components = xscale.components.custom,
                 yscale.components = yscale.components.custom)
  defaultArgs <- lattice.options()$default.args
  
  lattice.options(default.theme = myTheme,
                  default.args = modifyList(defaultArgs, myArgs))

## Aranjuez

  library(zoo)
  load('data/aranjuez.RData')

## lattice: =xyplot=

pdf(file="figs/aranjuez.pdf")
  ## The layout argument arranges panels in rows
  xyplot(aranjuez, layout=c(1, ncol(aranjuez)))
dev.off()

## ggplot2: =autoplot=

pdf(file="figs/aranjuezGG.pdf")
  autoplot(aranjuez) + facet_free()
dev.off()

## lattice: Función completa

pdf(file="figs/aranjuezXblocks.pdf")
  library(grid)
  library(latticeExtra)
  
  ## Auxiliary function to extract the year value of a POSIXct time
  ## index
  Year <- function(x)format(x, "%Y")
  
  xyplot(aranjuez, layout=c(1, ncol(aranjuez)),
         strip=FALSE,
         scales=list(y=list(cex=0.6, rot=0)),
         panel=function(x, y, ...){
           ## Alternation of years
           panel.xblocks(x, Year,
                         col = c("lightgray", "white"),
                         border = "darkgray")
           ## Values under the average highlighted with red regions
           panel.xblocks(x, y<mean(y, na.rm=TRUE),
                         col = "indianred1",
                         height=unit(0.1, 'npc'))
           ## Time series
           panel.lines(x, y, col='royalblue4', lwd=0.5, ...)
           ## Label of each time series
           panel.text(x[1], min(y, na.rm=TRUE),
                      names(aranjuez)[panel.number()],
                      cex=0.6, adj=c(0, 0), srt=90, ...)
           ## Triangles to point the maxima and minima 
           idxMax <- which.max(y)
           panel.points(x[idxMax], y[idxMax],
                        col='black', fill='lightblue', pch=24)
           idxMin <- which.min(y)
           panel.points(x[idxMin], y[idxMin],
                        col='black', fill='lightblue', pch=25)
         })
dev.off()

## ggplot2: acomodamos datos
## - ggplot2 necesita un =data.frame= en formato /long/: =fortify=

  timeIdx <- index(aranjuez)
  
  long <- fortify(aranjuez, melt=TRUE)

## ggplot2
## - Bandas de valores por debajo de la media

  ## Values below mean are negative after being centered
  scaled <- fortify(scale(aranjuez, scale=FALSE), melt=TRUE)
  ## The 'scaled' column is the result of the centering.
  ## The new 'Value' column store the original values.
  scaled <- transform(scaled, scaled=Value,
                      Value=long$Value)
  underIdx <- which(scaled$scaled <= 0)
  ## 'under' is the subset of values below the average
  under <- scaled[underIdx,]

## ggplot2

## - Bandas consecutivas de años: =xts::endpoints=

  library(xts)
  ep <- endpoints(timeIdx, on='years')
  N <- length(ep[-1])
  ## 'tsp' is start and 'tep' is the end of each band
  tep <- timeIdx[ep]
  tsp <- timeIdx[ep[-(N+1)]+1]
  ## 'cols' is a vector with the color of each band
  cols <- rep_len(c('gray', 'white'), N)

## ggplot2

## - Mínimos y máximos.

  minIdx <- timeIdx[apply(aranjuez, 2, which.min)]
  minVals <- apply(aranjuez, 2, min, na.rm=TRUE)
  mins <- data.frame(Index=minIdx,
                     Value=minVals,
                     Series=names(aranjuez))
  
  maxIdx <- timeIdx[apply(aranjuez, 2, which.max)]
  maxVals <- apply(aranjuez, 2, max, na.rm=TRUE)
  maxs <- data.frame(Index=maxIdx,
                     Value=maxVals,
                     Series=names(aranjuez))

## ggplot2: resultado

  ggplot(data=long, aes(Index, Value)) +
      ## Time series of each variable
      geom_line(colour = "royalblue4", lwd = 0.5) +
      ## Year bands
      annotate(geom='rect', ymin = -Inf, ymax = Inf,
                xmin=tsp, xmax=tep,
                fill = cols, alpha = 0.4) +
      ## Values below average
      geom_rug(data=under,
               sides='b', col='indianred1') +
      ## Minima
      geom_point(data=mins, pch=25) +
      ## Maxima
      geom_point(data=maxs, pch=24) +
      ## Axis labels and theme definition
      labs(x='Time', y=NULL) +
      theme_bw() +
      ## Each series has different panel and y-scale
      facet_free()

## Datos
## - Medidas de radiación solar en estaciones de Navarra.

  load('data/navarra.RData')

## lattice: =xyplot=

pdf(file="figs/navarra.pdf")
  avRad <- zoo(rowMeans(navarra, na.rm=1),
               index(navarra))
  pNavarra <- xyplot(navarra - avRad,
                     superpose=TRUE, auto.key=FALSE,
                     lwd=0.5, alpha=0.3,
                     col='midnightblue') 
  pNavarra
dev.off()

## =aspect= y =cut=
## - La recomendación general para transmitir adecuadamente el ratio de
##   cambio es elegir el ratio entre altura y anchura de la ventana
##   gráfica de forma que la orientación de los segmentos que componen la
##   serie estén centradas en 45 grados (/banking to 45/)
## - En =xyplot= se define con =aspect=, pero hay que usar el método
##   cut-and-stack para evitar figuras demasiado anchas.

pdf(file="figs/navarraBanking.pdf")
  xyplot(navarra - avRad,
         aspect='xy', cut=list(n=3, overlap=0.1),
         strip=FALSE,
         superpose=TRUE, auto.key=FALSE,
         lwd=0.5, alpha=0.3, col='midnightblue')
dev.off()

## =horizonplot=
## - Diferencias respecto de la media entre localidades

pdf(file="figs/navarraHorizonplot.pdf")
  library(latticeExtra)
  
  horizonplot(navarra-avRad,
              layout=c(1, ncol(navarra)),
              origin=0, colorkey=TRUE)
dev.off()

## =horizonplot=
## - Diferencias respecto a la media diaria interanual.

  Ta <- aranjuez$TempAvg
  timeIndex <- index(aranjuez)
  longTa <- ave(Ta, format(timeIndex, '%j'))
  diffTa <- (Ta - longTa)

## =horizonplot=
## - Usamos =cut= para dedicar un panel a cada año.

pdf(file="figs/diffTa_horizon.pdf")
  years <- unique(format(timeIndex, '%Y'))
  
  horizonplot(diffTa, cut=list(n=8, overlap=0),
              colorkey=TRUE, layout=c(1, 8),
              scales=list(draw=FALSE,
                  y=list(relation='same')),
              origin=0, strip.left=FALSE) +
      layer(grid.text(years[panel.number()],
                      x = 0, y = 0.1, 
                      gp=gpar(cex=0.8),
                      just = "left"))
dev.off()

## =splom= y =groups=

png(filename="figs/aranjuezSplom.png",res=600,height=4000,width=4000)
  load('data/aranjuez.RData')
  
  ## Red-Blue palette with black added (12 colors)
  colors <- c(brewer.pal(n=11, 'RdBu'), '#000000')
  ## Rearrange according to months (darkest for summer)
  colors <- colors[c(6:1, 12:7)]
  
  splom(~as.data.frame(aranjuez),
          groups=format(index(aranjuez), '%m'),
        auto.key=list(space='right', 
            title='Month', cex.title=1),
        pscale=0, varname.cex=0.7, xlab='',
          par.settings=custom.theme(symbol=colors,
              pch=19), cex=0.3, alpha=0.1)
dev.off()

## Reajustamos datos
## - Debemos pasar de format /wide/ a /long/ con =reshape=:

  aranjuezDF <- data.frame(aranjuez,
                           month=format(index(aranjuez),
                               '%m'))
  aranjuezRshp <- reshape(aranjuezDF,
                          direction='long',
                          varying=list(names(aranjuez)[1:3]),
                          v.names='Temperature',
                          times=names(aranjuez)[1:3],
                          timevar='Statistic')

## ggplot2

png(filename="figs/aranjuezFacetGrid.png",res=300,height=2000,width=2000)
  ggplot(data=aranjuezRshp,
         aes(Radiation, Temperature)) +
      facet_grid(Statistic ~ month) +
      geom_point(col='skyblue4',
                 pch=19, cex=0.5,
                 alpha=0.3) +
      geom_rug() +
      stat_smooth(se=FALSE, method='loess',
                  col='indianred1', lwd=1.2) +
      theme_bw()
dev.off()

## lattice

pdf(file="figs/aranjuezOuterStrips.pdf")
  useOuterStrips(xyplot(Temperature ~ Radiation | month * Statistic,
                        data=aranjuezRshp,
                        between=list(x=0),
                        col='skyblue4', pch=19,
                        cex=0.5, alpha=0.3)) +
      layer({
          panel.rug(..., col.line='indianred1',
                    end=0.05, alpha=0.6)
          panel.loess(..., col='indianred1',
                      lwd=1.5, alpha=1)
      })
dev.off()

## googleVis

## [[http://decastillo.github.io/googleVis_Tutorial/][Tutorial]]

library(googleVis)

## Ejemplo con datos de Navarra

navarraDF <- as.data.frame(navarra)
navarraDF <- stack(navarraDF)
navarraDF$ymd <- index(navarra)

navGVis <- gvisMotionChart(navarraDF,
                           idvar = 'ind', timevar='ymd')

plot(navGVis)

## rCharts
## [[http://ramnathv.github.io/rCharts/]]

library(rCharts)

## Highcharts

## [[http://www.highcharts.com/]]

aranjuezDF <- as.data.frame(aranjuez)
## Highcharts necesita que las fechas sean numéricas
aranjuezDF$tt <-
    as.numeric(as.POSIXct(index(aranjuez)))*1000
hp <- hPlot(TempAvg ~ tt, data = aranjuezDF,
            type = 'line')
hp$xAxis(type = 'datetime')
hp

## Morris

## http://morrisjs.github.io/morris.js/

mp <- mPlot(x = 'tt', y = c('TempAvg', 'TempMax'),
            data = aranjuezDF,
            type = 'Line')
## Ajustes para Morris            
mp$set(pointSize = 0, lineWidth = 1)
mp
