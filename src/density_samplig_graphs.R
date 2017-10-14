

sampling_prin <- function(){
  
  TAM = 10
  ifile        <- paste0('data/SUMmat', TAM, '.dat')
  mat          <- read.table(file = ifile, header = F)
  densi.true   <- read.table(file = paste0('data/densidad_real_5000.dat'), header = T)
  mat    <- as.matrix(mat)
  mat    <- mat[1:200,]
  globalZ2= 1e-3
  alpha <- c(1.5000000, 0.7500000, 0.5000000, 0.3750000, 0.3000000,
             0.2500000, 0.2142857, 0.1875000)
  densi  <- simDen(mat,alpha,minVal=3.5,maxVal=7,interVals=30, globalZ= globalZ2)#,globalZ=1e-2
  x <- seq(3.5,7,length=30)
  densi <- rbind(x, densi)
  colnames(densi) <- c(paste0('x_', 1:(ncol(densi))))
  rownames(densi) <- c('x', paste0('densi_', 1:(nrow(densi)-1)))
  
  densi.estimated = densi
  gg <- sampling_comparison(densi.true, densi, msn = 'Maximum Entropy Approach')
  
  gg
}

###########################################################################
###############################################################################

simDen <- function(mat,alpha,minVal=0,maxVal=2,interVals=30,globalZ=1e-4)
{
  resultados=NULL
  x=seq(minVal,maxVal,length=interVals) #x o S.
  for (i in 1:dim(mat)[1])
  {
    den=SME_density(mat[i,],alpha,x,S=NULL,globalZ,sepa=15)
    resultados=rbind(resultados,den$densidadS)
  }
  return(resultados)
}

SME_density<-function(lambda,alpha,x,S=NULL,globalZ=1e-3,sepa=15)
{
  #globalZ:sensibilidad de Z
  #Calculation of Z
  y<- seq(from=0,to=1,by=globalZ)
  f_y=sum_exp(lambda,alpha,y)
  #print(f_y)
  #Z<-sintegral(y,f_y)$value #Z normalization parameter
  Z<-trapz(y,f_y)
  if (is.na(Z)==TRUE)
  {
    cat("Change variable GlobalZ","\n")
    stop()
    geterrmessage() 
    return(0)
  }
  
  
  #Calculation of the final density
  f_x=sum_exp(lambda,alpha,exp(-x))
  densidad=exp(-x)*(f_x)*(1/Z)
  
  
  ##########
  #GRAPHS  #
  ##########
  
  if (length(S)!=0)
  {
    x11()
    plot(x,densidad,ylim = c(0,max(densidad)+0.1),ylab="density",xlab="values",lwd =2,type="o")
  }
  
  if (length(S)!=0) #need more data for the graphs
  {
    x11()
    sep1=25 
    d=hist(S[S>0],breaks=sep1,freq=FALSE,include.lowest = TRUE,ylim = c(0,0.6),
           xlab="S", main = paste("")) #main = paste("20 bars")
    plot(x,densidad,ylim = c(0,max(densidad)+0.1))
    lines(d$mids,d$density,type="l",lwd = 3,col="red")
    
    #PAPER
    x11()
    sep=sepa #15 #set 9 o 10
    par(ps = 12, cex.lab = 1.5, cex.main = 1.5) #, cex.main = .5,cex=.5,cex.lab=.5, cex.axis=.5, cex.main=.5
    d=hist(S[S>0],breaks=sep,freq=FALSE,include.lowest = TRUE,
           ylim = c(0,max(c(densidad,d$density))+0.1), xlim=c(min(c(S[S>0])), max(c(S[S>0],x))),
           xlab="S", main = paste("")) #main = paste("20 bars")
    lines(spline(x,densidad),col="black",lwd =3,lty=2) 
    par(ps = 12,cex=1.5)
    legend(max(c(S[S>0],x[densidad>0]))-7,max(c(densidad,d$density))-0.02, c("SME"), lty=c(2), 
           lwd=c(2),col=c("black")) 
    
  }
  
  #OUTPUT
  return(list(Z=Z, densidadS=densidad))
  
}

sum_exp <- function(lambda,alpha,y)
{
  #verifications
  # lambda,alpha size bigger than 0
  if (length(alpha)<=0 | length(lambda)<=0 | length(y)<=0) return(1001)
  
  if (length(alpha)!=length(lambda)) return(1001)
  
  
  #empiezan los calculos
  #y<-seq(0,1,length=100)
  sumfy<-rep(0,length(y))
  for (j in 1:length(alpha)) 
  {
    sfy<-lambda[j]*(y^alpha[j])
    sumfy<-sumfy+sfy
    sfy<-NULL
  }
  
  f<-exp(-sumfy) 
  
  return(f)
  
}

###########################################################################
###############################################################################


#
# This script contains the functions used to graph multiple densities generated for methods like convolutions, maxent and others, 
#using sampling.

require(data.table)
library(plotly)
require(ggplot2)


# GRAPH
sampling_comparison <- function(densi.true, densi.estimated, msn){
  
  X_estimated     <- densi.estimated[1, ]
  X_TRUE          <- densi.true[, 1]
  densi.estimated <- densi.estimated[-1, ]
  
  #discard those row that are all zero
  dt.densi.true  <- as.data.table(densi.true)
  densi.estimated <- densi.estimated[which(apply(densi.estimated, 1, sum) != 0), ]
  
  #average of the reconstructions
  average  <- apply(densi.estimated, 2, mean) 
  # densities that serves like frontiers
  lin_inf <- apply(densi.estimated, 2, min) 
  lin_sup <- apply(densi.estimated, 2, max) 
  
  dt.cross <- as.data.table(cbind(as.vector(as.numeric(X_estimated)), lin_inf, lin_sup, average))
  setnames(dt.cross, names(dt.cross), c('x_est', 'lin_inf', 'lin_sup', 'average'))
  dt.cross <- interpolate_matrix(dt.cross, n = 100)
  dt.cross <- dt.cross[, col := 'True Density'][, col2 := 'Reconstructions']
  
  names(dt.densi.true) <- c('x', 'true_density')
  if(nrow(dt.densi.true) < nrow(dt.cross)){
    dt.densi.true <- interpolate_values(dt.densi.true, nrow(dt.cross))
  }
  dt.densi.true        <- dt.densi.true[, col3 := 'Average']
  
  gg <- gg_graph(dt.cross, dt.densi.true, msn)
  
  return(gg)
}

#---------------------------------------------------------------------------------------------------------------

gg_graph <- function(dt.cross, dt.densi.true, msn){
  
  dt <- cbind(dt.cross, dt.densi.true)
  gg <- ggplot(dt, aes(x = x_est, ymin = lin_inf, ymax = lin_sup, col = col2)) +
    geom_ribbon(fill = 'gray', alpha= 0.8) 
  gg <- gg + geom_line(aes(x = x_est, y = average, col = col), size = 1)
  gg <- gg +  geom_line(aes(x = x, y = true_density, color = col3), size = 1)
  
  #gg <- gg + scale_color_manual("Click on the colors",values=c("red", "gray",  "black"))
  gg <- gg + scale_color_manual("",values=c("red", "gray",  "black"))
  gg <- gg + fte_theme() + theme(legend.position=  c(.9, .9))
  gg <- gg + xlab('x') + ylab('density')
  gg <- gg + ggtitle(msn)
  # gg <- ggplotly(gg, tooltip = c("y"), dynamicTicks = T)
  # gg$layout$height = 100
  # gg$layout$width = 100
  gg <- ggplotly(gg, session="knitr")
  gg
  # chart_link = api_create(gg, filename="ggplot-user-guide/2")
  # chart_link
  return(gg)
}

interpolate_matrix <- function(dt, n){
  s.n <- names(dt)
  dt.aux <- copy(dt)
  dt.n <- NULL
  if (ncol(dt) > 2){
    vect <- c('x', paste0('y', 2:ncol(dt)))
    for (i in 2:ncol(dt)){
      dt.aux <- dt[, c(1, i), with = F]
      dt.aux <- interpolate_values(dt.aux, n)
      if (i ==2){
        dt.n <- dt.aux[, c(1,2), with = F]
      }else{
        dt.n <- cbind(dt.n, dt.aux[, c(2), with = F])
      }
    }
  }
  return(dt.n)
}

interpolate_values <- function(dt, n){
  s.n       <- names(dt)
  names(dt) <- c('x', 'y')
  dt        <- spline(dt$x, dt$y, n)
  dt        <- as.data.table(cbind(dt$x, dt$y))
  names(dt) <- s.n
  
  return(dt)
}

#--------------------------------------------------------------------------------------------------------------------------------------
# alpha       <- c(1.5000000, 0.7500000, 0.5000000, 0.3750000, 0.3000000,
#                  0.2500000, 0.2142857, 0.1875000)
# lambda_5000 <- c(16240.12132,  -748.02989, -5997.50028,  1105.38763,  
#                  4745.65939,  3727.93062, -149.98752, -5356.22172)
# 
# 
# maxent_density <- function(maxent_results, ){
# 
#   x            <- sort(S[S>0])
#   F_SME        <- F_diff_new(x, lambda_5000, alpha, globalZ = 1e-5)
#   den_5000     <- F_SME$F_emp
# }

#----------------------------------------------------------------------------------------------------------------------------------------

fte_theme <- function() {
  
  require('RColorBrewer')
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[9]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  # Begin construction of chart
  theme_bw(base_size=9) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    #theme(legend.position=c(0.5, 0.5)) +
    theme(legend.position= 'none')  + 
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=18,color=color.axis.title)) +
    
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=20, vjust=1.25)) +
    theme(axis.text.x=element_text(size= 16,color=color.axis.text)) +
    theme(axis.text.y=element_text(size= 14,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=20,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=16,color=color.axis.title, vjust=0)) # +
  
  # Plot margins
  theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

#-----------------------------------------------------------------------------------------------------------------------------------