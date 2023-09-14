########################supporting functions##############################
# supporting functions for x-section models of the Texel models
# several functions need to be loaded into the markdown files 
# by "sourcing" them at the beginning of the Rmd.
#                               G. Bier September 2023
########################supporting functions##############################
library(IJPLOT) # for generating spatial or temporal plots with use of sliders (from Paul Trofs)


##################################vegetation related function##################################
Calc.Tpi = function(ETp,fi)
{
  return((ETp + 0.0000001) * fi) #the small addition should be removed GB dd 4/9/23
}


Calc.Tai.Tpi = function(z,ai,zi)
{
  bi = 4*ai^2/(ai^2-1)*((ai-1)/(ai+1))^(1/ai)
  Tai.Tpi = bi* (z/zi)^(ai-1)/(1+(z/zi)^ai)^2
  return(Tai.Tpi)  
}


Calc.dfi.dt = function(Tai.Tpi,fi.old,f0.old,di)
{
  dfi.dt = Tai.Tpi * fi.old * f0.old - di * fi.old
  return(dfi.dt)
}

plot.taipai = function()
{
  fractions = range(c(TaiTpi.f1,TaiTpi.f2))
  plot(nodes,TaiTpi.f1,type = "o",
       ylim = fractions, main = "TaiTpi fraction")
  lines(nodes,TaiTpi.f2, type = "b",col = "red")
  grid()
  abline(v=nodes[which.max(TaiTpi.f1)])  
  abline(v=nodes[which.max(TaiTpi.f2)], col = "red")  
  grid()
  legend("topright",legend = c("fractions f1","fractions f2"),lty = "solid",lwd = 2, col = c("black","red"))
}

plot.fractions = function()
{
  fractions = range(c(f1.fun(nodes), f2.fun(nodes), f0.fun(nodes)))
  plot(nodes, f1.fun(nodes), type = "o",lwd = 2,
       main = "Vegetation fractions",ylim = fractions,
       ylab = "vegetation/soil fraction")
  lines(nodes, f2.fun(nodes), type = "b", col = "red",lwd = 2)
  lines(nodes, f0.fun(nodes), type = "b", col = "brown",lwd = 2)
  abline(v=nodes[which.max(f1.fun(nodes))])
  abline(v=nodes[which.max(f2.fun(nodes))], col = "red")
  abline(v=nodes[which.max(f0.fun(nodes))], col = "brown")
  grid()
  legend("topright",legend = c("f1","f2","f0"), lty = "solid",lwd = 2,col = c("black","red","brown"))
}


##################################vegetation related function##################################


plot.wtable = function(mod.name)
{
  df.state = dataframe.states(mod.name)
  plot(flow.domain.x,flow.domain.y, xlab = "Distance (m)",ylab = "Elevation (m)", main = "Water table postion (red)")
  polygon(flow.domain.x,flow.domain.y,col = "lightblue",lwd = 2)
  lines(df.state$x,df.state$state, col = "red",lwd =2)
  grid()
}

plot.data.wtable = function(data.states, time.step)
{
  plot(flow.domain.x,flow.domain.y, xlab = "Distance (m)",ylab = "Elevation (m)",
       main = paste("Water table position (red), day :", as.Date(as.character(meteo$yearmonthday[time.step]),"%Y%m%d")))
  polygon(flow.domain.x,flow.domain.y,col = "lightblue",lwd = 2)
  lines(nodes,data.states[time.step,], col = "red",lwd =2)
  grid()
  
}  

plot.trans.wtable = function(data.states, time.step)
{
  df.state = dataframe.states(data.states)
  plot(flow.domain.x,flow.domain.y, xlab = "Distance (m)",ylab = "Elevation (m)",
       main = paste("Water table location (red), day :", as.Date(as.character(meteo$yearmonthday[time.step]),"%Y%m%d")))
  polygon(flow.domain.x,flow.domain.y,col = "lightblue",lwd = 2)
  lines(df.state, col = "red",lwd =2)
  grid()
  
}

plot.external = function(model.name)
{
  df.ext = dataframe.externalfluxes(model.name)
  nr.wbterms = (length(df.ext[1,])-1)/2
  kleur = rainbow(n=nr.wbterms)
  y.range = range(df.ext[,2:(1+nr.wbterms)]*1000)
  plot(nodes,df.ext[,2]*1000, type = "b",lwd=2, col=kleur[1],
       ylim = y.range, ylab = "Flux rate mm/d",xlab = "Distance", main = "Flux rates of external fluxes in mm/d") #drainage
  for (i in 1:nr.wbterms)
  {
  lines(nodes,df.ext[,(1+i)]*1000, type = "b",lwd = 2, col= kleur[i])
  }
  grid()
  legend("bottomright",legend = names(df.ext[1,2:(1+nr.wbterms)]), col = kleur, lwd = 2, horiz = FALSE)
  # precip.sum = paste("ppt m2/d: ",as.character(round(sum(df.ext[,9]),digits = 2))) #sum precipitation
  # drnage.sum = paste("drn m2/d: ",as.character(round(sum(df.ext[,6]),digits = 2))) #sum drainage
  # evptrn.sum = paste("evp m2/d: ",as.character(round(sum(df.ext[,3]),digits = 2))) #sum evapotranspiration
  # q.legend = c(precip.sum,drnage.sum,evptrn.sum)
  # legend("bottomleft",q.legend, col= c("green","blue","red"), lwd = 3, horiz = FALSE)
}

plot.wbudget = function(wb.name)
{
  wb.range = range(wb.name[,(2:6)])
  plot(wb.name[,1], type = "l",lwd=2, ylim = wb.range, ylab = "flux m2/d", 
       xlab = 'Days (d)', main = paste("water budget of: ",deparse(substitute(wb.name)))) #drn
  lines(wb.name[,2], col = "red",lwd=2)#evap
  lines(wb.name[,3], col = "green",lwd=2)#stor_in
  lines(wb.name[,4], col = "orange",lwd=2)#stor_out
  lines(wb.name[,5], col = "blue",lwd=2)#precip
  lines(wb.name[,6], col = 'brown',lwd=2)#bnd
  grid()
  legend("topright", c("drainage","evap","stor2flow","flow2stor","precip","boundary"),
         col=c("black","red","green","orange","blue","brown"),horiz = FALSE, lwd =3)
}

plot.wbudget.veg = function(wb.name)
{
  nr.wbs = length(wb.name[1,-1]) #number of water balance terms
  nr.colors = rainbow(n = nr.wbs)
  wb.range = range(wb.name[,(2:(nr.wbs+1))])
  plot(as.Date(as.character(wb.name[,1]), "%Y%m%d"), wb.name[,2],col = nr.colors[1], type = "l",lwd=2, ylim = wb.range, ylab = "flux m2/d", 
       xlab = 'Days (d)', main = paste("water budget of: ",deparse(substitute(wb.name)))) #drn
  for (i in 2:nr.wbs)
  {
    lines(as.Date(as.character(wb.name[,1]), "%Y%m%d"), wb.name[,i+1], col = nr.colors[i], lwd = 2.5)
  }
  grid()
  legend("topright", names(wb.name[-1]),
         col=nr.colors ,horiz = FALSE, lwd =3)
  #legend("topright", c("drainage","evap","stor2flow","flow2stor","precip","boundary"),
  #       col=c("black","red","green","orange","blue","brown"),horiz = FALSE, lwd =3)
}



yyyymmdd2date = function(meteo.date)
{
  split = strsplit(meteo.date,)
}
# dag = meteo$yearmonthday[3]
# as.Date(as.character(meteo$yearmonthday[730]),"%Y%m%d")

########plot functions to view the vegetation fraction distribution######
plot.all.fi.distributions = function()
{
#a series of wtable (wspiegel) depths
  wspiegel = rep(0:5000)

plot(wspiegel,Calc.Tai.Tpi(wspiegel,f1.ai,f1.zi), type = "l",lwd = 2,
     main = "Vegetation fraction as function of water table depth", 
     ylab = "vegetation fraction", xlab = "water table depth in mm")
lines(wspiegel,Calc.Tai.Tpi(wspiegel,f2.ai,f2.zi),col = "red",lwd = 2)
lines(wspiegel,Calc.Tai.Tpi(wspiegel,f3.ai,f3.zi),col = "green",lwd = 2)
lines(wspiegel,Calc.Tai.Tpi(wspiegel,f4.ai,f4.zi),col = "blue",lwd = 2)
lines(wspiegel,Calc.Tai.Tpi(wspiegel,f5.ai,f5.zi),col = "orange",lwd = 2)
lines(wspiegel,Calc.Tai.Tpi(wspiegel,f6.ai,f6.zi),col = "cyan",lwd = 2)
grid()
abline(v=f6.zi,col="cyan",lwd=2)
abline(v=f5.zi,col="orange",lwd=2)
abline(v=f4.zi,col="blue",lwd=2)
abline(v=f3.zi,col="green",lwd=2)
abline(v=f2.zi,col="red",lwd=2)
abline(v=f1.zi,lwd=2)
legend("topright",legend = c("f1","f2","f3","f4","f5","f6"),
       col = c("black","red","green","blue","orange","cyan"),horiz = FALSE, lwd =3)
}






########plot functions to view the vegetation fraction distribution######