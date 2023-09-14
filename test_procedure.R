#a very simple groundwater model to check whether the vegetation partitioning is
#is working OK
# G. Bier 7/9/23

darcy = function(x,h,gradh)
{
  return(-100*gradh)
}
simple.domain = c(0,1000)
nodes = seq(0,1000, by = 50)
simple.mod = newFLOW1D(simple.domain,darcy,name = "simple")
BC.left = 2.0
set.BC.fixedstate(simple.mod,"l",value = BC.left)
set.BC.fixedstate(simple.mod,"r",value = 1.0)
set.discretisation(simple.mod,nodes,"FV")

drainage = function(x,state)
{
  if (state > surface.fun(x))
  {
    return((surface.fun(x)-state)/100)
  }else
    return(0)
}

add.spatialflux(simple.mod,rate = drainage, name = "drainage")
solve.steps(simple.mod)
plot(simple.mod,fluxplot = T)

surface.fun = approxfun(nodes,rep(BC.left,times = 21))

#vegetation f1
f1.ai = 1.39
f1.zi = 250
f1.di = 0.05
f1 = 0.5
f1.start = f1
#vegetation f2
f2.ai = 1.87
f2.zi = 500
f2.di = 0.05
f2 = 0.4
f2.start = f2
#soil evaporation
f0.aE = 1.58
f0.zE = 5.0
f0 = 0.1
f0.start = f0

f1.fun = approxfun(nodes, rep(f1.start, times = 21))
f2.fun = approxfun(nodes, rep(f2.start, times = 21))
f0.fun = approxfun(nodes, rep(f0.start, times = 21))


##adding precip 
add.spatialflux(simple.mod, 0.00025)

evaptrns = 0.001

calc.Tai.f1.simple = function(x,state)
{
  f1 = f1.fun(x) 
  Tpi.f1 <<- Calc.Tpi(evaptrns,f1) 
  # Tai.Tpi.f1 = Calc.Tai.Tpi(Tpi.f1,max(0,(ahn2.fun(x)-state))*1000,f1.ai,f1.zi)
  Tai.Tpi.f1 = Calc.Tai.Tpi(max(0,(surface.fun(x)-state))*1000,f1.ai,f1.zi)
  Tai.f1 = Tai.Tpi.f1 * evaptrns * f1
  return(-Tai.f1)
}

calc.Tai.f2.simple = function(x,state)
{
  f2 = f2.fun(x) 
  Tpi.f2 <<- Calc.Tpi(evaptrns,f2) 
  # Tai.Tpi.f1 = Calc.Tai.Tpi(Tpi.f1,max(0,(ahn2.fun(x)-state))*1000,f1.ai,f1.zi)
  Tai.Tpi.f2 = Calc.Tai.Tpi(max(0,(surface.fun(x)-state))*1000,f1.ai,f1.zi)
  Tai.f2 = Tai.Tpi.f2 * evaptrns * f2
  return(-Tai.f2)
}

calc.Ea.f0.simple = function(x,state)
{
  #Ep = ETp - sum(Tpi.fi)
  Ep = evaptrns - (Tpi.f1 + Tpi.f2 )
  Ea =  Ep/(1+max(0,(surface.fun(x)-state))*1000/f0.zE)^f0.aE
  return(-Ea)
}


##adding the vegetations
add.spatialflux(simple.mod, calc.Tai.f1.simple,"veggie 1" )
add.spatialflux(simple.mod, calc.Tai.f2.simple,"veggie 2" )
add.spatialflux(simple.mod, calc.Ea.f0.simple,"soil" )
do.initialize(simple.mod,BC.left)
solve.steps(simple.mod, verboselevel = 1)

plot(simple.mod,fluxplot = T)
df.external = dataframe.externalfluxes(simple.mod)
df.states = dataframe.states(simple.mod)
df.wb = dataframe.balance(simple.mod)
waterspiegel = (surface.fun(nodes) -  df.states$state)*1000
waterspiegel[which(waterspiegel < 0)]= 0
waterspiegel.fun = approxfun(nodes,waterspiegel)
#set up values to calculate the new fractions
#fractions vegetation 1
#Tai.f1 = df.external$vegetation1 not sure what purpose this has?
TaiTpi.f1 = Calc.Tai.Tpi(waterspiegel.fun(nodes),f1.ai,f1.zi)
df1.dt = Calc.dfi.dt(TaiTpi.f1,f1,f0,f1.di)
f1 = f1.fun(nodes) + df1.dt
f1.fun = approxfun(nodes,f1)
#f1.frac = rbind(f1.frac,f1)
#f1.set = c(f1.set,f1)

#fractions vegetation 2
#Tai.f2 = df.external$vegetation2
TaiTpi.f2 = Calc.Tai.Tpi(waterspiegel.fun(nodes),f2.ai,f2.zi)
df2.dt = Calc.dfi.dt(TaiTpi.f2,f2,f0,f2.di)
f2 = f2.fun(nodes) + df2.dt
f2.fun = approxfun(nodes,f2)
#f2.frac = rbind(f2.frac,f2)
#f2.set = c(f2.set,f2)

#fractions bare soil
f0 = f0 - df1.dt - df2.dt 
f0.fun = approxfun(nodes,f0)
#f0.frac = rbind(f0.frac,f0)
#f0.set = c(f0.set,f0)


plot(nodes,waterspiegel.fun(nodes), type = "b")
grid()
plot.taipai()

plot.external(simple.mod)
plot.fractions()
