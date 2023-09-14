library(manipulate)

# Ze = 5
# ae = 1.58

spiegel = rep(0:250)

# ea.ep = 1/(1+(spiegel/Ze)^ae)
# 
# plot(spiegel,ea.ep)
# 
# z1 = 25
# a1 = 1.39
# 
# z = z1*((a1-1)/(a1+1))^(1/a1)
# 
# 
# plot(spiegel,spiegel*((a1-1)/(a1+1))^(1/a1))

#vegetation f1
f1.ai = 1.39
f1.zi = 250
f1.di = 0.05
f1 = 0.7
f1.start = f1
#vegetation f2
f2.ai = 1.87
f2.zi = 500
f2.di = 0.01
f2 = 0.02
f2.start = f2
#vegetation f3
f3.ai = 2.59
f3.zi = 690
f3.di = 0.05
f3 = 0.02
f3.start = f3
#vegetation f4
f4.ai = 3.27
f4.zi = 1000
f4.di = 0.05
f4 = 0.015
f4.start = f4
#vegetation f5
f5.ai = 2.59
f5.zi = 920
f5.di = 0.05
f5 = 0.02
f5.start = f5
#vegetation f6
f6.ai = 4.5
f6.zi = 1210
f6.di = 0.05
f6 = 0.01
f6.start = f6
#soil evaporation
f0.aE = 1.58
f0.zE = 5.0
f0 = 0.215
f0.start = f0




Calc.Tai.Tpi = function(Tpi,z,ai,zi)
{
  if (Tpi > 0.0)
  {
    bi = 4*ai^2/(ai^2-1)*((ai-1)/(ai+1))^(1/ai)
    Tai = bi* (z/zi)^(ai-1)/(1+(z/zi)^ai)^2
    return(Tai/Tpi)
  }else(return(0))
}

#Tpi is potential transpiration * current fraction.

wspiegel = rep(0:5000)

Tpi = 1.0
plot(wspiegel,Calc.Tai.Tpi(Tpi,wspiegel,f1.ai,f1.zi), type = "l",lwd = 2)
lines(wspiegel,Calc.Tai.Tpi(Tpi,wspiegel,f2.ai,f2.zi),col = "red",lwd = 2)
lines(wspiegel,Calc.Tai.Tpi(Tpi,wspiegel,f3.ai,f3.zi),col = "green",lwd = 2)
lines(wspiegel,Calc.Tai.Tpi(Tpi,wspiegel,f4.ai,f4.zi),col = "blue",lwd = 2)
lines(wspiegel,Calc.Tai.Tpi(Tpi,wspiegel,f5.ai,f5.zi),col = "orange",lwd = 2)
lines(wspiegel,Calc.Tai.Tpi(Tpi,wspiegel,f6.ai,f6.zi),col = "cyan",lwd = 2)
grid()
abline(v=f6.zi,col="cyan",lwd=2)
abline(v=f5.zi,col="orange",lwd=2)
abline(v=f4.zi,col="blue",lwd=2)
abline(v=f3.zi,col="green",lwd=2)
abline(v=f2.zi,col="red",lwd=2)
abline(v=f1.zi,lwd=2)


# f1.set = Calc.Tai.Tpi(Tpi,wspiegel,f1.ai,f1.zi)
# hist(Calc.Tai.Tpi(Tpi,wspiegel,f1.ai,f1.zi))
# summary(Calc.Tai.Tpi(Tpi,wspiegel,f1.ai,f1.zi))
# f.ai = 1.39
# f.zi = 250

plot.graph = function(f.zi,f.ai)
{
base.ai = 2.25
base.zi = 400
  plot(wspiegel,Calc.Tai.Tpi(1,wspiegel,base.ai,base.zi), type = "l",lwd = 2,
       xlab = "depth grw. table in mm", ylab = "fraction",main = "distribution of water uptake")
  lines(wspiegel,Calc.Tai.Tpi(1,wspiegel,f.ai,f.zi), col = "blue",lwd = 2)
  abline(v=f.zi, col = "darkblue",lty = "dashed", lwd = 2)
  grid()
}
  
manipulate(
  plot.graph(f.zi = f.zi,f.ai=f.ai),
    f.ai = slider(1.25,10.0, initial = 1.5),
  f.zi = slider(1,5000, initial = 400)
)



manipulate1(
  # f.ai = 1.39,
  # f.zi = 250,
  # base.zi = f.zi,
  # base.ai = f.ai,
  plot(wspiegel,Calc.Tai.Tpi(1,wspiegel,base.ai,base.zi), type = "l",lwd = 2),
  grid(),
#  lines(wspiegel,Calc.Tai.Tpi(1,wspiegel,f.ai,f.zi),col = "red",lwd = 2),
  base.ai = slider(1.25,10.0, initial = 1.5),
  base.zi = slider(1,5000, initial = 400)
)

