
f1.ai = 1.39
f1.zi = 250


Calc.Tai.Tpi = function(z,ai,zi)
{
  bi = 4*ai^2/(ai^2-1)*((ai-1)/(ai+1))^(1/ai)
  Tai.Tpi = bi* (z/zi)^(ai-1)/(1+(z/zi)^ai)^2
  return(Tai.Tpi)  
}





wtable = rep(0:500)*10
p1 = new_ijplot("veg frac",xfixed =FALSE, bottommargin=60,leftmargin=60)
add_line(ijp = p1, x=wtable, y=Calc.Tai.Tpi(wtable,f1.ai,f1.zi), color = "blue")
ai = 1
zi = 25
for (i in 1:5)
{
  ai = i*1.05
#  print(i)
# add_line(ijp = p1, x=wtable, y=Calc.Tai.Tpi(wtable,ai,f1.zi), selectnum= c("ai :",i), color = "red")
  add_line(ijp = p1, x=wtable, y=Calc.Tai.Tpi(wtable,ai,f1.zi), color = "red")
}
draw(p1)
