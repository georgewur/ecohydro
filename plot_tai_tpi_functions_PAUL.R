library(IJPLOT)

f1.ai = 1.39
f1.zi = 250


Calc.Tai.Tpi = function(z,ai,zi)
{
  bi = 4*ai^2/(ai^2-1)*((ai-1)/(ai+1))^(1/ai)
  Tai.Tpi = bi* (z/zi)^(ai-1)/(1+(z/zi)^ai)^2
  return(Tai.Tpi)  
}

# opm: ik heb de indruk dat die "500" te ruim gekozen is
wtable = rep(0:500)*10
# opm: ik heb de y-as vast (ongevoelig voor zoomen en pannen) gezet, maakt makkelijk om in x lokaal te kijken
p = new_ijplot("veg frac", bottommargin=60,leftmargin=60,yfixed=TRUE)
add_line(p, x=wtable, y=Calc.Tai.Tpi(wtable,f1.ai,f1.zi), color = "rgba(0,0,255,0.5)",width=3)


draw(p)

minai = 1.05
maxai = 1.25
# linear:
# ai_vec = seq(minai,maxai,lenght.lout=25)
# logaritmic:
ai_vec = exp(seq(log(minai),log(maxai),length.out=25))
        
add_selectnum(p,name="a index",maxnum=length(ai_vec))
zi = 25
for (i in 1:length(ai_vec))
{
  yi = Calc.Tai.Tpi(wtable,ai_vec[i],f1.zi)
  add_title(p,text=paste("ai=",round(ai_vec[i],4)),selectnum=c("a index",i))
  add_line(p, x=wtable, y=yi, color = "rgba(255,0,0,0.5)",width=3,selectnum=c("a index",i))
}
draw(p)
