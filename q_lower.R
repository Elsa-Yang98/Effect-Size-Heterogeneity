####u is a function of t
uu=function(x){
  y=(2*dnorm(-x)-1)*((1.6*((1+x^2)*pnorm(-x)-x*dnorm(x))+0.2*(1+x^2)-1)/(0.2*(1+x^2)*(1-2*pnorm(-x))+2*x*dnorm(x)))+1
  return (y);
}
########


fdrlasso =function (tpp, delta, epsi){
#--------------------------------------------------------------------------
# This function calculates the Lasso trade-off curve given tpp (true
# positive proportion), delta = n/p (shape of the design matrix, or
# subsampling rate), and epsi = k/p (sparsity ratio).
# All tpp, delta, and epsi are between 0 and 1; if the
# pair (delta, epsi) is above the Donoho-Tanner phase transition, tpp
# should be no larger than u^\star = powermax(delta, epsi)
#--------------------------------------------------------------------------
# Copyright @ Weijie Su, Malgorzata Bogdan, and Emmanuel Candes, 2015
#--------------------------------------------------------------------------




if (tpp == 0){
  q = 0;
  return (q)
}

## make stepsize smaller for higher accuracy
stepsize = 0.1;
tmax = max(10, sqrt(delta/epsi/tpp) + 1);
tmin = tmax - stepsize;

while (tmin > 0){
  if (lsandwich(tmin, tpp, delta, epsi) < rsandwich(tmin, tpp)){
    break
  }
  
  tmax = tmin;
  tmin = tmax - stepsize;
}

if (tmin <= 0){
  stepsize = stepsize/100;
  tmax = max(10, sqrt(delta/epsi/tpp) + 1);
  tmin = tmax - stepsize;
  while (tmin > 0){
    if (lsandwich(tmin, tpp, delta, epsi) < rsandwich(tmin, tpp)){
      break
    }
     
    tmax = tmin;
    tmin = tmax - stepsize;
  }
}
  
diff = tmax - tmin;
while (diff > 1e-6){
  tmid = 0.5*tmax + 0.5*tmin;
  if (lsandwich(tmid, tpp, delta, epsi) > rsandwich(tmid, tpp)){
    tmax = tmid;
  }
  else   
  { 
    tmin = tmid;
  }
  diff = tmax - tmin;
}

t = (tmax + tmin)/2;

q = 2*(1-epsi)*pnorm(-t)/(2*(1-epsi)*pnorm(-t) + epsi*tpp);

return (q);
}




####################################
lsandwich=function(t, tpp, delta, epsi){
Lnume = (1-epsi)*(2*(1+t^2)*pnorm(-t) - 2*t*dnorm(t)) + epsi*(1+t^2) - delta;
Ldeno = epsi*((1+t^2)*(1-2*pnorm(-t)) + 2*t*dnorm(t));
L = Lnume/Ldeno;
return (L);
}
####################################
rsandwich=function (t, tpp){
R = (1 - tpp)/(1 - 2*pnorm(-t));
return (R);
}


####################################
## highest power for delta < 1 and epsilon > epsilon_phase
powermax=function(delta, epsilon){
if (delta >= 1){
  power = 1;
  return;
}
epsilon_star = epsilonDT(delta);
if (epsilon <= epsilon_star){
  power = 1;
  return;
}
power = (epsilon - epsilon_star)*(delta - epsilon_star)/epsilon/(1 - epsilon_star) + epsilon_star/epsilon;
return (power)
}

#############f  
epsilonDT=function (delta){
minus_f = function(x) -(1+2/delta*x*dnorm(x) - 2/delta*(1+x^2)*pnorm(-x))/(1+x^2-2*(1+x^2)*pnorm(-x)+2*x*dnorm(x))*delta;
alpha_phase = fminbnd(minus_f, 0, 8)$xmin;
epsi = -minus_f(alpha_phase);
return (epsi)
}



xx=seq(0.01,0.91,0.01)
yy=rep(0,91)
for (i in 1:91){
yy[i]=fdrlasso(0.01*i,0.8,0.6)
}
plot(xx,yy,type="l")