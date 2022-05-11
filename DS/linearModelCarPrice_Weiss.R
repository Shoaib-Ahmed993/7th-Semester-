price<-c(85, 103, 70, 82 ,89 ,98 ,66 ,95 ,169  ,70, 48)
age<-c(5 ,4 ,6  ,5 ,5 ,5 ,6   ,6    ,2  ,7 ,7)
lnMod<-lm(price~ age)
lnMod
plot(fitted(lnMod),resid(lnMod))
qqnorm(resid(lnMod))
