#a
podaci=matrix(c(187,37,35,31),2,2)
n=sum(podaci)
f=c(podaci)
pkapa=podaci/n
pkapa
pkapa=c(pkapa)
png('1.png')
barplot(c(pkapa),names.arg=c("JN","ŠN","JB","ŠB"),ylim=c(0,0.7),col="darkgreen",main="Procijenjena razdioba vektora (X,Y)")
dev.off()

x1=c(3/4,1/4)
y1=c(3/4,1/4)
x1
y1
nez1=x1%*%t(y1)
nezv=c(nez1)

lista=matrix(numeric(0),2,4)
lista[1,]=pkapa
lista[2,]=c(nez1)
png('2.png')
barplot(lista,beside=TRUE,names.arg=c("JN","ŠN","JB","ŠB"),ylim=c(0,0.7),col=c("cornsilk", "lavender"),main="Usporedba procijenjene i nezavisne razdiobe",legend.text=list("Slucajni uzorak","Nezavisna razdioba"))
dev.off()

#b
fnez=nez1*n
fnez
fnez=c(fnez)
h=sum(((f-fnez)^2)/fnez)
h
c=qchisq(0.95,3) 
c
pv=1-pchisq(h,3)
pv

#c
hi=qchisq(0.95,3)
hi
gama=function(lambda){
return(1-pchisq(hi,3,lambda))
}
png('3.png')
plot(gama,type="l",xlim=c(0,100),main="Graf funkcije jakosti testa",xlab="Parametar necentralnosti lambda",ylab="gama(lambda)")
dev.off()
optim(0,gama,method=c("L-BFGS-B"),lower=0,upper=0.05)

#d
h=function(theta){
p2=c(9/16+theta,3/16-theta,3/16-theta,1/16+theta)
n2=p2*290
return(sum(((f-n2)^2)/n2))
}
png('4.png')
plot(Vectorize(h),type="l",xlim=c(-1/16,3/16),main="Graf funkcije h(theta)")
dev.off()
optim(0.05,h,method="L-BFGS-B",lower=0,upper=0.15)
theta1=optim(0.05,h,method="L-BFGS-B",lower=0,upper=0.15)$par
f2=matrix(c(9/16+theta1,3/16-theta1,3/16-theta1,1/16+theta1),2,2)
f2
hop=h(theta1)
hop
pv=1-pchisq(hop,2)
pv

#e
mle=function(theta){
return ((9/16 + theta)^187 * (3/16 - theta)^35 *(3/16 - theta)^37 * (1/16+ theta)^31)
}
plot(mle,type="l",xlim=c(-1/16,3/16),main="Graf funkcije vjerodostojnosti")
D(expression((9/16 + theta)^187 * (3/16 - theta)^35 *(3/16 - theta)^37 * (1/16+ theta)^31),"theta")
der=function(theta){
((187 * (9/16 + theta)^186 * (3/16 - theta)^35 - (9/16 + theta)^187 * 
(35 * (3/16 - theta)^34)) * (3/16 - theta)^37 - (9/16 + theta)^187 * 
(3/16 - theta)^35 * (37 * (3/16 - theta)^36)) * (1/16 + theta)^31 + 
(9/16 + theta)^187 * (3/16 - theta)^35 * (3/16 - theta)^37 * 
(31 * (1/16 + theta)^30)
}
plot(der,type="l",xlim=c(-1/16,3/16),main="Graf derivacije funkcije vjerodostojnosti")
theta2=uniroot(der,c(0,0.1))$root
theta2
f3=matrix(c(9/16+theta2,3/16-theta2,3/16-theta2,1/16+theta2),2,2)
f3
usporedba=matrix(numeric(8),2,4)
usporedba[1,]=c(f2)
usporedba[2,]=c(f3)
usporedba
barplot(usporedba,beside=TRUE,names.arg=c("JN","ŠN","JB","ŠB"),ylim=c(0,0.7),col=c("cornsilk", "lavender"),main="Usporedba razdioba slucajnog vektora (X,Y)",legend.text=list("Minimum hi-kvadrat metoda","Metoda maksimalne vjerodostojnosti"))



