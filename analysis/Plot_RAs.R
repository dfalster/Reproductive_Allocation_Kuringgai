Plot_RAs<-function()
{
#Function plotting the comparison plot of RA for all the species.
InvSummary=read.csv('output/InvestmentSummary.csv')
RA=aggregate(RA~species+age,data=InvSummary,FUN=mean)

RA=t(reshape(RA,timevar="age",idvar="species",direction="wide"))
colnames(RA)=RA[1,]
RA=RA[-1,]
RA=apply(RA,MARGIN=2,FUN=as.numeric)
for(k in 5:2)
{
  for(i in 1:14)
  {
    if((!is.na(RA[k,i]))&is.na(RA[k-1,i]))
        RA[k-1,i]=0
  }
}
RA=RA[,order(colnames(RA))]
Age=matrix(ncol=14,nrow=5,c(1.3,5,7,9,32))
pdf(file='output/docs/RA_Comparison.pdf',width=20,height=10)
matplot(Age,RA,type="b",pch='o',main="Reproductive Allovation for 14 species along time",col=rainbow_hcl(14),lty=1,lwd=3,xlim=c(0,35),bty="L")
legend(33,0.7,legend=colnames(RA),col=rainbow_hcl(14),lty=1,lwd=3)
dev.off()
}
