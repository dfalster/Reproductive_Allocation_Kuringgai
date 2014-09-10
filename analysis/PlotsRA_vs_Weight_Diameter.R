PlotRAs_vs_Diameter<-function()
{
  InvSummary=read.csv('output/InvestmentSummary.csv')
  pdf(file='output/docs/RA_Diam_Comparison.pdf',width=20,height=10)
  plot(InvSummary$FinalBasalDiamAv,InvSummary$RA,type="p",col=rainbow_hcl(14)[as.numeric(InvSummary$species)],xlab="Basal Diameter, year 2013",ylab="RA",bty="L")
  legend('topright',legend=as.character(sort(unique(InvSummary$species))),col=rainbow_hcl(14),pch=1)
  dev.off()
}

PlotRAs_vs_Weight<-function()
{
  InvSummary=read.csv('output/InvestmentSummary.csv')
  pdf(file='output/docs/RA_Weight_Comparison.pdf',width=20,height=10)
  plot(InvSummary$FinalWeight,InvSummary$RA,type="p",col=rainbow_hcl(14)[as.numeric(InvSummary$species)],xlab="Tree Weight, year 2013",ylab="RA",bty="L")
  legend('topright',legend=as.character(sort(unique(InvSummary$species))),col=rainbow_hcl(14),pch=1)
  dev.off()
}



PlotIndividualRAs_vs_Weight_Diam<-function()
{
  InvSummary=read.csv('output/InvestmentSummary.csv')
  species_v=as.character(sort(unique(InvSummary$species)))
  for(i in 1:14)
  {
    species=species_v[i]
    Data=InvSummary[InvSummary$species==species,]
    pdf(file=paste0('output/docs/RA_',species,'_DW.pdf'),width=14,height=7)
    par(mfrow=c(1,2),cex.main=2,cex.lab=1.5)
    plot(Data$FinalBasalDiamAv,Data$RA,type="p",col=rainbow_hcl(14)[i],xlab="Diameter, year 2013",ylab="RA",bty="L",main="Diameter",cex=1.5)
    plot(Data$FinalWeight,Data$RA,type="p",col=rainbow_hcl(14)[i],xlab="Weight, year 2013",ylab="RA",bty="L",main="Weight",cex=1.5)
    dev.off()
  }
}