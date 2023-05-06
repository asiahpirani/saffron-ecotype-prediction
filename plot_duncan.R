
col_names  = c('No of Flower', 'Flower FW', 'Stigma FW', 'Flower DW', 'Stigma DW', 'Stigma length', 'Yield', 'Picrocrocin', 'Safranal', 'Crocin', 'Total metabolites')
file_names = c('No_of_Flower', 'Flower_FW', 'Stigma_FW', 'Flower_DW', 'Stigma_DW', 'Stigma_length', 'Yield', 'Picrocrocin', 'Safranal', 'Crocin', 'Total_metabolites')
num_ecotypes = 18
num_rep = 3

for (i in 1:length(file_names))
{
  a = read.csv(paste(file_names[i],'.csv',sep=''))
  ecotypes = as.vector(t(matrix(a[,2],num_ecotypes,num_rep)))
  year1 = as.vector(t(a[,3:5]))
  year2 = as.vector(t(a[,6:8]))
  d1 = data.frame(ecotypes,year=year1)
  d2 = data.frame(ecotypes,year=year2)
  
  model<-aov(year~ecotypes,data=d1)
  out <- duncan.test(model,"ecotypes",main=paste(col_names[i],', Year 1', sep=''))
  n = rownames(out$groups)
  pdf(paste('duncan_2/',file_names[i],'_y1.pdf', sep=''))
  par(mar=c(7,4.1,4.1,2.1))
  p = plot(out, xaxt='n', main=paste(col_names[i],', Year 1', sep=''))
  py = par('usr')[3]-0.03*(par('usr')[4]-par('usr')[3])
  text(x=p, y=py,labels = n, cex=0.8, xpd=NA,srt=90, adj = 1)
  dev.off()
  
  model<-aov(year~ecotypes,data=d2)
  out <- duncan.test(model,"ecotypes",main=paste(col_names[i],', Year 2', sep=''))
  n = rownames(out$groups)
  pdf(paste('duncan_2/',file_names[i],'_y2.pdf', sep=''))
  par(mar=c(7,4.1,4.1,2.1))
  p = plot(out, xaxt='n', main=paste(col_names[i],', Year 2', sep=''))
  py = par('usr')[3]-0.03*(par('usr')[4]-par('usr')[3])
  text(x=p, y=py,labels = n, cex=0.8, xpd=NA,srt=90, adj = 1)
  dev.off()
}