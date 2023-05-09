require(ggfortify)
require(FactoMineR)
require(factoextra)
require(ggrepel)


col_names = c('N.o.F', 'F.FW', 'S.FW', 'F.DW', 'S.DW', 'S.Len', 'Yield', 'Picrocrocin', 'Safranal', 'Crocin', 'T.Met')
for (y in c('1' , '2'))
{
  a=read.csv(paste('avg_',y,'.csv', sep=''))
  X = a[,3:13]
  rownames(X) = a[,2]
  colnames(X) = col_names
  
  X1 = X[,1:7]
  X2 = X[,8:10]
  X3 = X[,8:11]
  
  all_x = list(X, X1, X2, X3)
  all_names = c('All', 'Morphology features', 'Metabolites', 'Metabolites (+total)')
  all_fnames = c('all', 'morphology', 'metabolites', 'metabolites_total')
  
  for (i in 1:length(all_x))
  {
    X = all_x[[i]]
    res.pca <- PCA(X, graph = FALSE, scale.unit = TRUE)
    p = fviz_pca_biplot(res.pca, repel = TRUE, label.padding=0.5, 
                        label='var', labelsize = 7, col.var = 'blue', 
                        col.ind="contrib", 
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) + 
      geom_label_repel(aes(label=rownames(X), color=contrib), 
                       alpha=0.6, size=7, 
                       box.padding = unit(.35, "lines"), 
                       min.segment.length = unit(0, 'lines')) + 
      theme(panel.border = element_rect(colour = "gray", fill=NA, size=1), 
            plot.title = element_text(size=30, hjust = 0.5),
            axis.text=element_text(size=20),
            axis.title=element_text(size=28)) + 
      ggtitle(paste(all_names[i],' (Y',y,')', sep=''))
    
    ggsave(filename = paste('pca_',all_fnames[i],'_y',y,'_v2.pdf', sep=''))
  }
}

col_names = c('N.o.F', 'F.FW', 'S.FW', 'F.DW', 'S.DW', 'S.Len', 'Yield', 'Picrocrocin', 'Safranal', 'Crocin', 'T.Met', 'Year')

a=read.csv('avg_1.csv', stringsAsFactors = F)
X1 = cbind(a[,3:13],1)
rownames(X1) = paste(a[,2],'1',sep='.')
colnames(X1) = col_names

a=read.csv('avg_2.csv', stringsAsFactors = F)
X2 = cbind(a[,3:13],2)
rownames(X2) = paste(a[,2],'2',sep='.')
colnames(X2) = col_names

X = rbind(X1, X2)

res.pca = prcomp(X[,-12], scale=T)

p = fviz_pca_biplot(res.pca, repel = TRUE, label.padding=0.5, 
                    label='var', labelsize = 7, habillage = X$Year) +
  ggforce::geom_mark_ellipse(aes(fill=Groups, color=Groups)) +
  geom_label_repel(aes(label=rownames(X)), alpha=0.6, size=7, 
                   box.padding = unit(.35, "lines"), 
                   min.segment.length = unit(0, 'lines')) +
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=1), 
        plot.title = element_text(size=30, hjust = 0.5),
        axis.text=element_text(size=20),
        axis.title=element_text(size=28)) + 
  ggtitle('Both Years')

ggsave(filename = 'pca_together.pdf', width = 8, height = 8)

