
require("ggplot2")
require("ggrepel")

plot_volcano = function()
{
  fnames = c('No_of_Flower', 'Flower_FW', 'Stigma_FW', 'Flower_DW', 'Stigma_DW', 'Stigma_length', 'Yield', 'Picrocrocin', 'Safranal', 'Crocin', 'Total_metabolites')
  names  = c('No of Flower', 'Flower FW', 'Stigma FW', 'Flower DW', 'Stigma DW', 'Stigma length', 'Yield', 'Picrocrocin', 'Safranal', 'Crocin', 'Total metabolites')
  for (i in 1:length(names))
  {
    file_name = paste(fnames[i],'_pval_fc.csv', sep='')
    a = read.csv(file=file_name, stringsAsFactors = FALSE)
    sig_index = a$X.log10pval > 1.3
    a$sig = 'No'
    a$sig[sig_index] = 'Yes'
    a$col = 'black'
    a$col[sig_index] = 'red'
    a$label = NA
    a$label[sig_index] =  a$name[sig_index]
    p = ggplot(data=a, aes(x=log2fc, y=X.log10pval, col=sig, label=name)) +
      geom_point() + 
      theme_minimal() + 
      theme(panel.border = element_rect(colour = "gray", fill=NA, size=1), legend.position = 'none') + 
      geom_hline(yintercept=-log10(0.05), col="green", linetype=2)  +
      geom_vline(xintercept=0, col="green", linetype=2)  + 
      xlab(expression(paste(log[2], ' fold change'))) + 
      ylab(expression(paste(-log[10], italic(' p'), '-val'))) + 
      ggtitle(names[i]) + theme(plot.title = element_text(hjust = 0.5)) + 
      # geom_label_repel(aes(fill = 'f8766dff'), color = 'white', segment.color='red', box.padding = unit(0.35, "lines"), min.segment.length = unit(0, 'lines')) +
      geom_label_repel(aes(label = name, fill = sig, segment.color=col), color = 'white', box.padding = unit(0.35, "lines"), min.segment.length = unit(0, 'lines')) + 
      scale_color_manual(values=c("red","black")) + 
      scale_fill_manual(values=c("#f8766dff","gray"))
    # theme_update(panel.border = element_rect(colour = "gray", fill=NA, size=1))
    ggsave(filename = paste(fnames[i],'.pdf', sep=''))
  }
}

plot_volcano()
