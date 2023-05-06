require(MASS)
require(e1071)
require(mda)
require(ggplot2)
require(ggrepel)
require(gridExtra)
require(patchwork)
require(klaR)

run_rda = function(new_data, train_ids)
{
  rda.model = rda(eco ~ . , data=new_data, subset=train_ids)
  rda.pred = predict(rda.model, new_data[-train_ids,])
  res = rda.pred$class == TRUE

  mat = as.data.frame(table(res, new_data$eco[-train_ids]))
  colnames(mat) = c('Predicted', 'Actual', 'Count')
  return(list(mat=mat,res=res))
}

run_lda = function(new_data, train_ids)
{
  lda.model = lda(eco ~ . , data=new_data, subset=train_ids)
  lda.pred = predict(lda.model, new_data[-train_ids,])
  res = lda.pred$class == TRUE

  mat = as.data.frame(table(res, new_data$eco[-train_ids]))
  colnames(mat) = c('Predicted', 'Actual', 'Count')
  return(list(mat=mat,res=res))
}

run_qda = function(new_data, train_ids)
{
  qda.model = qda(eco ~ . , data=new_data, subset=train_ids)
  qda.pred = predict(qda.model, new_data[-train_ids,])
  res = qda.pred$class == TRUE

  mat = as.data.frame(table(res, new_data$eco[-train_ids]))
  colnames(mat) = c('Predicted', 'Actual', 'Count')
  return(list(mat=mat,res=res))
}

run_mda = function(new_data, train_ids)
{
  mda.model = mda(eco ~ . , data=new_data, subset=train_ids)
  mda.pred = predict(mda.model, new_data[-train_ids,])
  res = mda.pred == TRUE

  mat = as.data.frame(table(res, new_data$eco[-train_ids]))
  colnames(mat) = c('Predicted', 'Actual', 'Count')
  return(list(mat=mat,res=res))
}

run_fda = function(new_data, train_ids)
{
  fda.model = fda(eco ~ . , data=new_data, subset=train_ids)
  fda.pred = predict(fda.model, new_data[-train_ids,])
  res = fda.pred == TRUE

  mat = as.data.frame(table(res, new_data$eco[-train_ids]))
  colnames(mat) = c('Predicted', 'Actual', 'Count')
  return(list(mat=mat,res=res))
}

run_nb = function(new_data, train_ids)
{
  nb.model = naiveBayes(eco ~ ., data=new_data, subset=train_ids)
  nb.pred = predict(nb.model, new_data[-train_ids,])
  res = nb.pred == TRUE
  
  mat = as.data.frame(table(res, new_data$eco[-train_ids]))
  colnames(mat) = c('Predicted', 'Actual', 'Count')
  return(list(mat=mat,res=res))
}

make_plot = function(all_results, fname, name, k)
{
  sub_results = all_results[all_results$Accuracy>0,]
  p1=ggplot(sub_results, aes(x=Accuracy, y=Sensitivity)) + geom_point() + xlab("Accuracy") + ylab("Sensitivity") + ggtitle(name)
  p2=ggplot(sub_results, aes(x=Accuracy, y=Specificity)) + geom_point() + xlab("Accuracy") + ylab("Specificity") + ggtitle(' ')
  p3=ggplot(sub_results, aes(x=Sensitivity, y=Specificity)) + geom_point() + xlab("Sensitivity") + ylab("Specificity") + ggtitle(' ')
  # p = grid.arrange(p1, p2, p3, ncol=3)
  p = p1 + p2 + p3
  ggsave(filename=paste('test_best_subset/new_all_figures/2fold/',fname,'/',fname,'.',k,'_all.pdf',sep = ''), width = 8, height = 3, units = 'in')
}

make_heat = function(mat, fname, name, k, i)
{
  ggplot(mat, aes(x=Actual, y=Predicted, fill=Count)) + geom_tile() + xlab("In Khorasan") + 
    ylab("Predicted") + geom_text(aes(label = Count)) + ggtitle(name)
  ggsave(filename=paste('test_best_subset/new_all_figures/2fold/', fname,'/', k,'/',i, '_', fname, '_heat.pdf', sep=''), width = 4, height = 3, units = 'in')
}

make_scatter = function(x, y, enames, fname, name, k, i)
{
  outname = paste('test_best_subset/new_all_figures/2fold/', fname, '/', k,'/', i, '_', fname, '_scatter.pdf', sep='')
  names = enames
  names[x==y] = '' 
  d = 0.1
  data = cbind(x,y)
  n = dim(data)[1]
  r = runif(1,0,0.15)
  a = runif(1,0,2*pi)
  data[1,] = data[1,]+c(r*sin(a),r*cos(a))
  for (i in 2:n)
  {
    while(T)
    {
      t = data[i,]
      r = runif(1,0,0.15)
      a = runif(1,0,2*pi)
      t = t+c(r*sin(a),r*cos(a))
      flag = T
      for (j in 1:(i-1))
      {
        if (norm(t-data[j,],'2')<0.025)
        {
          flag = F
          break
        }
      }
      if (flag)
      {
        break
      }
    }
    data[i,] = t
  }
  # plot(data)
  data = data.frame(data)
  colnames(data) = c('x','y')
  data$col = as.factor(2*x+y)
  ggplot(data=data, aes(x,y,col=col)) + theme_minimal() + 
    theme(panel.grid.minor = element_blank(), legend.position = 'none') +
    geom_point() + scale_color_manual(values=c("black", "red", 'green', 'blue')) +
    geom_label_repel(aes(label = names), box.padding = unit(0.35, "lines"), min.segment.length = unit(0, 'lines')) +
    scale_x_continuous(breaks = round(seq(0, 1, by = 1),1), limits = c(-.2,1.2), labels = c('FALSE','TRUE')) +
    scale_y_continuous(breaks = round(seq(0, 1, by = 1),1), limits = c(-.2,1.2), labels = c('FALSE','TRUE')) +
    xlab('In Khorasan') + ylab('Prediction') + ggtitle(name)
  ggsave(filename=outname, width = 4, height = 4, units = 'in')
}

outs = c('Bahabad', 'Zarand', 'Gorgan', 'Estahbanat', 'Natanz')

a=read.csv('2.csv', stringsAsFactors = F)
enames = paste(a$Ecotype,a$Block,sep='.')

data = a[, 4:14]
data = as.data.frame(scale(as.matrix(data)))
n = dim(data)[2]-1
subsets = unlist(lapply(1:n, function(i){combn(1:n, i, simplify = F)}), recursive = F)

eco = rep(T, dim(data)[[1]])
eco[a$Ecotype %in% outs] = F
names(eco) = 1:54

selected_t = floor(sum(eco==T)/2)
selected_f = floor(sum(eco==F)/2)

for (k in 1:100)
{
  train_ids = c(sample(as.numeric(names(eco[eco==T])),selected_t),sample(as.numeric(names(eco[eco==F])),selected_f))
  
  tot = length(eco[-train_ids])
  tot_pos = sum(eco[-train_ids]==T)
  tot_neg = sum(eco[-train_ids]==F)

  for (j in 1:6)
  {
    run_da = switch (j,
                     run_lda,
                     run_qda,
                     run_fda,
                     run_rda,
                     run_mda,
                     run_nb
    )
    fname = switch (j,
                    'lda',
                    'qda',
                    'fda',
                    'rda',
                    'mda',
                    'nb'
    )
    name = switch (j,
                   'LDA',
                   'QDA',
                   'FDA',
                   'RDA',
                   'MDA',
                   'Naive Bayes'
    )
    
    cat(fname, k, '\n')
    
    all_results = c()
    all_mats = list()
    
    for (i in 1:length(subsets))
    {
      cids = subsets[[i]]
      new_data = as.data.frame(data[,cids])
      new_data = cbind(new_data, eco)
      row_res = c(0,0,0)
      mat = 'no_res'
      tryCatch(
        {
          res = run_da(new_data, train_ids)
          
          pred = res$res
          res  = res$mat
          
          res$Predicted = factor(res$Predicted, levels=c(F,T))
          res$Actual = factor(res$Actual, levels=c(F,T))
          tot_c = sum(res[res$Actual == res$Predicted,'Count'])
          tot_pos_c = sum(res[res$Actual == T & res$Predicted == T,'Count'])
          tot_neg_c = sum(res[res$Actual == F & res$Predicted == F,'Count'])
          row_res = c(tot_c/tot, tot_pos_c/tot_pos, tot_neg_c/tot_neg)
          # all_mats = append(all_mats, list(res))
          mat = list(res)
          
          # make_scatter(as.numeric(eco), as.numeric(pred), enames, fname, name, i)
          # make_heat(as.data.frame(all_mats[[i]]), fname, name, k, i)
          
        },
        error=function(cond){
          # cat(i, length(cids), '\n')
          # message(cond)
          # cat('\n')
          # traceback()
          # cat('\n')
        })
      all_mats = append(all_mats, mat)
      all_results = rbind(all_results, row_res)
    }
    all_results = data.frame(all_results)
    rownames(all_results) = 1:1023
    colnames(all_results) = c('Accuracy', 'Sensitivity', 'Specificity')
    
    all_results$colnames = sapply(1:1023, function(i){paste(colnames(data)[ subsets[[i]]], collapse = ', ')})
    write.csv(all_results, file=paste('test_best_subset/new_all_figures/2fold/', fname,'/',fname, '.', k, '.csv', sep=''))
    
    # rid = as.numeric(rownames(head(all_results[rev(order(all_results$Accuracy, all_results$Specificity)),], n=1)))
    # cat(colnames(data)[ subsets[[rid]]],'\n')
    
    # make_plot(all_results, fname, name, k)
    # 
    # rids = which(all_results[,1]==max(all_results[,1]))
    # for (rid in rids)
    # {
    #   # make_scatter(as.numeric(eco), as.numeric(all_pred[[rid]]), enames, fname, name, j, rid)
    #   make_heat(as.data.frame(all_mats[[rid]]), fname, name, j, rid)
    # }
  }
}