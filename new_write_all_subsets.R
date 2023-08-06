

require(MASS)
require(e1071)
require(mda)
require(ggplot2)
require(ggrepel)
require(gridExtra)
require(patchwork)
require(klaR)
require(tree)

make_res = function(res, e, rnames)
{
  res = factor(res)
  levels(res) = c(F,T)
  e = factor(e)
  levels(e) = c(F,T)
  
  mat = as.data.frame(table(res, e))
  colnames(mat) = c('Predicted', 'Actual', 'Count')
  
  res = cbind(res, e)
  rownames(res) = rnames
  
  return(list(mat=mat,res=res))
}

make_mat = function(res)
{
  res$Predicted = factor(res$Predicted, levels=c(F,T))
  res$Actual = factor(res$Actual, levels=c(F,T))
  tot_c = sum(res[res$Actual == res$Predicted,'Count'])
  tot_pos_c = sum(res[res$Actual == T & res$Predicted == T,'Count'])
  tot_neg_c = sum(res[res$Actual == F & res$Predicted == F,'Count'])
  tot = sum(res$Count)
  tot_pos = sum(res[res$Actual==T, ]$Count)
  tot_neg = sum(res[res$Actual==F, ]$Count)
  row_res = c(tot_c/tot, tot_pos_c/tot_pos, tot_neg_c/tot_neg)
  
  return(row_res)
}

fix_all_res = function(all_results, data, subsets)
{
  all_results = data.frame(all_results)
  rownames(all_results) = 1:1023
  colnames(all_results) = c('Accuracy', 'Sensitivity', 'Specificity')
  all_results$colnames = sapply(1:1023, function(i){paste(colnames(data)[ subsets[[i]]], collapse = ', ')})
  return(all_results)
}

run_tree = function(new_data, train_ids)
{
  rnames = rownames(new_data)
  rnames_test  = rnames[-train_ids]
  rnames_train = rnames[train_ids]
  
  tree.model1 = tree(eco ~ . , data=new_data, subset=train_ids)
  tree.pred = predict(tree.model1, new_data[-train_ids,])
  res1_test = tree.pred[,1]<tree.pred[,2]
  tree.pred = predict(tree.model1, new_data[train_ids,])
  res1_train = tree.pred[,1]<tree.pred[,2]

  res_test  = res1_test
  e_test    = new_data$eco[-train_ids]
  res_train = res1_train
  e_train   = new_data$eco[train_ids]

  out = make_res(res_test, e_test, rnames_test)
  mat_test = out$mat
  res_test = out$res

  out = make_res(res_train, e_train, rnames_train)
  mat_train = out$mat
  res_train = out$res
  
  return(list(mat_train=mat_train, mat_test=mat_test,
              res_train=res_train, res_test=res_test))
}

run_svm_lin = function(new_data, train_ids)
{
  rnames = rownames(new_data)
  rnames_test  = rnames[-train_ids]
  rnames_train = rnames[train_ids]
  
  svm.model = svm(eco ~ . , data=new_data, subset=train_ids, kernel='linear')
  svm.pred = predict(svm.model, new_data[-train_ids,])
  res1_test = svm.pred
  svm.pred = predict(svm.model, new_data[train_ids,])
  res1_train = svm.pred
  
  res_test  = res1_test
  e_test    = new_data$eco[-train_ids]
  res_train = res1_train
  e_train   = new_data$eco[train_ids]
  
  out = make_res(res_test, e_test, rnames_test)
  mat_test = out$mat
  res_test = out$res
  
  out = make_res(res_train, e_train, rnames_train)
  mat_train = out$mat
  res_train = out$res
  
  return(list(mat_train=mat_train, mat_test=mat_test,
              res_train=res_train, res_test=res_test))
}

run_svm_rad = function(new_data, train_ids)
{
  rnames = rownames(new_data)
  rnames_test  = rnames[-train_ids]
  rnames_train = rnames[train_ids]
  
  svm.model = svm(eco ~ . , data=new_data, subset=train_ids, kernel='radial')
  svm.pred = predict(svm.model, new_data[-train_ids,])
  res1_test = svm.pred
  svm.pred = predict(svm.model, new_data[train_ids,])
  res1_train = svm.pred
  
  res_test  = res1_test
  e_test    = new_data$eco[-train_ids]
  res_train = res1_train
  e_train   = new_data$eco[train_ids]
  
  out = make_res(res_test, e_test, rnames_test)
  mat_test = out$mat
  res_test = out$res
  
  out = make_res(res_train, e_train, rnames_train)
  mat_train = out$mat
  res_train = out$res
  
  return(list(mat_train=mat_train, mat_test=mat_test,
              res_train=res_train, res_test=res_test))
}

run_rda = function(new_data, train_ids)
{
  rnames = rownames(new_data)
  rnames_test  = rnames[-train_ids]
  rnames_train = rnames[train_ids]
  
  rda.model = rda(eco ~ . , data=new_data, subset=train_ids)
  rda.pred = predict(rda.model, new_data[-train_ids,])
  res1_test = rda.pred$class == TRUE
  rda.pred = predict(rda.model, new_data[train_ids,])
  res1_train = rda.pred$class == TRUE
  
  res_test  = res1_test
  e_test    = new_data$eco[-train_ids]
  res_train = res1_train
  e_train   = new_data$eco[train_ids]
  
  out = make_res(res_test, e_test, rnames_test)
  mat_test = out$mat
  res_test = out$res
  
  out = make_res(res_train, e_train, rnames_train)
  mat_train = out$mat
  res_train = out$res
  
  return(list(mat_train=mat_train, mat_test=mat_test,
              res_train=res_train, res_test=res_test))
}

run_lda = function(new_data, train_ids)
{
  rnames = rownames(new_data)
  rnames_test  = rnames[-train_ids]
  rnames_train = rnames[train_ids]
  
  lda.model = lda(eco ~ . , data=new_data, subset=train_ids)
  lda.pred = predict(lda.model, new_data[-train_ids,])
  res1_test = lda.pred$class == TRUE
  lda.pred = predict(lda.model, new_data[train_ids,])
  res1_train = lda.pred$class == TRUE
  
  res_test  = res1_test
  e_test    = new_data$eco[-train_ids]
  res_train = res1_train
  e_train   = new_data$eco[train_ids]
  
  out = make_res(res_test, e_test, rnames_test)
  mat_test = out$mat
  res_test = out$res
  
  out = make_res(res_train, e_train, rnames_train)
  mat_train = out$mat
  res_train = out$res
  
  return(list(mat_train=mat_train, mat_test=mat_test,
              res_train=res_train, res_test=res_test))
}

run_qda = function(new_data, train_ids)
{
  rnames = rownames(new_data)
  rnames_test  = rnames[-train_ids]
  rnames_train = rnames[train_ids]
  
  qda.model = qda(eco ~ . , data=new_data, subset=train_ids)
  qda.pred = predict(qda.model, new_data[-train_ids,])
  res1_test = qda.pred$class == TRUE
  qda.pred = predict(qda.model, new_data[train_ids,])
  res1_train = qda.pred$class == TRUE
  
  res_test  = res1_test
  e_test    = new_data$eco[-train_ids]
  res_train = res1_train
  e_train   = new_data$eco[train_ids]
  
  out = make_res(res_test, e_test, rnames_test)
  mat_test = out$mat
  res_test = out$res
  
  out = make_res(res_train, e_train, rnames_train)
  mat_train = out$mat
  res_train = out$res
  
  return(list(mat_train=mat_train, mat_test=mat_test,
              res_train=res_train, res_test=res_test))
}

run_mda = function(new_data, train_ids)
{
  rnames = rownames(new_data)
  rnames_test  = rnames[-train_ids]
  rnames_train = rnames[train_ids]
  
  mda.model = mda(eco ~ . , data=new_data, subset=train_ids)
  mda.pred = predict(mda.model, new_data[-train_ids,])
  res1_test = mda.pred == TRUE
  mda.pred = predict(mda.model, new_data[train_ids,])
  res1_train = mda.pred == TRUE
  
  res_test  = res1_test
  e_test    = new_data$eco[-train_ids]
  res_train = res1_train
  e_train   = new_data$eco[train_ids]
  
  out = make_res(res_test, e_test, rnames_test)
  mat_test = out$mat
  res_test = out$res
  
  out = make_res(res_train, e_train, rnames_train)
  mat_train = out$mat
  res_train = out$res
  
  return(list(mat_train=mat_train, mat_test=mat_test,
              res_train=res_train, res_test=res_test))
}

run_fda = function(new_data, train_ids)
{
  rnames = rownames(new_data)
  rnames_test  = rnames[-train_ids]
  rnames_train = rnames[train_ids]
  
  fda.model = fda(eco ~ . , data=new_data, subset=train_ids)
  fda.pred = predict(fda.model, new_data[-train_ids,])
  res1_test = fda.pred == TRUE
  fda.pred = predict(fda.model, new_data[train_ids,])
  res1_train = fda.pred == TRUE
  
  res_test  = res1_test
  e_test    = new_data$eco[-train_ids]
  res_train = res1_train
  e_train   = new_data$eco[train_ids]
  
  out = make_res(res_test, e_test, rnames_test)
  mat_test = out$mat
  res_test = out$res
  
  out = make_res(res_train, e_train, rnames_train)
  mat_train = out$mat
  res_train = out$res
  
  return(list(mat_train=mat_train, mat_test=mat_test,
              res_train=res_train, res_test=res_test))
}

run_nb = function(new_data, train_ids)
{
  rnames = rownames(new_data)
  rnames_test  = rnames[-train_ids]
  rnames_train = rnames[train_ids]
  
  nb.model = naiveBayes(eco ~ ., data=new_data, subset=train_ids)
  nb.pred = predict(nb.model, new_data[-train_ids,])
  res1_test = nb.pred == TRUE
  nb.pred = predict(nb.model, new_data[train_ids,])
  res1_train = nb.pred == TRUE
  
  res_test  = res1_test
  e_test    = new_data$eco[-train_ids]
  res_train = res1_train
  e_train   = new_data$eco[train_ids]
  
  out = make_res(res_test, e_test, rnames_test)
  mat_test = out$mat
  res_test = out$res
  
  out = make_res(res_train, e_train, rnames_train)
  mat_train = out$mat
  res_train = out$res
  
  return(list(mat_train=mat_train, mat_test=mat_test,
              res_train=res_train, res_test=res_test))
}

outs = c('Bahabad', 'Zarand', 'Gorgan', 'Estahbanat', 'Natanz')

a=read.csv('2.csv', stringsAsFactors = F)
enames = paste(a$Ecotype,a$Block,sep='.')

data = a[, 4:14]
# data = as.data.frame(scale(as.matrix(data)))
data = as.data.frame(as.matrix(data))
rownames(data) = enames
n = dim(data)[2]-1
subsets = unlist(lapply(1:n, function(i){combn(1:n, i, simplify = F)}), recursive = F)

eco = rep(T, dim(data)[[1]])
eco[a$Ecotype %in% outs] = F
names(eco) = 1:54

selected_t = floor(sum(eco==T)/2)
selected_f = floor(sum(eco==F)/2)

for (k in 1:100) # 100
{
  
  train_ids = read.csv(file=paste('test_best_subset/new_with_tree/2fold/train_ids.', k, '.csv', sep=''))
  train_ids = train_ids$x

  tot = length(eco)
  tot_pos = sum(eco==T)
  tot_neg = sum(eco==F)
  
  for (j in 1:9)
  {
    run_da = switch (j,
                     run_lda,
                     run_qda,
                     run_fda,
                     run_rda,
                     run_mda,
                     run_nb,
                     run_tree,
                     run_svm_lin,
                     run_svm_rad
    )
    fname = switch (j,
                    'lda',
                    'qda',
                    'fda',
                    'rda',
                    'mda',
                    'nb',
                    'tree',
                    'svm_lin',
                    'svm_rad'
    )
    name = switch (j,
                   'LDA',
                   'QDA',
                   'FDA',
                   'RDA',
                   'MDA',
                   'Naive Bayes',
                   'Decision Tree',
                   'SVM (Linear)',
                   'SVM (Radial)'
    )
    
    cat(fname, k, '\n')
    
    all_results_train = c()
    all_results_test  = c()
    # all_mats = list()
    
    for (i in 1:length(subsets))
    {
      cids = subsets[[i]]
      if (fname == 'tree')
      {
        new_data = as.data.frame(data[,cids])
      }
      else
      {
        new_data = as.data.frame(scale(data[,cids]))
      }
      new_data = cbind(new_data, eco)
      if (fname == 'tree' || fname == 'svm_lin' || fname == 'svm_rad')
      {
        new_data$eco = factor(new_data$eco)
      }
      row_res_train = c(0,0,0)
      row_res_test  = c(0,0,0)
      mat = 'no_res'
      tryCatch(
        {
          res = run_da(new_data, train_ids)
          
          mat  = res$mat_train
          row_res_train = make_mat(mat)
          mat  = res$mat_test
          row_res_test  = make_mat(mat)
          
        },
        error=function(cond){
          # cat(i, length(cids), '\n')
          # message(cond)
          # cat('\n')
          # traceback()
          # cat('\n')
        })
      all_results_train = rbind(all_results_train, row_res_train)
      all_results_test  = rbind(all_results_test,  row_res_test)
    }
    all_results_train = fix_all_res(all_results_train, data, subsets)
    all_results_test  = fix_all_res(all_results_test, data, subsets)
    
    write.csv(all_results_train, file=paste('2fold/', fname,'/',fname, '.', k, '_train.csv', sep=''))
    write.csv(all_results_test,  file=paste('2fold/', fname,'/',fname, '.', k, '_test.csv', sep=''))
    
  }
}
