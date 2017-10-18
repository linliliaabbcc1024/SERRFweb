options(warn=-1)

path = paste0(workingdirectory,"\\",filename)
if(!"pacman" %in% rownames(installed.packages())){
  install.packages("pacman")
}
cat("Checking required packages (auto-installing if missing).\n")
pacman::p_load("randomForest", "affy", "e1071", "data.table", "parallel", "ReporteRs", "xlsx")
setwd(workingdirectory)
# load sources
source("normalizations.R")
source("utils.R")
source("evaluationMethods.R")
# read data.
cat("Reading Data.\n")
# metaData = read.csv("P20 dataset1 2016_11-30.csv")
# p = fread("p-pos.csv")
# p <- merge(p,metaData,by.x="Subject ID", by.y = "GBID", all.x = T,sort = F)
# f = fread("f-pos.csv")
# e = fread("e-pos.csv")
# e = as.matrix(e)
# p$`Acq. Date-Time`  = gsub("/13 ", "/2013 ", p$`Acq. Date-Time`)
# p$`Acq. Date-Time` = as.numeric(strptime(p$`Acq. Date-Time`, "%m/%d/%Y %H:%M"))
#
#
# path = "C:\\Users\\Sili Fan\\Desktop\\WORK\\WCMC\\projects\\mayo_depression_SSRIs_2013_normalization\\mayo_depression_SSRIs_2013_with_time_stamp.xlsx"

# for xlsx input
data = readData(path)
p = data$p
p$`Acq. Date-Time` = p$time
p$`Stat Level 1` = p$type
p$`Stat Level 1`[p$`Stat Level 1`=='validate'] = "NIST"
f = data$f
e = as.matrix(data$e)

if(sum(is.na(e)) > 0){
  cat(paste0("NOTE: ",sum(is.na(e)), " missing values detected in the data. They will be replaced by the half-minimum for each compound."))
  missing_compounds = which(is.na(e), arr.ind = T)[,1]
  for(i in missing_compounds){
    e[i, is.na(e[i,])] = 1/2 * min(e[i,!is.na(e[i,])])
  }
}


e = data.matrix(e)


# ggplot2 theme
library(ggplot2)
theme.scatter = theme(
  plot.title = element_text(size = rel(2), hjust = 0.5,face = 'bold',family = "Arial"),#title size.
  # axis.title = element_text(size = rel(2)),
  axis.text	 = element_text(colour = 'black'),
  panel.background = element_blank(),
  plot.background = element_blank(),
  legend.key = element_rect(fill = "white",colour = "white"),
  legend.title = element_text(face = 'bold'),
  text=element_text(family="Arial")
)



# batch = rep(diff(sort(c(0,order(diff(p$time), decreasing = T)[c(1:3,5:6)], nrow(p)))), diff(sort(c(0,order(diff(p$time), decreasing = T)[c(1:3,5:6)], nrow(p)))))
# batch = revalue(as.character(batch), c("157"="Batch1", "140" = "Batch2", "167" = "Batch3", "179" = "Batch4", "204" = "Batch5", "178" = "Batch6"))

# png("batch defination.png", width = 800, height = 800)
# plot(p$time, col = factor(batch))
# dev.off()
# define batches according to time interval.
# cat("Visualize the batch vs injection order.\n")
batch = p$batch
batch = matrix(rep(batch,nrow(f)), nrow = nrow(f), byrow = T, ncol = nrow(p))
# dta = data.frame(injection.order = 1:nrow(p), Date = p$`Acq. Date-Time`, batch = batch[1,])
# ggplot(dta, aes(x=injection.order, y=Date, color=batch)) +
#   geom_point()+
#   labs(title="Batch Defination \n according to time intervals",
#        x="Injection Order", y = "Date") + theme.scatter


# check if data has NIST (validate)
NISTavailable = sum(p$`Stat Level 1`=="NIST") > 0

cat(paste0("validate samples are not detected."))

# set parallel computing if necessary.
cat("Initializing the parallel procedure, using all cores.\n")
cl = makeCluster(detectCores())

# results will be saved in the result_norm.
result_norm = list()

cat("set up Monte Carlo cross-validation index. 5-fold 8/2 split.\n");
QC.index.train = QC.index.test = list()
n_CV = 5
seed = 8
set.seed(seed)
for(j in 1:n_CV){
  QC.index = which(p$`Stat Level 1` == "QC")
  QC.index.train.temp = sample(QC.index,round(length(QC.index)*.8))
  QC.index.test.temp = QC.index[!QC.index%in%QC.index.train.temp]
  QC.index.train. = rep(F,ncol(e))
  QC.index.test. = rep(F,ncol(e))
  QC.index.train.[QC.index.train.temp] = T
  QC.index.test.[QC.index.test.temp] = T
  QC.index.train[[j]] = QC.index.train.
  QC.index.test[[j]] = QC.index.test.
}

cat("\n<========== Normalizations Started! ==========>\n");
# no normalization.
cat("\n<========== No Normalization Started! ==========>\n")
result_norm[['none']] = (none_norm(e=e,f=f,p=p))
none.QC.CV = RSD(result_norm[['none']]$e[,p$`Stat Level 1`=='QC'],f,p[p$`Stat Level 1`=='QC',],cl=cl)
none.validate = RSD(result_norm[['none']]$e[,p$`Stat Level 1`=='NIST'],f,p,cl=cl)
cat(paste0("No normalization QC CV RSD is ", signif(median(none.QC.CV, na.rm = T), 4)*100,"%. ", signif(sum(none.QC.CV<0.10, na.rm = T)/nrow(f),4)*100,"% of QC CV RSD < 10%.\n"))
if(NISTavailable){
  cat( "No normalization validate QC RSD is ", signif(median(none.validate, na.rm = T), 4)*100,"%. ", paste0(signif(sum(none.validate<0.10, na.rm = T)/nrow(f),4)*100,"% of validate QC RSD < 10%.\n"))
}
dir.create("normalized-data-sets")
dta = data$original
dta[5:nrow(dta),3:ncol(dta)] = result_norm[['none']]$e
write.csv(dta, file="normalized-data-sets\\normalization-result-none-normalization.csv",row.names=FALSE)

# mTIC normalization.
cat("\n<========== mTIC Normalization Started! ==========>\n")
result_norm[['mTIC']] = (mTIC_norm(e=e,f=f,p=p))
mTIC.QC.CV = RSD(result_norm[['mTIC']]$e[,p$`Stat Level 1`=='QC'],f,p[p$`Stat Level 1`=='QC',],cl=cl)
mTIC.validate = RSD(result_norm[['mTIC']]$e[,p$`Stat Level 1`=='NIST'],f,p,cl=cl)
cat(paste0("mTIC normalization QC CV RSD is ", signif(median(mTIC.QC.CV, na.rm = T), 4)*100,"%. ", signif(sum(mTIC.QC.CV<0.10, na.rm = T)/nrow(f),4)*100,"% of QC CV RSD < 10%.\n"))
if(NISTavailable){
  cat("mTIC normalization validate QC RSD is ", signif(median(mTIC.validate, na.rm = T), 4)*100,"%. ", paste0(signif(sum(mTIC.validate<0.10, na.rm = T)/nrow(f),4)*100,"% of validate QC RSD < 10%.\n"))
}
dta = data$original
dta[5:nrow(dta),3:ncol(dta)] = result_norm[['mTIC']]$e
write.csv(dta, file="normalized-data-sets\\normalization-result-mTIC-normalization.csv",row.names=FALSE)

# batchwise-loess normalization.
cat("\n<========== Batch-wise LOESS Normalization Started! ==========>\n")
result_norm[['loess']] = loess_norm(e = e, f=f, p=p,batch,QC.index,"Acq. Date-Time",span.para = 0.75)
loess.validate = RSD(result_norm[['loess']]$e[,p$`Stat Level 1`=="NIST"],f,p[p$`Stat Level 1`=="NIST",],cl=cl)
result_norm_loess_CV = list()
loess.QC.CV.  = list()
for(j in 1:n_CV){
  time = p$`Acq. Date-Time`
  qc. = QC.index.train[[j]]
  norms = parSapply(cl, X = 1:nrow(f), function(i,eData,qc,batch,time,remove_outlier,span_para,get_loess_para,loess.span.limit){
    models = by(data.frame(v=eData[i,qc],t=time[qc]),
                batch[i,qc],function(x){
                  # x = data.frame(v=e[i,qc],t=time[qc])[batch[i,qc]=="B",]
                  if(length(remove_outlier(x$v)[[2]])>0){# if outlier exists.
                    span = ifelse(span_para=='auto',
                                  get_loess_para(x=x$t[-remove_outlier(x$v)[[2]]],y=remove_outlier(x$v)[[1]],
                                                 loess.span.limit = loess.span.limit),span_para) # find a proper span.
                  }else{
                    span = ifelse(span_para=='auto',
                                  get_loess_para(x=x$t,y=x$v,
                                                 loess.span.limit = loess.span.limit),span_para) # find a proper span.

                  }
                  if(length(remove_outlier(x$v)[[2]])>0){
                    tryCatch(loess(v~t,data=x[-remove_outlier(x$v)[[2]],],span=span),error = function(e){
                      NA
                    })
                  }else{
                    tryCatch(loess(v~t,data=x,span=span), error = function(e){
                      NA
                    })
                  }
                })

    # predict using the models.
    norm = mapply(function(u,v){
      o = tryCatch({
        predict(u,newdata = v)
      },
      error = function(e){
        print(e)
        rep(0,length(v))
      })
    },models,by(time,batch[i,],function(x){x}))


    norm = unlist(norm)
    # replace NA with the closest value.
    if(length(which(is.na(norm)))>0){
      for(j in which(is.na(norm))){
        NA_batch = batch[1,][j]
        time_notNA = time[batch[1,]%in%NA_batch][-which(is.na(norm[batch[1,]%in%NA_batch]))]
        closest_time = time_notNA[which.min(abs(time_notNA-time[j]))]
        norm[j] = norm[batch[1,]%in%NA_batch][which(time[batch[1,]%in%NA_batch]==closest_time)[1]]
      }
    }
    return(norm)
  },e,qc.,batch,time,remove_outlier,0.75,get_loess_para,0.1)
  norms = t(norms)
  e_norm = matrix(NA,nrow=nrow(e),ncol=ncol(e))
  # if(divide){
  for(k in 1:nrow(e)){
    e_norm[k,] = e[k,]/(norms[k,]/median(e[k,],na.rm = T))
  }
  result = e_norm
  loess.QC.CV.[[j]] = RSD(result[,QC.index.test[[j]]],f,p[QC.index.test[[j]],],cl=cl)
}
loess.QC.CV = apply(do.call("cbind",loess.QC.CV.),1,mean, na.rm=T)
cat(paste0("loess normalization QC CV RSD is ", signif(median(loess.QC.CV, na.rm = T), 4)*100,"%. ", signif(sum(loess.QC.CV<0.10, na.rm = T)/nrow(f),4)*100,"% of QC CV RSD < 10%.\n"))
if(NISTavailable){
  cat("loess normalization validate QC RSD is ", signif(median(loess.validate, na.rm = T), 4)*100,"%. ", paste0(signif(sum(loess.validate<0.10, na.rm = T)/nrow(f),4)*100,"% of validate QC RSD < 10%.\n"))
}
dta = data$original
dta[5:nrow(dta),3:ncol(dta)] = result_norm[['loess']]$e
write.csv(dta, file="normalized-data-sets\\normalization-result-loess-normalization.csv",row.names=FALSE)

# SERRF
cat("\n<========== SERRF Normalization Started! ==========>\n")
cat("This may take some time. \n")
qc = rep(F, nrow(p))
qc[QC.index] = T
e. = e
result_norm[['SERRF']] = SERRF_norm(e., f, p, batch, QC.index, time = "Acq. Date-Time")
SERRF.validate = RSD(result_norm[['SERRF']]$e[,p$`Stat Level 1`=="NIST"],f,p[p$`Stat Level 1`=="NIST",],cl=cl)
for(i in 1:nrow(e)){ # MAKE SURE THE QC AND SAMPLES ARE AT THE SAME LEVEL. This is critical for SERRF algorithm (and other tree-based machine learning algorithm) because when building each tree, the split on each leaf considers the level of the values. If the values are not consistant, then the RF models will be wrong and the RF will bias the intensity level after normalization (although the relative position won't change.)
  e.[i,qc] = unlist(by(data.frame(e.[i,],qc),batch[1,],function(x){# x = data.frame(e.[i,],qc)[batch[1,]=='A',]
    x[x[,2],1] - (median(x[x[,2],1]) - median(x[!x[,2],1]))
  }))
}
result_norm_SERRF_CV = list()
SERRF.QC.CV.  = list()
for(i in 1:n_CV){
  time = p$`Acq. Date-Time`
  qc. = QC.index.train[[j]]
  e_SERRF_pred = parSapply(cl, X = 1:nrow(f), function(j,eData,batch,randomForest, qc., time){
    data = data.frame(y = eData[j,], t(eData[-j,]), batch = batch[1,], time = time)
    colnames(data) = c("y", paste0("X",1:nrow(eData))[-j], "batch", "time")
    model = randomForest(y~., data = data,subset = qc., importance = F, ntree = 500)
    newdata = data.frame(t(eData[-j,]), batch = batch[1,], time = time)
    colnames(newdata) =   c(paste0("X",1:nrow(eData))[-j], "batch", "time")
    new = (eData[j,]/predict(model,newdata = newdata)) * median(eData[j,])
    return(new)
  }, e.,batch,randomForest, qc., p$`Acq. Date-Time`)
  e_SERRF_pred = t(e_SERRF_pred)

  dta = e_SERRF_pred[,QC.index.test[[i]]]

  # dta = mTIC_norm(e=dta,f=f,p=p[QC.index.test[[i]],])$e

  SERRF.QC.CV.[[i]] = RSD(dta,f,p[QC.index.test[[i]],],cl=cl)
}
SERRF.QC.CV = apply(do.call("cbind",SERRF.QC.CV.),1,mean, na.rm=T)
cat(paste0("SERRF normalization QC CV RSD is ", signif(median(SERRF.QC.CV, na.rm = T), 4)*100,"%. ",signif(sum(SERRF.QC.CV<0.10, na.rm = T)/nrow(f),4)*100,"% of QC CV RSD < 10%.\n"))
if(NISTavailable){
  cat("SERRF normalization validate QC RSD is ", signif(median(SERRF.validate, na.rm = T), 4)*100,"%. ",signif(sum(SERRF.validate<0.10, na.rm = T)/nrow(f),4)*100,"% of validate QC RSD < 10%.\n")
}
dta = data$original
dta[5:nrow(dta),3:ncol(dta)] = result_norm[['SERRF']]$e
write.csv(dta, file="normalized-data-sets\\normalization-result-SERRF-normalization.csv",row.names=FALSE)


# SVM5
cat("\n<========== SVM Normalization Started! ==========>\n")
multiple = 5
result_norm[['SVM']] = SVM_norm(e, f, p, QC.index, multiple = 5, time = "Acq. Date-Time")
SVM.validate = RSD(result_norm[['SVM']]$e[,p$`Stat Level 1`=="NIST"],f,p[p$`Stat Level 1`=="NIST",],cl=cl)
result_norm_SVM_CV = list()
SVM.QC.CV. = SVM.validate. = list()
for(j in 1:n_CV){
  e_SVM_pred = e
  qc. = QC.index.train[[j]]
  for(i in 1:nrow(f)){
    all.cor <- apply(e[,qc.], 1,function(x) {cor(e[1,qc.], x)})
    cor.peak <-match(sort(all.cor, decreasing = TRUE)[2:(as.numeric(multiple)+1)], all.cor)
    if (multiple != 1) {
      svr.reg <- svm(t(e[cor.peak,qc.]),e[i,qc.])
      newdata = t(e[cor.peak, ])
      pred = predict(svr.reg, newdata = newdata)
      e_SVM_pred[i,] = (e[i,]/pred)*median(e[i,qc.])
    } else{
      svr.reg <- svm(e[i,qc.] ~ p$`Acq. Date-Time`[qc.])
      pred = predict(svr.reg, newdata = p$`Acq. Date-Time`)
      e_SVM_pred[i,] = (e[i,]/pred)*median(e[i,qc.])
    }
  }
  result = e_SVM_pred
  SVM.QC.CV.[[j]] = RSD(result[,QC.index.test[[j]]],f,p[QC.index.test[[j]],],cl=cl)
}
SVM.QC.CV = apply(do.call("cbind",SVM.QC.CV.),1,mean, na.rm=T)
cat(paste0("SVM normalization QC CV RSD is ", signif(median(SVM.QC.CV, na.rm = T), 4)*100,"%. ",signif(sum(SVM.QC.CV<0.10, na.rm = T)/nrow(f),4)*100,"% of QC CV RSD < 10%.\n"))
if(NISTavailable){cat("SVM normalization validate QC RSD is ", signif(median(SVM.validate, na.rm = T), 4)*100,"%. ",signif(sum(SVM.validate<0.10, na.rm = T)/nrow(f),4)*100,"% of validate QC RSD < 10%.\n")}
dta = data$original
dta[5:nrow(dta),3:ncol(dta)] = result_norm[['SVM']]$e
write.csv(dta, file="normalized-data-sets\\normalization-result-svm-normalization.csv",row.names=FALSE)

# sum normalization.
cat("\n<========== sum Normalization Started! ==========>\n")
result_norm[['sum']] = (sum_norm(e=e,f=f,p=p))
sum.QC.CV = RSD(result_norm[['sum']]$e[,p$`Stat Level 1`=='QC'],f,p[p$`Stat Level 1`=='QC',],cl=cl)
sum.validate = RSD(result_norm[['sum']]$e[,p$`Stat Level 1`=='NIST'],f,p,cl=cl)
cat(paste0("sum normalization QC CV RSD is ", signif(median(sum.QC.CV, na.rm = T), 4)*100,"%. ", signif(sum(sum.QC.CV<0.10, na.rm = T)/nrow(f),4)*100,"% of QC CV RSD < 10%.\n"))
if(NISTavailable){
  cat("sum normalization validate QC RSD is ", signif(median(sum.validate, na.rm = T), 4)*100,"%. ", paste0(signif(sum(sum.validate<0.10, na.rm = T)/nrow(f),4)*100,"% of validate QC RSD < 10%.\n"))
}
dta = data$original
dta[5:nrow(dta),3:ncol(dta)] = result_norm[['sum']]$e
write.csv(dta, file="normalized-data-sets\\normalization-result-sum-normalization.csv",row.names=FALSE)


# median normalization.
cat("\n<========== median Normalization Started! ==========>\n")
result_norm[['median']] = (median_norm(e=e,f=f,p=p))
median.QC.CV = RSD(result_norm[['median']]$e[,p$`Stat Level 1`=='QC'],f,p[p$`Stat Level 1`=='QC',],cl=cl)
median.validate = RSD(result_norm[['median']]$e[,p$`Stat Level 1`=='NIST'],f,p,cl=cl)
cat(paste0("median normalization QC CV RSD is ", signif(median(median.QC.CV, na.rm = T), 4)*100,"%. ", signif(median(median.QC.CV<0.10, na.rm = T)/nrow(f),4)*100,"% of QC CV RSD < 10%.\n"))
if(NISTavailable){
  cat("median normalization validate QC RSD is ", signif(median(median.validate, na.rm = T), 4)*100,"%. ", paste0(signif(median(median.validate<0.10, na.rm = T)/nrow(f),4)*100,"% of validate QC RSD < 10%.\n"))
}
dta = data$original
dta[5:nrow(dta),3:ncol(dta)] = result_norm[['median']]$e
write.csv(dta, file="normalized-data-sets\\normalization-result-median-normalization.csv",row.names=FALSE)


# PQN normalization.
cat("\n<========== PQN Normalization Started! ==========>\n")
result_norm[['PQN']] = (PQN_norm(e=e,f=f,p=p))
PQN.QC.CV = RSD(result_norm[['PQN']]$e[,p$`Stat Level 1`=='QC'],f,p[p$`Stat Level 1`=='QC',],cl=cl)
PQN.validate = RSD(result_norm[['PQN']]$e[,p$`Stat Level 1`=='NIST'],f,p,cl=cl)
cat(paste0("PQN normalization QC CV RSD is ", signif(median(PQN.QC.CV, na.rm = T), 4)*100,"%. ", signif(sum(PQN.QC.CV<0.10, na.rm = T)/nrow(f),4)*100,"% of QC CV RSD < 10%.\n"))
if(NISTavailable){
  cat("PQN normalization validate QC RSD is ", signif(median(PQN.validate, na.rm = T), 4)*100,"%. ", paste0(signif(sum(PQN.validate<0.10, na.rm = T)/nrow(f),4)*100,"% of validate QC RSD < 10%.\n"))
}
dta = data$original
dta[5:nrow(dta),3:ncol(dta)] = result_norm[['PQN']]$e
write.csv(dta, file="normalized-data-sets\\normalization-result-PQN-normalization.csv",row.names=FALSE)


# contrast normalization.
cat("\n<========== contrast Normalization Started! ==========>\n")
result_norm[['contrast']] = (contrast_norm(e=e,f=f,p=p))
contrast.QC.CV = RSD(result_norm[['contrast']]$e[,p$`Stat Level 1`=='QC'],f,p[p$`Stat Level 1`=='QC',],cl=cl)
contrast.validate = RSD(result_norm[['contrast']]$e[,p$`Stat Level 1`=='NIST'],f,p,cl=cl)
cat(paste0("contrast normalization QC CV RSD is ", signif(median(contrast.QC.CV, na.rm = T), 4)*100,"%. ", signif(sum(contrast.QC.CV<0.10, na.rm = T)/nrow(f),4)*100,"% of QC CV RSD < 10%.\n"))
if(NISTavailable){
  cat("contrast normalization validate QC RSD is ", signif(median(contrast.validate, na.rm = T), 4)*100,"%. ", paste0(signif(sum(contrast.validate<0.10, na.rm = T)/nrow(f),4)*100,"% of validate QC RSD < 10%.\n"))
}
dta = data$original
dta[5:nrow(dta),3:ncol(dta)] = result_norm[['contrast']]$e
write.csv(dta, file="normalized-data-sets\\normalization-result-contrast-normalization.csv",row.names=FALSE)

# quantile normalization.
cat("\n<========== quantile Normalization Started! ==========>\n")
result_norm[['quantile']] = (quantile_norm(e=e,f=f,p=p))
quantile.QC.CV = RSD(result_norm[['quantile']]$e[,p$`Stat Level 1`=='QC'],f,p[p$`Stat Level 1`=='QC',],cl=cl)
quantile.validate = RSD(result_norm[['quantile']]$e[,p$`Stat Level 1`=='NIST'],f,p,cl=cl)
cat(paste0("quantile normalization QC CV RSD is ", signif(median(quantile.QC.CV, na.rm = T), 4)*100,"%. ", signif(sum(quantile.QC.CV<0.10, na.rm = T)/nrow(f),4)*100,"% of QC CV RSD < 10%.\n"))
if(NISTavailable){
  cat("quantile normalization validate QC RSD is ", signif(median(quantile.validate, na.rm = T), 4)*100,"%. ", paste0(signif(sum(quantile.validate<0.10, na.rm = T)/nrow(f),4)*100,"% of validate QC RSD < 10%.\n"))
}
dta = data$original
dta[5:nrow(dta),3:ncol(dta)] = result_norm[['quantile']]$e
write.csv(dta, file="normalized-data-sets\\normalization-result-quantile-normalization.csv",row.names=FALSE)

# linear normalization.
cat("\n<========== linear Normalization Started! ==========>\n")
result_norm[['linear']] = (linear_norm(e=e,f=f,p=p))
linear.QC.CV = RSD(result_norm[['linear']]$e[,p$`Stat Level 1`=='QC'],f,p[p$`Stat Level 1`=='QC',],cl=cl)
linear.validate = RSD(result_norm[['linear']]$e[,p$`Stat Level 1`=='NIST'],f,p,cl=cl)
cat(paste0("linear normalization QC CV RSD is ", signif(median(linear.QC.CV, na.rm = T), 4)*100,"%. ", signif(sum(linear.QC.CV<0.10, na.rm = T)/nrow(f),4)*100,"% of QC CV RSD < 10%.\n"))
if(NISTavailable){
  cat("linear normalization validate QC RSD is ", signif(median(linear.validate, na.rm = T), 4)*100,"%. ", paste0(signif(sum(linear.validate<0.10, na.rm = T)/nrow(f),4)*100,"% of validate QC RSD < 10%.\n"))
}
dta = data$original
dta[5:nrow(dta),3:ncol(dta)] = result_norm[['linear']]$e
write.csv(dta, file="normalized-data-sets\\normalization-result-linear-normalization.csv",row.names=FALSE)


# liwong normalization.
cat("\n<========== Li-Wong Normalization Started! ==========>\n")
result_norm[['liwong']] = tryCatch(liwong_norm(e=e,f=f,p=p), error = function(e){
  return(NA)
})
if(is.na(result_norm[['liwong']])){
  liwong.QC.CV = liwong.validate = NA
}else{
  liwong.QC.CV = RSD(result_norm[['liwong']]$e[,p$`Stat Level 1`=='QC'],f,p[p$`Stat Level 1`=='QC',],cl=cl)
  liwong.validate = RSD(result_norm[['liwong']]$e[,p$`Stat Level 1`=='NIST'],f,p,cl=cl)
  cat(paste0("liwong normalization QC CV RSD is ", signif(median(liwong.QC.CV, na.rm = T), 4)*100,"%. ", signif(sum(liwong.QC.CV<0.10, na.rm = T)/nrow(f),4)*100,"% of QC CV RSD < 10%.\n"))
  if(NISTavailable){
    cat("liwong normalization validate QC RSD is ", signif(median(liwong.validate, na.rm = T), 4)*100,"%. ", paste0(signif(sum(liwong.validate<0.10, na.rm = T)/nrow(f),4)*100,"% of validate QC RSD < 10%.\n"))
  }
  dta = data$original
  dta[5:nrow(dta),3:ncol(dta)] = result_norm[['liwong']]$e
  write.csv(dta, file="normalized-data-sets\\normalization-result-liwong-normalization.csv",row.names=FALSE)
}


# cubic normalization.
cat("\n<========== Cubic Normalization Started! ==========>\n")
result_norm[['cubic']] = tryCatch(cubic_norm(e=e,f=f,p=p), error = function(e){
  return(NA)
})
if(is.na(result_norm[['cubic']])){
  cubic.QC.CV = cubic.validate = NA
}else{
  cubic.QC.CV = RSD(result_norm[['cubic']]$e[,p$`Stat Level 1`=='QC'],f,p[p$`Stat Level 1`=='QC',],cl=cl)
  cubic.validate = RSD(result_norm[['cubic']]$e[,p$`Stat Level 1`=='NIST'],f,p,cl=cl)
  cat(paste0("cubic normalization QC CV RSD is ", signif(median(cubic.QC.CV, na.rm = T), 4)*100,"%. ", signif(sum(cubic.QC.CV<0.10, na.rm = T)/nrow(f),4)*100,"% of QC CV RSD < 10%.\n"))
  if(NISTavailable){
    cat( "cubic normalization validate QC RSD is ", signif(median(cubic.validate, na.rm = T), 4)*100,"%. ", paste0(signif(sum(cubic.validate<0.10, na.rm = T)/nrow(f),4)*100,"% of validate QC RSD < 10%.\n"))
  }
  dta = data$original
  dta[5:nrow(dta),3:ncol(dta)] = result_norm[['cubic']]$e
  write.csv(dta, file="normalized-data-sets\\normalization-result-cubic-normalization.csv",row.names=FALSE)
}


# batchratio
cat("\n<========== Batch-Ratio Median Normalization Started! ==========>\n")
result_norm[['batchratio']] = batchratio_norm(e, f, p,batch,QC.index)
batchratio.validate = RSD(result_norm[['batchratio']]$e[,p$`Stat Level 1`=="NIST"],f,p[p$`Stat Level 1`=="NIST",],cl=cl)
result_norm_batchratio_CV = list()
batchratio.QC.CV. = batchratio.validate. = list()
for(j in 1:n_CV){
  qc. = QC.index.train[[j]]
  e_batch_norm = matrix(,nrow=nrow(e),ncol=ncol(e))
  for(i in 1:nrow(f)){
    means = by(as.numeric(e[i,qc.]),batch[i,qc.], mean, na.rm=T)
    mean_means = mean(means)
    e_batch_norm[i,] = as.numeric(e[i,])/(rep(means,times=table(batch[i,]))/mean_means)
  }
  batchratio.QC.CV.[[j]] = RSD(e_batch_norm[,QC.index.test[[j]]],f,p[QC.index.test[[j]],],cl=cl)
}
batchratio.QC.CV = apply(do.call("cbind",batchratio.QC.CV.),1,mean, na.rm=T)
cat(paste0("batchratio normalization QC CV RSD is ", signif(median(batchratio.QC.CV, na.rm = T), 4)*100,"%. "))
cat(paste0(signif(sum(batchratio.QC.CV<0.10, na.rm = T)/nrow(f),4)*100,"% of QC CV RSD < 10%.\n"))
if(NISTavailable){
  cat(paste0("batchratio normalization validate QC RSD is ", signif(median(batchratio.validate, na.rm = T), 4)*100,"%. "))
  cat(paste0(signif(sum(batchratio.validate<0.10, na.rm = T)/nrow(f),4)*100,"% of validate QC RSD < 10%.\n"))
}
dta = data$original
dta[5:nrow(dta),3:ncol(dta)] = result_norm[['batchratio']]$e
write.csv(dta, file="normalized-data-sets\\normalization-result-batchratio-normalization.csv",row.names=FALSE)

# NOMIS [CCMN, RUV2 are supervised methods and are not in the consideration. The normalization should be bio-info blind (unsupervised)]
# cat("\n<========== NOMIS Normalization Started! ==========>\n")
# library(metabolomics,  verbose = F)
# inputdata = data.frame(Group = "A", t(e))
# colnames(inputdata) = c("Group",f$Annotation)
# rownames(inputdata) = paste0("S",1:nrow(inputdata))
#
# result_norm[["NOMIS"]]$e = t(Normalise(inputdata,method = 'nomis',nc = f$Know == "ISTD")$output[,-1])
# NOMIS.QC.CV = RSD(result_norm[['NOMIS']]$e[,p$`Stat Level 1`=='QC'],f,p[p$`Stat Level 1`=='QC',],cl=cl)
#
# NOMIS.validate = RSD(result_norm[['NOMIS']]$e[,p$`Stat Level 1`=='NIST'],f,p[p$`Stat Level 1`=='NIST',],cl=cl)
#
# cat(paste0("NOMIS normalization QC CV RSD is ", signif(median(NOMIS.QC.CV), 4)*100,"%. "))
# cat(paste0(signif(sum(NOMIS.QC.CV<0.10)/nrow(f),4)*100,"% of QC CV RSD < 10%.\n"))
# cat(paste0("NOMIS normalization validate QC RSD is ", signif(median(mTIC.validate), 4)*100,"%. "))
# cat(paste0(signif(sum(NOMIS.validate<0.10)/nrow(f),4)*100,"% of validate QC RSD < 10%.\n"))
# dta = data$original
# dta[5:nrow(dta),3:ncol(dta)] = result_norm[['NOMIS']]$e
# write.csv(dta, file="normalized-data-sets\\normalization-result-nomis-normalization.csv",row.names=FALSE)


# cyclic normalization.
# cat("<========== Cyclic Normalization Started! ==========>\n")
# result_norm[['cyclic']] = (cyclic_norm(e=e,f=f,p=p))
# cyclic.QC.CV = RSD(result_norm[['cyclic']]$e[,p$`Stat Level 1`=='QC'],f,p[p$`Stat Level 1`=='QC',],cl=cl)
# cyclic.validate = RSD(result_norm[['cyclic']]$e[,p$`Stat Level 1`=='NIST'],f,p,cl=cl)
# cat(paste0("cyclic normalization QC CV RSD is ", signif(median(cyclic.QC.CV), 4)*100,"%. ", signif(sum(cyclic.QC.CV<0.10)/nrow(f),4)*100,"% of QC CV RSD < 10%.\n", "cyclic normalization validate QC RSD is ", signif(median(cyclic.validate), 4)*100,"%. ", paste0(signif(sum(cyclic.validate<0.10)/nrow(f),4)*100,"% of validate QC RSD < 10%.\n")))
# dta = data$original
# dta[5:nrow(dta),3:ncol(dta)] = result_norm[['cyclic']]$e
# write.csv(dta, file="normalized-data-sets\\normalization-result-cyclic-normalization.csv",row.names=FALSE)


# save performance CV QC RSD and validate RSD
performance = data.frame(method = names(result_norm),
                         QC.CV.RSD = c(none = median(none.QC.CV, na.rm = T),
                                       mTIC = median(mTIC.QC.CV, na.rm = T),
                                       sum = median(sum.QC.CV, na.rm = T),
                                       median = median(median.QC.CV, na.rm = T),
                                       PQN = median(PQN.QC.CV, na.rm = T),
                                       contrast = median(contrast.QC.CV, na.rm = T),
                                       linear = median(linear.QC.CV, na.rm = T),
                                       liwong = median(liwong.QC.CV, na.rm = T),
                                       quantile = median(quantile.QC.CV, na.rm = T),
                                       cubic = median(cubic.QC.CV, na.rm = T),
                                       batchratio = median(batchratio.QC.CV, na.rm = T),
                                       loess = median(loess.QC.CV, na.rm = T),
                                       SVM = median(SVM.QC.CV, na.rm = T),
                                       # NOMIS = median(NOMIS.QC.CV, na.rm = T),
                                       SERRF = median(SERRF.QC.CV, na.rm = T)
                                       # ,cyclic = median(cyclic.QC.CV, na.rm = T)
                         )[names(result_norm)],
                         validate.QC.RSD = sapply(names(result_norm),function(x){
                           # for(x in 1:length(result_norm)){
                           if(sum(is.na(result_norm[[x]]))>0){
                             return(NA)
                           }else{
                             return(median(RSD(result_norm[[x]]$e[,p$`Stat Level 1`=='NIST'],f,p[p$`Stat Level 1`=='NIST',],cl=cl), na.rm = T))
                           }
                           # }


                         })
)
if(!NISTavailable){
  performance = performance[,1:2]
}


write.csv(performance, "normalization-performance.csv")

# detailed performance on each compound.
performanceQC.CV = do.call("cbind",list(none.QC.CV,
                                        mTIC.QC.CV,
                                        sum.QC.CV,
                                        median.QC.CV,
                                        PQN.QC.CV,
                                        contrast.QC.CV,
                                        linear.QC.CV,
                                        liwong.QC.CV,
                                        quantile.QC.CV,
                                        cubic.QC.CV,
                                        batchratio.QC.CV,
                                        loess.QC.CV,
                                        SVM.QC.CV,
                                        # NOMIS.QC.CV,
                                        SERRF.QC.CV
                                        # ,cyclic.QC.CV
))
colnames(performanceQC.CV) = c("none",'mTIC','sum','median','PQN','contrast','linear','liwong','quantile','cubic','batchratio','loess','SVM',
                               # 'NOMIS',
                               'SERRF'
                               # ,'cyclic'
)
rownames(performanceQC.CV) = f$label
write.csv(performanceQC.CV, "normalization-performance-QCCVRSD-eachCompound.csv")

if(NISTavailable){
  performancevalidateCV = sapply(names(result_norm),function(x) {
    if(is.na(result_norm[[x]])){
      return(NA)
    }else{
      return(RSD(result_norm[[x]]$e[,p$`Stat Level 1`=='NIST'],f,p[p$`Stat Level 1`=='NIST',],cl=cl))
    }
  })
  write.csv(performancevalidateCV, "normalization-performance-validateRSD-eachCompound.csv")
}



cat("Drawing PCA plot for each of the normalization method.")
# Generate PCA for each method.
doc = pptx( )

for(method in names(result_norm)){
  if(sum(is.na(result_norm[[method]]))>0){

  }else{
    doc = addSlide(doc, slide.layout = "Title and Content")

    index1 = apply(result_norm[[method]]$e, 1, function(x){
      !sd(x, na.rm = T) == 0
    })# remove zero standard deviation
    index2 = !is.na(index1)

    index = index1 & index2

    dta = result_norm[[method]]$e[index,]

    for(i in 1:nrow(dta)){
      dta[i,is.na(dta[i,])] = median( dta[i,!is.na(dta[i,])])
    }

    doc = addPlot(doc, fun = function() {


      print(generate_PCA(dta,f[index,],p,batch = batch[index,],QC.index =  QC.index,method))



    },
    vector.graphic = TRUE, width = 6, height = 6)
  }


}
# write the document to a file
writeDoc(doc, file = "PCA score plots.pptx")


cat("The QC CV RSD and validate QC RSD can be summarized in the bar plots.\n")
# bar plot on QC.CV.
QC.CVRSD = sort(c(raw = median(none.QC.CV, na.rm = T),
                  mTIC = median(mTIC.QC.CV, na.rm = T),
                  sum = median(sum.QC.CV, na.rm = T),
                  median = median(median.QC.CV, na.rm = T),
                  PQN = median(PQN.QC.CV, na.rm = T),
                  CONTRAST = median(contrast.QC.CV, na.rm = T),
                  linear = median(linear.QC.CV, na.rm = T),
                  liwong = median(liwong.QC.CV, na.rm = T),
                  quantile = median(quantile.QC.CV, na.rm = T),
                  cubic = median(cubic.QC.CV, na.rm = T),
                  batch_ratio = median(batchratio.QC.CV, na.rm = T),
                  loess = median(loess.QC.CV, na.rm = T),
                  SVM = median(SVM.QC.CV, na.rm = T),
                  # NOMIS = median(NOMIS.QC.CV),
                  SERRF = median(SERRF.QC.CV, na.rm = T)
                  # ,cyclic = median(cyclic.QC.CV)
),decreasing = T)



cat("\nCONCLUSION: The ",names(QC.CVRSD)[length(QC.CVRSD)]," outperformed others with a average CV QC RSD of ",signif(QC.CVRSD[length(QC.CVRSD)])*100,"%.\n")
QC.CVRSD.ggplot2Data = data.frame(RSD = signif(QC.CVRSD,3),methods = factor(names(QC.CVRSD),levels = names(QC.CVRSD)))
ggplot(QC.CVRSD.ggplot2Data, aes(x = methods, y = RSD)) + geom_bar(stat = "identity",fill = c(rep("black",length(QC.CVRSD)-2),'red','gold'))+
  theme.scatter+
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=90, hjust=1))+
  geom_text(aes(label=RSD), vjust=-1, colour="black")


# Generate bar plot for evaluation.
doc = pptx( )


doc = addSlide(doc, slide.layout = "Title and Content")
doc = addPlot(doc, fun = function(){

  print(ggplot(QC.CVRSD.ggplot2Data, aes(x = methods, y = RSD)) + geom_bar(stat = "identity",fill = c(rep("black",length(QC.CVRSD)-2),'red','gold'))+theme.scatter+theme(text = element_text(size=20),
                                                                                                                                                                         axis.text.x = element_text(angle=90, hjust=1))+
          geom_text(aes(label=RSD), vjust=-1, colour="black"))


} ,
vector.graphic = TRUE, width = 8, height = 8)


# write the document to a file
writeDoc(doc, file = "QC CV RSD bar-plot.pptx")

if(NISTavailable){
  # bar plot on validate
  validateRSD = sort(c(raw = median(none.validate, na.rm = T),
                       mTIC = median(mTIC.validate, na.rm = T),
                       sum = median(sum.validate, na.rm = T),
                       median = median(median.validate, na.rm = T),
                       PQN = median(PQN.validate, na.rm = T),
                       CONTRAST = median(contrast.validate, na.rm = T),
                       linear = median(linear.validate, na.rm = T),
                       liwong = median(liwong.validate, na.rm = T),
                       quantile = median(quantile.validate, na.rm = T),
                       cubic = median(cubic.validate, na.rm = T),
                       batch = median(batchratio.validate, na.rm = T),
                       loess = median(loess.validate, na.rm = T),
                       SVM = median(SVM.validate, na.rm = T),
                       # NOMIS = median(NOMIS.validate, na.rm = T),
                       SERRF = median(SERRF.validate, na.rm = T)
                       # ,cyclic = median(cyclic.validate, na.rm = T)
  ),decreasing = T)
  cat("CONCLUSION: The ",names(validateRSD)[length(validateRSD)]," outperformed others with a average validate QC RSD of ",signif(validateRSD[length(validateRSD)])*100,"%.\n")
  validateRSD.ggplot2Data = data.frame(RSD = signif(validateRSD,3),methods = factor(names(validateRSD),levels = names(validateRSD)))
  ggplot(validateRSD.ggplot2Data, aes(x = methods, y = RSD)) + geom_bar(stat = "identity",fill = c(rep("black",length(validateRSD)-2),'red','gold'))+
    theme.scatter+
    theme(text = element_text(size=20),
          axis.text.x = element_text(angle=90, hjust=1))+
    geom_text(aes(label=RSD), vjust=-1, colour="black")


  # Generate bar plot for evaluation.
  doc = pptx( )


  doc = addSlide(doc, slide.layout = "Title and Content")
  doc = addPlot(doc, fun = function(){

    print(ggplot(validateRSD.ggplot2Data, aes(x = methods, y = RSD)) + geom_bar(stat = "identity",fill = c(rep("black",length(validateRSD)-2),'red','gold'))+
            theme.scatter+
            theme(text = element_text(size=20),
                  axis.text.x = element_text(angle=90, hjust=1))+
            geom_text(aes(label=RSD), vjust=-1, colour="black")

    )


  } ,
  vector.graphic = TRUE, width = 8, height = 8)
  # write the document to a file
  writeDoc(doc, file = "validate RSD bar-plot.pptx")

}


cat("\nGOOD JOB. DATA NORMALIZATION FINISHED!")

# library(caret)
# library(dplyr)         # Used by caret
# library(kernlab)       # support vector machine
# library(pROC)	       # plot the ROC curves
# pacman::p_load(doParallel)
#
# cl <- makeCluster(min(detectCores(),20), outfile="")
# registerDoParallel(cl)
# # evaluate predicting performance on the validate set only to avoid overfitting.
# set.seed(seed+1) # avoid having same seed.
# trainIndex <- createDataPartition(dataxgb$gender,p=.5,list=FALSE)
# ctrl <- trainControl(method="cv",   # 10fold cross validation
#                      summaryFunction=twoClassSummary,	# Use AUC to pick the best model
#                      classProbs=TRUE,
#                      allowParallel=T)
# X = result_norm[['none']]$e
# pvalue = vector()
# for(i in 1:nrow(result_norm[[method]]$e)){
#   pvalue[i] = t.test(result_norm[[method]]$e[i,]~p$Gender)$p.value
# }
# dataxgb = data.frame(t(X[p.adjust(pvalue, method = "fdr")<0.05,!is.na(p$Gender)]),
#                      gender = p$Gender[!is.na(p$Gender)])
# trainData <- dataxgb[trainIndex,]
# testData  <- dataxgb[-trainIndex,] # this is the validate set.
# trainX <-trainData[,-ncol(dataxgb)]
# grid=data.frame(nrounds = 100, max_depth = 12, gamma = 0, min_child_weight = 12, colsample_bytree = 1, subsample = 1, eta = 0.01) # parameter selected using cross validation (time consuming) on the trainData.
# none_xgb <- train(x=trainX,
#                    y= trainData$gender,
#                    method = "xgbTree", # Xgboost
#                    tuneGrid = grid,
#                    metric="ROC",
#                    trControl=ctrl)
# none_pred = predict(none_xgb, newdata = testData[,-ncol(dataxgb)], type='prob')[,1]
# auc(pROC::roc(predictor = none_pred,response = as.factor(as.numeric(testData[,ncol(dataxgb)])-1)))
#
#
# X = result_norm[['loess']]$e
# pvalue = vector()
# for(i in 1:nrow(result_norm[[method]]$e)){
#   pvalue[i] = t.test(result_norm[[method]]$e[i,]~p$Gender)$p.value
# }
# dataxgb = data.frame(t(X[p.adjust(pvalue, method = "fdr")<0.05,!is.na(p$Gender)]),
#                      gender = p$Gender[!is.na(p$Gender)])
# trainData <- dataxgb[trainIndex,]
# testData  <- dataxgb[-trainIndex,] # this is the validate set.
# trainX <-trainData[,-ncol(dataxgb)]
# grid=data.frame(nrounds = 150, max_depth = 12, gamma = 0, min_child_weight = 2, colsample_bytree = 0.8, subsample = 0.8, eta = 0.06)# parameter selected using cross validation (time consuming) on the trainData.
# loess_xgb <- train(x=trainX,
#                   y= trainData$gender,
#                   method = "xgbTree", # Xgboost
#                   tuneGrid = grid,
#                   metric="ROC",
#                   trControl=ctrl)
# loess_pred = predict(loess_xgb, newdata = testData[,-ncol(dataxgb)], type='prob')[,1]
# auc(pROC::roc(predictor = loess_pred,response = as.factor(as.numeric(testData[,ncol(dataxgb)])-1)))
#
#
# X = result_norm[['SERRF']]$e
# pvalue = vector()
# for(i in 1:nrow(result_norm[[method]]$e)){
#   pvalue[i] = t.test(result_norm[[method]]$e[i,]~p$Gender)$p.value
# }
# dataxgb = data.frame(t(X[p.adjust(pvalue, method = "fdr")<0.05,!is.na(p$Gender)]),
#                      gender = p$Gender[!is.na(p$Gender)])
# trainData <- dataxgb[trainIndex,]
# testData  <- dataxgb[-trainIndex,] # this is the validate set.
# trainX <-trainData[,-ncol(dataxgb)]
# grid=data.frame(nrounds = 150, max_depth = 6, gamma = 2, min_child_weight = 5, colsample_bytree = 0.6, subsample = 0.8, eta = 0.05)# parameter selected using cross validation (time consuming) on the trainData.
# SERRF_xgb <- train(x=trainX,
#                    y= trainData$gender,
#                    method = "xgbTree", # Xgboost
#                    tuneGrid = grid,
#                    metric="ROC",
#                    trControl=ctrl)
# serrf_pred = predict(SERRF_xgb, newdata = testData[,-ncol(dataxgb)], type='prob')[,1]
# auc(pROC::roc(predictor = serrf_pred,response = as.factor(as.numeric(testData[,ncol(dataxgb)])-1)))
#
#
#
# # Generate AUROC for each method.
# doc = pptx( )
#
#
#   doc = addSlide(doc, slide.layout = "Title and Content")
#   doc = addPlot(doc, fun = function() {
#     plot(roc(predictor = none_pred,response = as.factor(as.numeric(testData[,ncol(dataxgb)])-1), direction=">"),col="black", lwd=3, main="AUROC")
#     lines(roc(predictor = loess_pred,response = as.factor(as.numeric(testData[,ncol(dataxgb)])-1), direction=">"),col="red", lwd=3, main="AUROC")
#     lines(roc(predictor = serrf_pred,response = as.factor(as.numeric(testData[,ncol(dataxgb)])-1), direction=">"),col="gold", lwd=3, main="AUROC")
#   },
#                 vector.graphic = TRUE, width = 6, height = 6)
#
#
# # write the document to a file
# writeDoc(doc, file = "AUROC.pptx")
