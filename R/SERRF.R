SERRF = function(input = "C:\\Users\\Sili Fan\\Downloads\\Constanze GCTOF for Normalization.xlsx", ip = '128.120.143.234'){

  library(rgeolocate)

  start = Sys.time()

  readData = function(path =  "G:\\data\\D\\data project D.xlsx"){

    #check if it is csv of xlsx
    if(grepl("xlsx", path)){
      d <- openxlsx::read.xlsx(path, sheet = 1,colNames = FALSE)
    }else if(grepl("csv", path)){
      # file = "C:\\Users\\Sili Fan\\Downloads\\val (18).csv"
      d <- data.table::fread(path)
    }

    # make "" as NA
    d[d==""] <- NA

    #### fData
    fData <- d[!is.na(d[,1]),c(which(is.na(d[1,])),sum(is.na(d[1,]))+1)] # The first row and column is critical of formating the data.
    colnames(fData) = as.character(fData[1,]); fData = data.frame(fData[-1,],stringsAsFactors = F,check.names = FALSE);rownames(fData) = 1:nrow(fData);
    # following steps keeps the column type.
    fData.=lapply(fData,function(x){
      if(sum(!is.na(as.numeric(x))) == length(x)){
        as.numeric(x)
      }else{
        x
      }
    })
    fData. = do.call(cbind, lapply(fData., data.frame, stringsAsFactors=FALSE))
    colnames(fData.) = colnames(fData)
    fData = fData.

    fData = fData[,c(ncol(fData),2:ncol(fData)-1)]
    fData[[1]] = make.unique(fData[[1]], sep = '_')

    #### pData
    pData <- d[c(which(is.na(d[,1])),max(which(is.na(d[,1])))+1) ,!is.na(d[1,])]
    pData <- t(pData); colnames(pData) = pData[1,]; pData = data.frame(pData[-1,],stringsAsFactors = F,check.names = FALSE)
    # following steps keeps the column type.
    pData.=lapply(pData,function(x){
      if(sum(!is.na(as.numeric(x))) == length(x)){
        as.numeric(x)
      }else{
        x
      }
    })
    pData. = do.call(cbind, lapply(pData., data.frame, stringsAsFactors=FALSE))
    colnames(pData.) = colnames(pData)
    pData = pData.

    pData = pData[,c(ncol(pData),2:ncol(pData)-1)]
    pData[[1]] = make.unique(make.names(pData[[1]]), sep = '_')

    #### eData
    eData <- d[!is.na(d[,1]),!is.na(d[1,])][-1,-1]
    eData <- sapply(eData, as.numeric)
    eData <- data.frame(eData,stringsAsFactors = F)
    colnames(eData) = pData[[1]]; rownames(eData) = fData[[1]]

    # # remove any unwanted character in columns of eData, fData and pData to _.
    # colnames(eData) = gsub("([_])|[[:punct:]]", "_", colnames(eData))
    # colnames(fData) = gsub("([_])|[[:punct:]]", "_", colnames(fData))
    # colnames(pData) = gsub("([_])|[[:punct:]]", "_", colnames(pData))

    # remove all the NA. And replace NA with "NA" Otherwise DataTables will give error.datatables warning requested unknown parameter
    # eData[is.na(eData)]="NA"
    # fData[is.na(fData)]="NA"
    # pData[is.na(pData)]="NA"

    # remove unwanted character in p.
    # for(i in 1:nrow(pData)){
    #   for(j in 1:ncol(pData)){
    #     pData[i,j] = gsub("\\+|~|-", " ", pData[i,j])
    #   }
    # }

    return(list(e = eData, f = fData, p = pData))

  }

  info = read.csv(paste0("http://localhost:5984/serrf/info/info.csv"), stringsAsFactors = F,na.strings = "")


  file <- system.file("extdata","ip2_sample.bin", package = "rgeolocate")

  info$num[info$code == ip2location(ip, file, c("country_code"))[[1]]] =   info$num[info$code == ip2location(ip, file, c("country_code"))[[1]]]+1

  put_att_csv = function(projectID = 'tryThu.Aug.17.14.53.35.2017', attname = 'test.csv', att = data.table::fread("G:\\initialize MetDA\\user_active.csv")){
    projectUrl <- paste0("http://localhost:5984/serrf/",projectID)
    projectList <- jsonlite::fromJSON(projectUrl)

    new_att = projectList[["_attachments"]]
    new_att = new_att[!names(new_att)%in%attname]
    new_att[[attname]] = list(content_type="text/csv", data = RCurl::base64(
      paste0(R.utils::captureOutput(write.csv(att,stdout(), row.names=F)),collapse = "\n")
    ))
    projectList[["_attachments"]] = new_att
    result = RCurl::getURL(paste0("http://localhost:5984/serrf/",projectID),customrequest='PUT',httpheader=c('Content-Type'='application/json'),postfields= jsonlite::toJSON(projectList,auto_unbox = T, force = T))
    while(grepl("error",result)) {
      projectList <- jsonlite::fromJSON(projectUrl)
      new_att = projectList[["_attachments"]]
      new_att = new_att[!names(new_att)%in%attname]
      new_att[[attname]] = list(content_type="text/csv", data = RCurl::base64(
        paste0(R.utils::captureOutput(write.csv(att,stdout(), row.names=F)),collapse = "\n")
      ))
      projectList[["_attachments"]] = new_att
      result = RCurl::getURL(paste0("http://localhost:5984/serrf/",projectID),customrequest='PUT',httpheader=c('Content-Type'='application/json'),postfields= jsonlite::toJSON(projectList,auto_unbox = T, force = T))
      if(grepl("ok",result)){
        break;
      }
    }
    return(result)
  }

  put_att_csv("info", "info.csv", info)

  data = readData(input)

  e = data$e
  f = data$f
  p = data$p

  # impute missing value.
  if(sum(is.na(e))>0){
    missing_compounds = which(is.na(e), arr.ind = T)[,1]
    for(i in missing_compounds){
      e[i, is.na(e[i,])] = 1/2 * min(e[i,!is.na(e[i,])])
    }
  }
  e = data.matrix(e)


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

  # define batch.
  batch = matrix(rep(p$batch, nrow(f)), nrow = nrow(f), byrow = T)
  QC.index = which(p$type == "QC")

  # parallel computing.
  library(parallel)
  cl = makeCluster(8)

  # SERRF normalization
  SERRF_norm = function(e,f,p,
                        batch = define_batch(e,f,p),
                        QC.index, time = "Acq. Date-Time"){
    library(randomForest)
    qc = rep(F, nrow(p))
    qc[QC.index] = T
    e. = e
    for(i in 1:nrow(e)){ # MAKE SURE THE QC AND SAMPLES ARE AT THE SAME LEVEL. This is critical for SERRF algorithm (and other tree-based machine learning algorithm) because when building each tree, the split on each leaf considers the level of the values. If the values are not consistant, then the RF models will be wrong and the RF will bias the intensity level after normalization (although the relative position won't change.)
      e.[i,qc] = unlist(by(data.frame(e.[i,],qc),batch[1,],function(x){# x = data.frame(e.[i,],qc)[batch[1,]=='A',]
        x[x[,2],1] - (median(x[x[,2],1]) - median(x[!x[,2],1]))
      }))
    }
    pred = parSapply(cl, X = 1:nrow(f), function(j,eData,batch,randomForest, QC.index, time){
      data = data.frame(y = eData[j,], t(eData[-j,]), batch = batch[1,], time = time)
      colnames(data) = c("y", paste0("X",1:nrow(eData))[-j], "batch", "time")
      model = randomForest(y~., data = data,subset = QC.index, importance = F)
      newdata = data.frame(t(eData[-j,]), batch = batch[1,], time = time)
      colnames(newdata) =   c(paste0("X",1:nrow(eData))[-j], "batch", "time")
      new = (eData[j,]/predict(model,newdata = newdata))*median(eData[j,])
      return(new)
    }, e.,batch,randomForest, QC.index, p[[time]])

    e_SERRF_pred = t(pred)
    return(list(e = e_SERRF_pred, p = p, f = f))
  }

  norm = SERRF_norm(e, f, p, batch, QC.index, time = "time")



  # evaluation methods.
  # RSD
  RSD = function(e,f,p,robust = F,cl){
    library(parallel)
    if(robust){
      result=parSapply(cl=cl,X=1:nrow(e),FUN = function(i,remove_outlier,e){
        x = remove_outlier(e[i,])[[1]]
        sd(x,na.rm=T)/mean(x,na.rm=T)
      },remove_outlier,e)
    }else{
      result=parSapply(cl=cl,X=1:nrow(e),FUN = function(i,e){
        x = e[i,]
        sd(x,na.rm=T)/mean(x,na.rm=T)
      },e)
    }


    return(result)
  }
  SERRF.validate = RSD(norm$e[,p$type=="validate"],f,p[p$type=="validate",],cl=cl)
  raw.validate = RSD(e[,p$type=="validate"],f,p[p$type=="validate",],cl=cl)
  stopCluster(cl)
  # PCA
  # generate PCA plot.
  generate_PCA = function(e, f, p, QC.index, batch, method){
    pca = prcomp(t(e), center = T, scale. = T)
    variance = pca$sdev^2/sum(pca$sdev^2)
    pca.data = data.frame(pca$x,batch = batch[1,],order = 1:nrow(pca$x))
    batch.QC = batch[1,];
    batch.QC[QC.index] = "QC"
    qc = rep(F, nrow(p))
    qc[QC.index] = TRUE
    ggplot(pca.data, aes(PC1, PC2, color = batch.QC,size = qc, order = order)) +
      geom_point(alpha = 3/4) +
      stat_ellipse( linetype = 2, size = 0.5) +
      labs(x = paste0("PC1: ",signif(variance[1]*100,3),"%"), y = paste0("PC2: ",signif(variance[2]*100,3),"%"),
           title = method)+
      theme.scatter
  }

  SERRFpca = generate_PCA(norm$e,f,p,QC.index, batch , "SERRF")
  rawpca = generate_PCA(e,f,p,QC.index, batch , "raw")


  # save results.
  rownames(norm$e) = f$label
  write.csv(norm$e, "SERRF_normalized.csv")
  write.csv(data.frame(label=f$label,rawValidateRSD = raw.validate, SERRFValidateRSD = SERRF.validate), 'performance -  validateRSD.csv')
  ggsave("SERRFpca.png",SERRFpca, width = 8, height = 8)
  ggsave("rawpca.png",rawpca, width = 8, height = 8)

  library(ReporteRs)
  doc = pptx( )
  doc = addSlide(doc, slide.layout = "Title and Content")
  doc = addPlot(doc, fun = function() print(rawpca),
                vector.graphic = TRUE, width = 6, height = 6)
  doc = addSlide(doc, slide.layout = "Title and Content")
  doc = addPlot(doc, fun = function() print(SERRFpca),
                vector.graphic = TRUE, width = 6, height = 6)

  # write the document to a file
  writeDoc(doc, file = "PCAs.pptx")

  zip(files = c(
    "SERRF_normalized.csv",
    'performance -  validateRSD.csv',
    "SERRFpca.png",
    "rawpca.png",
    "PCAs.pptx"
  ), zipfile = "SERRF - results.zip")
  end = Sys.time()







  # get ip summary
  ip_summ = t(info$num)
  colnames(ip_summ) = info$code

  ip_summ = data.frame(ip_summ)

  ip_summ = jsonlite::toJSON(ip_summ)





  return(list(validateSERRF = signif(median(SERRF.validate),3)*100, validateraw = signif(median(raw.validate),3)*100, count_reduced = sum(SERRF.validate<raw.validate, na.rm = T), perc_reduced = sum(SERRF.validate<raw.validate, na.rm = T)/nrow(f), count_less_20_raw = sum(raw.validate<.2, na.rm = T), count_less_20_SERRF = sum(SERRF.validate<.2, na.rm = T), perc_less_20_raw = signif(sum(raw.validate<.2, na.rm = T)/nrow(f),3) * 100, perc_less_20_SERRF = signif(sum(SERRF.validate<.2, na.rm = T)/nrow(f),3)*100, runtime = signif(as.numeric(end - start)/60,3),ip_summ=ip_summ))

}
