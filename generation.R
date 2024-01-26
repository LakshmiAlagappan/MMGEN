mmgen = function(ref_list, target_profile, nc=2, repn=6){
  #ref_list : list with the df of the known
  #target profile: plist
  if (length(ref_list) == 1){
    l = nrow(ref_list[[1]]$NIR)
    gen_spec = ref_list[[1]]$NIR[sample(1:l, repn, TRUE), , drop=FALSE]
    #gen_spec = gen_spec + matrix(runif(1751*repn, 0,1)/1000, nrow = repn,ncol = 1751)
    gen_spec[is.na(gen_spec)] = 0
    gen = list("NIR" = gen_spec, "pp" = rep(list(target_profile), repn))
    return (gen)
  }
  else{
    purity_mat = matrix(nrow = nc, ncol = nc,0)
    target_mat = matrix(nrow = nc, ncol = 1, target_profile)
    for (i in 1:length(ref_list)){
      purity_mat[,i] = ref_list[[i]]$pp[[1]]
    }
    ratio_mix_mat = solve(purity_mat, target_mat)
    gen_spec = matrix(nrow = repn, ncol = ncol(ref_list[[1]]$NIR), 0)
    for (i in 1:nc){
      l = nrow(ref_list[[i]]$NIR)
      cont = ratio_mix_mat[i,1]*ref_list[[i]]$NIR[sample(1:l, repn, TRUE), , drop=FALSE]
      gen_spec = gen_spec + cont
    }
    #gen_spec = gen_spec + matrix(runif(1751*repn, 0,1)/1000, nrow = repn,ncol = 1751)
    gen_spec[is.na(gen_spec)] = 0
    gen = list("NIR" = gen_spec, "pp" = rep(list(target_profile), repn))
    return (gen)
  }
}

mmgen_shifted = function(ref_list, target_profile, nc=2, repn=6, batch){
  #ref_list : list with the df of the known
  #target profile: plist
  
  params = readRDS(paste0("C:/Users/alagl/OneDrive - Wilmar International Limited/Desktop/IPP Research/Projects/MM_Gen/ShiftParams/RDS/Purity/", batch, "_params.rds")) 
  
  
  params_lst = list(list(params$m100, params$a100), list(params$m0, params$a0))
  if (length(ref_list) == 1){
    if (ref_list[[1]]$pp[[1]][1] == 100){
      i = 1
    }
    else{
      i = 2
    }
    l = nrow(ref_list[[1]]$NIR)
    gen_spec = ref_list[[1]]$NIR[sample(1:l, repn, TRUE), , drop=FALSE]%*%as.matrix(params_lst[[i]][[1]])
    gen_spec = sweep(gen_spec, 2, as.numeric(t(params_lst[[i]][[2]])), "+")
    #gen_spec = gen_spec + matrix(runif(1751*repn, 0,1)/1000, nrow = repn,ncol = 1751)
    gen_spec[is.na(gen_spec)] = 0
    gen = list("NIR" = gen_spec, "pp" = rep(list(target_profile), repn))
    return (gen)
  }
  else{
    purity_mat = matrix(nrow = nc, ncol = nc,0)
    target_mat = matrix(nrow = nc, ncol = 1, target_profile)
    for (i in 1:length(ref_list)){
      purity_mat[,i] = ref_list[[i]]$pp[[1]]
    }
    ratio_mix_mat = solve(purity_mat, target_mat)
    gen_spec = matrix(nrow = repn, ncol = ncol(ref_list[[1]]$NIR), 0)
    for (i in 1:nc){
      l = nrow(ref_list[[i]]$NIR)
      cont = ref_list[[i]]$NIR[sample(1:l, repn, TRUE), , drop=FALSE]%*%as.matrix(params_lst[[i]][[1]])
      cont = sweep(cont, 2, as.numeric(t(params_lst[[i]][[2]])), "+")
      cont = ratio_mix_mat[i,1]*cont
      gen_spec = gen_spec + cont
    }
    #gen_spec = gen_spec + matrix(runif(1751*repn, 0,1)/1000, nrow = repn,ncol = 1751)
    gen_spec[is.na(gen_spec)] = 0
    gen = list("NIR" = gen_spec, "pp" = rep(list(target_profile), repn))
    return (gen)
  }
}

mmgen_shifted_br = function(ref_list, target_profile, nc=2, repn=6, batch, b1, b2){
  #ref_list : list with the df of the known
  #target profile: plist
  if (length(ref_list) == 1){
    if (b1 == 0){
      params2 = readRDS(paste0("C:/Users/alagl/OneDrive - Wilmar International Limited/Desktop/IPP Research/Projects/MM_Gen/ShiftParams/RDS/Brand/", batch, "_", b2, "_m0_a0.rds"))
      params_lst = list(list(params2$m0, params2$a0))
    }
    if (b2 == 0){
      params1 = readRDS(paste0("C:/Users/alagl/OneDrive - Wilmar International Limited/Desktop/IPP Research/Projects/MM_Gen/ShiftParams/RDS/Brand/", batch, "_", b1, "_m100_a100.rds"))
      params_lst = list(list(params1$m100, params1$a100))
    }
    l = nrow(ref_list[[1]]$NIR)
    gen_spec = ref_list[[1]]$NIR[sample(1:l, repn, TRUE), , drop=FALSE]%*%as.matrix(params_lst[[1]][[1]])
    gen_spec = sweep(gen_spec, 2, as.numeric(t(params_lst[[1]][[2]])), "+")
    #gen_spec = gen_spec + matrix(runif(1751*repn, -1,1)/100, nrow = repn,ncol = 1751)
    gen_spec[is.na(gen_spec)] = 0
    gen = list("NIR" = gen_spec, "pp" = rep(list(target_profile), repn))
    return (gen)
  }
  else{
    params1 = readRDS(paste0("C:/Users/alagl/OneDrive - Wilmar International Limited/Desktop/IPP Research/Projects/MM_Gen/ShiftParams/RDS/Brand/", batch, "_", b1, "_m100_a100.rds"))
    params2 = readRDS(paste0("C:/Users/alagl/OneDrive - Wilmar International Limited/Desktop/IPP Research/Projects/MM_Gen/ShiftParams/RDS/Brand/", batch, "_", b2, "_m0_a0.rds"))
    params_lst = list(list(params1$m100, params1$a100), list(params2$m0, params2$a0))
    purity_mat = matrix(nrow = nc, ncol = nc,0)
    target_mat = matrix(nrow = nc, ncol = 1, target_profile)
    for (i in 1:length(ref_list)){
      purity_mat[,i] = ref_list[[i]]$pp[[1]]
    }
    ratio_mix_mat = solve(purity_mat, target_mat)
    gen_spec = matrix(nrow = repn, ncol = ncol(ref_list[[1]]$NIR), 0)
    for (i in 1:nc){
      l = nrow(ref_list[[i]]$NIR)
      cont = ref_list[[i]]$NIR[sample(1:l, repn, TRUE), , drop=FALSE]%*%as.matrix(params_lst[[i]][[1]])
      cont = sweep(cont, 2, as.numeric(t(params_lst[[i]][[2]])), "+")
      cont = ratio_mix_mat[i,1]*cont
      gen_spec = gen_spec + cont
    }
    #gen_spec = gen_spec + matrix(runif(1751*repn, -1,1)/100, nrow = repn,ncol = 1751)
    gen_spec[is.na(gen_spec)] = 0
    gen = list("NIR" = gen_spec, "pp" = rep(list(target_profile), repn))
    return (gen)
  }
}

format_pp = function(df){
  un = unique(df$pno_purity)
  df_list = list()
  for (i in 1:length(un)){
    u = un[[i]]
    df_list[[i]] = list("NIR" = I(df[df$pno_purity==u,]$NIR),
                        "pp" = rep(list(c(u,100-u)), length(df[df$pno_purity==u,]$pno_purity)))
  }
  names(df_list) = as.character(un)
  return(df_list)
}

reformat_pp = function(lst, batch_name, blen){
  df = data.frame("NIR" = I(do.call(rbind, lapply(lst, function(x){x$NIR}))))
  repn = dim(df$NIR)[1]/length(lst)
  df$sid = rep("NA", repn)
  df$type = rep("NA", repn)
  df$spec_batch = rep(batch_name, repn)
  df$pno_br = rep("NA", repn)
  df$mzo_br = rep("NA", repn)
  df$pno_purity = rep(unlist(lapply(lst, function(x){x$pp[[1]][[1]][1]})), each = 6)
  df$prep_batch = rep("NA", repn)
  df$blender_batch = rep(blen, repn)
  df$sid_type = rep("NA", repn)
  df$rep_batch = rep("NA", repn)
  df$all_batch = rep("Gen", repn)
  df$all_br = rep("NA", repn)
  df$all_label = df$pno_purity
  return(df)
}

calc_gen_metrics = function(gen_each, real, cl="pno_purity"){
  cond = "Basic"
  mreal = apply(real$NIR,2,median)
  gen = apply(gen_each$NIR,2,median)
  can = calc_cms(gen, mreal, "canberra")
  pop = build_pop_classification_model(real, "pno_purity")
  dp = calc_depth_score(can, pop$pop_list[[1]])
  lst = data.frame("Mode"=cond, "Label" = real$all_label[1], "Batch" = real$spec_batch[1],
  "can_cms" = round(can,2), "can_dep" = round(dp,2))
  return(lst)
}

eval_gen = function(df, kns){
  df$reff = paste0(df$spec_batch, "_", df$all_label)
  gen_all = list()
  metric_all = list()
  for (i in 1:nrow(df)){
    kn = data.frame()
    tst_here = df[i,]
    tst_set = df[df$reff == tst_here$reff, ]
    target_profile = c(tst_here$pno_purity, 100-tst_here$pno_purity)
    if (tst_here$pno_br != 0){
      kn = rbind(kn,kns[kns$pno_br == tst_here$pno_br,])
    }
    if (tst_here$mzo_br != 0){
      kn = rbind(kn,kns[kns$mzo_br == tst_here$mzo_br,])
    }
    gen = reformat_pp(list(mmgen(format_pp(kn), target_profile, 2,6)), tst_here$spec_batch,
                      tst_here$blender_batch)
    gen_all[[i]] = gen
    #gen_all[[i]] = gen[1,]
    #gen_all[[i]]$NIR = data.frame(t(apply(gen$NIR,2,median)))
    metric_all[[i]] = calc_gen_metrics(gen, tst_set, paste0(i))
  }
  gendf = do.call(rbind, gen_all)
  rownames(gendf) = paste0("MMGEN_", 1:nrow(gendf))
  metdf = do.call(rbind, metric_all)
  return (list(gendf, metdf))
}

eval_shgen = function(df, kns){
  df$reff = paste0(df$spec_batch, "_", df$all_label)
  gen_all = list()
  metric_all = list()
  for (i in 1:nrow(df)){
    kn = data.frame()
    tst_here = df[i,]
    tst_set = df[df$reff == tst_here$reff, ]
    target_profile = c(tst_here$pno_purity, 100-tst_here$pno_purity)
    if (tst_here$pno_br != 0){
      kn = rbind(kn,kns[kns$pno_br == tst_here$pno_br,])
    }
    if (tst_here$mzo_br != 0){
      kn = rbind(kn,kns[kns$mzo_br == tst_here$mzo_br,])
    }
    if (tst_here$spec_batch == kns$spec_batch[1]){
      gen = reformat_pp(list(mmgen(format_pp(kn), target_profile, 2,6)), tst_here$spec_batch,
                        tst_here$blender_batch)
    }
    else{
      gen = reformat_pp(list(mmgen_shifted(format_pp(kn), target_profile, 2,6, tst_here$spec_batch)), tst_here$spec_batch, tst_here$blender_batch)
    }
    gen_all[[i]] = gen
    #gen_all[[i]] = gen[1,]
    #gen_all[[i]]$NIR = data.frame(t(apply(gen$NIR,2,median)))
    metric_all[[i]] = calc_gen_metrics(gen, tst_set, paste0(i))
  }
  gendf = do.call(rbind, gen_all)
  rownames(gendf) = paste0("MMGENSH1_", 1:nrow(gendf))
  metdf = do.call(rbind, metric_all)
  return (list(gendf, metdf))
}

eval_shgenbr = function(df, kns){
  df$reff = paste0(df$spec_batch, "_", df$all_label)
  gen_all = list()
  metric_all = list()
  for (i in 1:nrow(df)){
    kn = data.frame()
    tst_here = df[i,]
    tst_set = df[df$reff == tst_here$reff, ]
    target_profile = c(tst_here$pno_purity, 100-tst_here$pno_purity)
    if (tst_here$pno_br != 0){
      kn = rbind(kn,kns[kns$pno_br == tst_here$pno_br,])
    }
    if (tst_here$mzo_br != 0){
      kn = rbind(kn,kns[kns$mzo_br == tst_here$mzo_br,])
    }
    if (tst_here$spec_batch == kns$spec_batch[1]){
      gen = reformat_pp(list(mmgen(format_pp(kn), target_profile, 2,6)), tst_here$spec_batch,
                        tst_here$blender_batch)
    }
    else{
      gen = reformat_pp(list(mmgen_shifted_br(format_pp(kn), target_profile, 2,6,
                                              tst_here$spec_batch,tst_here$pno_br,
                                              tst_here$mzo_br)), tst_here$spec_batch,
                        tst_here$blender_batch)
    }
    gen_all[[i]] = gen
    #gen_all[[i]] = gen[1,]
    #gen_all[[i]]$NIR = data.frame(t(apply(gen$NIR,2,median)))
    metric_all[[i]] = calc_gen_metrics(gen, tst_set, paste0(i))
  }
  gendf = do.call(rbind, gen_all)
  rownames(gendf) = paste0("MMGENSH2_", 1:nrow(gendf))
  metdf = do.call(rbind, metric_all)
  return (list(gendf, metdf))
}

plot_stackbar = function(df, pal = palette("default"), 
                         ll=c("Simple Plot", "Population Size", "% of Spectra within RA", "RA")) {
  col_df = colnames(df)
  col_df[1] = "x"
  col_df[2] = "y"
  colnames(df) = col_df
  df_m = reshape2::melt(df, id.vars = c("x", "y"))
  df_m$value = as.numeric(df_m$value)
  val = round(df_m$value,2)
  val[val<=0.03] = NA
  p = ggplot2::ggplot(df_m, ggplot2::aes(x = factor(variable), y = value, 
                                         fill = factor(y, levels = c(50,25,10,5,3,2,1)))) + 
    ggplot2::scale_fill_manual(values = pal)+
    ggplot2::geom_bar(stat = "identity", position = "stack", color = "black") +
    ggplot2:: geom_text(ggplot2::aes(label = val), 
                        position = ggplot2::position_stack(vjust = 0.5))+
    ggplot2::facet_grid(~x)+
    theme_Publication(base_size = 16) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20,  vjust = 0.6))+
    ggplot2::labs(title = ll[1],
                  x = ll[2],
                  y = ll[3],
                  fill = ll[4])
  return(p)
}

plot_stackbar = function(df, pal = palette("default"), 
                         ll=c("Simple Plot", "Testing variable", "Ratio of spectra within interval", "RA interval")) {
  ran = c("[0,1]", "(1,2]", "(2,3]", "(3,5]", "(5,10]", "(10,INF]")
  col_df = colnames(df)
  col_df[1] = "ra"
  col_df[2] = "tv"
  colnames(df) = col_df
  df_m = reshape2::melt(df, id.vars = c("ra", "tv"))
  df_m$value = as.numeric(df_m$value)*100
  #df_m$tv = as.numeric(df_m$tv)
  labb = round(df_m$value,1)
  labb[labb<4] = NA
  p = ggplot2::ggplot(df_m, ggplot2::aes(x = factor(tv), y = value, fill = factor(ra, levels = rev(ran)))) + 
    ggplot2::scale_fill_manual(values = pal)+
    ggplot2::geom_bar(stat = "identity", position = "stack", color = "black") +
    ggplot2:: geom_text(ggplot2::aes(label = labb), 
                        position = ggplot2::position_stack(vjust = 0.5))+
    ggplot2::facet_grid(~variable)+
    theme_Publication(base_size = 18) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, vjust = 0.9, hjust = 1))+
    ggplot2::labs(title = ll[1],
                  x = ll[2],
                  y = ll[3],
                  fill = ll[4])
  return(p)
}

value_bins = function(cms_list, bin1, bin2){
  df_val = cms_list>bin1 & cms_list<=bin2
  ans = signif(sum(df_val)/length(df_val),3)
  return (ans)
}

eval_metric_quant = function(ts, gen, genmet,scn, gn){
  if (length(unique(ts$pno_purity)) == 1){
    ca = paste0(length(gen$pno_purity), "/",length(gen$pno_purity) )
    rmsep = 0
  }
  else{
    ca = build_test_pclda(ts, gen, "pno_purity", 2)$formatted
    rmsep = test_pls_model(build_pls_model(ts$NIR, ts$pno_purity, 20, FALSE),
                           gen$NIR, gen$pno_purity, FALSE)$rmsep
  }
  
  cm1 = rbind(
    data.frame("TS" = scn, "GM" = gn, "Bin" = "[0,1]",
               "#CMS" = value_bins(genmet$can_cms, 0,1),
               "#Depth"= value_bins(genmet$can_dep, 0,1)),
    data.frame("TS" = scn, "GM" = gn, "Bin" = "(1,2]",
               "#CMS" =  value_bins(genmet$can_cms, 1,2), 
               "#Depth"= value_bins(genmet$can_dep, 1,2)),
    data.frame("TS" = scn, "GM" = gn, "Bin" = "(2,3]",
               "#CMS" =  value_bins(genmet$can_cms, 2,3),
               "#Depth"= value_bins(genmet$can_dep, 2,3)),
    data.frame("TS" = scn, "GM" = gn, "Bin" = "(3,5]",
               "#CMS" =  value_bins(genmet$can_cms, 3,5), 
               "#Depth"= value_bins(genmet$can_dep, 3,5)),
    data.frame("TS" = scn, "GM" = gn, "Bin" = "(5,10]",
               "#CMS" =  value_bins(genmet$can_cms,5, 10),
               "#Depth"=  value_bins(genmet$can_dep, 5,10)),
    data.frame("TS" = scn, "GM" = gn, "Bin" = "(10, INF]",
               "#CMS" =  value_bins(genmet$can_cms, 10,100), 
               "#Depth"=  value_bins(genmet$can_dep, 10,100)))
  return(list(ca, rmsep, cm1))
}

format_pp_3c = function(df){
  un = unique(df$ps)
  df_list = list()
  for (i in 1:length(un)){
    u = un[[i]]
    df_list[[i]] = list("NIR" = I(df[df$ps==u,]$NIR),
                        "pp" = rep(list(df[df$ps==u,]$plist[[1]]), length(df[df$ps==u,]$ps)))
  }
  names(df_list) = as.character(un)
  return(df_list)
}

reformat_pp_3c = function(lst){
  df = data.frame("NIR" = I(do.call(rbind, lapply(lst, function(x){x$NIR}))))
  repn = dim(df$NIR)[1]/length(lst)
  df$date = rep("Gen", repn)
  df$code = rep("NA", repn)
  df$C1 = rep("NA", repn)
  df$C2 = rep("NA", repn)
  df$C3 = rep("NA", repn)
  df$P1 = rep(unlist(lapply(lst, function(x){x$pp[[1]][1]})), each = repn)
  df$P2 = rep(unlist(lapply(lst, function(x){x$pp[[1]][2]})), each = repn)
  df$P3 = rep(unlist(lapply(lst, function(x){x$pp[[1]][3]})), each = repn)
  df$clist = rep("NA", repn)
  df$plist = rep(lapply(lst, function(x){x$pp[[1]]}), each = repn)
  df$cs = rep("NA", repn)
  df$ps = paste0(df$P1, ":", df$P2, ":", df$P3)
  return (df)
}

calc_gen_metrics_3c = function(gen_each, real, cl="ps"){
  cond = "Basic"
  cl = "ps"
  mreal = apply(real$NIR,2,median)
  gen = apply(gen_each$NIR,2,median)
  can = calc_cms(gen, mreal, "canberra")
  pop = build_pop_classification_model(gen_each, cl)
  dp = calc_depth_score(can, pop$pop_list[[1]])
  lst = data.frame("Mode"=cond, "Label" = real$ps[[1]], "can_cms" = round(can,2), "can_dep" = round(dp,2))
  return(lst)
}

eval_gen_3c = function(df, kns){
  gen_all = list()
  metric_all = list()
  for (i in 1:nrow(df)){
    tst_here = df[i,]
    target_profile = tst_here$plist[[1]]
    gen_here = reformat_pp_3c(list(mmgen(format_pp_3c(kns), target_profile,3,6)))
    gen_all[[i]] = gen_here
    #gen_all[[i]] = gen[1,]
    #gen_all[[i]]$NIR = data.frame(t(apply(gen$NIR,2,median)))
    metric_all[[i]] = calc_gen_metrics_3c(gen_here, tst_here, paste0(i))
  }
  gendf = do.call(rbind, gen_all)
  rownames(gendf) = paste0("MMGEN_", 1:nrow(gendf))
  metdf = do.call(rbind, metric_all)
  return (list(gendf, metdf))
}