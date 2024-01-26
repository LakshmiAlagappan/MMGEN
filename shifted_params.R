make_shift_params_purity = function(Knowns, Knowns_Tr){
  pri0 = Knowns[Knowns$pno_purity == 0,]
  pri100 = Knowns[Knowns$pno_purity == 100,]
  sec0 = Knowns_Tr[Knowns_Tr$pno_purity == 0,]
  sec100 = Knowns_Tr[Knowns_Tr$pno_purity == 100,]
  
  p0ind = ks_sample(pri0, "pno_purity", min(nrow(pri0), nrow(sec0)))
  s0ind = ks_sample(sec0, "pno_purity", min(nrow(pri0), nrow(sec0)))
  p100ind = ks_sample(pri100, "pno_purity", min(nrow(pri100), nrow(sec100)))
  s100ind = ks_sample(sec100, "pno_purity", min(nrow(pri100), nrow(sec100)))
  
  mod0 = build_pds_model(sec0[s0ind,], pri0[p0ind,], 1, 1, "pno_purity")
  m0 = as.matrix(mod0$model$tm$Fmat)
  a0 = mod0$model$tm$Bvec
  
  mod100 = build_pds_model(sec100[s100ind,], pri100[p100ind,], 1, 1, "pno_purity")
  m100 = as.matrix(mod100$model$tm$Fmat)
  a100 = mod100$model$tm$Bvec
  shift_params = list(batch = Knowns_Tr$spec_batch[1], m0 = m0, a0 = a0, m100 = m100, a100 = a100)
  return(shift_params)
}

make_shift_params_brand_pno = function(Knowns, Knowns_Tr,b1){
  pri100 = Knowns[Knowns$pno_purity == 100,]
  pri100 = pri100[pri100$pno_br == b1, ]
  sec100 = Knowns_Tr[Knowns_Tr$pno_purity == 100,]
  sec100 = sec100[sec100$pno_br == b1, ]
  p100ind = ks_sample(pri100, "all_label", min(nrow(pri100), nrow(sec100)))
  s100ind = ks_sample(sec100, "all_label", min(nrow(pri100), nrow(sec100)))

  mod100 = build_pds_model(sec100[s100ind,], pri100[p100ind,], 1, 1, "all_label")
  m100 = as.matrix(mod100$model$tm$Fmat)
  a100 = mod100$model$tm$Bvec
  shift_params = list(batch = Knowns_Tr$spec_batch[1], m100 = m100, a100 = a100)
  return(shift_params)
}

make_shift_params_brand_mzo = function(Knowns, Knowns_Tr,b2){
  pri0 = Knowns[Knowns$pno_purity == 0,]
  pri0 = pri0[pri0$mzo_br == b2, ]
  sec0 = Knowns_Tr[Knowns_Tr$pno_purity == 0,]
  sec0 = sec0[sec0$mzo_br == b2, ]
  
  p0ind = ks_sample(pri0, "all_label", min(nrow(pri0), nrow(sec0)))
  s0ind = ks_sample(sec0, "all_label", min(nrow(pri0), nrow(sec0)))
  
  mod0 = build_pds_model(sec0[s0ind,], pri0[p0ind,], 1, 1, "all_label")
  m0 = as.matrix(mod0$model$tm$Fmat)
  a0 = mod0$model$tm$Bvec
  
  shift_params = list(batch = Knowns_Tr$spec_batch[1], m0 = m0, a0 = a0)
  return(shift_params)
}

bats = unique(transfer$spec_batch)
datadir =  paste0("C:/Users/alagl/OneDrive - Wilmar International Limited/Desktop/IPP Research/Projects/MM_Gen/ShiftParams/RDS/Purity/")
for (i in 1:25){
  tr_here = transfer[transfer$spec_batch == bats[i],]
  param = make_shift_params_purity(Knowns, tr_here)
  fname = paste0(datadir, param$batch[1], "_params.rds")
  saveRDS(param, fname)
}

brs = c(1,5)
bats = unique(transfer$spec_batch)
datadir = paste0("C:/Users/alagl/OneDrive - Wilmar International Limited/Desktop/IPP Research/Projects/MM_Gen/ShiftParams/RDS/Brand/")
for (i in 1:25){
  tr_here = transfer[transfer$spec_batch == bats[i],]
  for (j in 1:2){
    param_pno = make_shift_params_brand_pno(Knowns, tr_here,brs[[j]])
    param_mzo = make_shift_params_brand_mzo(Knowns, tr_here,brs[[j]])
    fname1 = paste0(datadir, param_pno$batch, "_", brs[[j]], "_m100_a100.rds")
    fname2 = paste0(datadir, param_mzo$batch, "_", brs[[j]], "_m0_a0.rds")
    saveRDS(param_pno, fname1)
    saveRDS(param_mzo, fname2)
  }
}

bats = unique(transfer$spec_batch)
datadir = paste0("C:/Users/alagl/OneDrive - Wilmar International Limited/Desktop/IPP Research/Projects/MM_Gen/ShiftParams/Py/Purity/")
for (i in 1:25){
  tr_here = transfer[transfer$spec_batch == bats[i],]
  param = make_shift_params_purity(Knowns, tr_here)
  dfn1 = paste0(datadir, param$batch[1], "_m0.csv")
  write.csv(param$m0, dfn1)
  dfn2 = paste0(datadir, param$batch[1], "_a0.csv")
  write.csv(param$a0, dfn2)
  dfn3 = paste0(datadir, param$batch[1], "_m100.csv")
  write.csv(param$m100, dfn3)
  dfn4 = paste0(datadir, param$batch[1], "_a100.csv")
  write.csv(param$a100, dfn4)
}

brs = c(1,5)
bats = unique(transfer$spec_batch)
datadir = paste0("C:/Users/alagl/OneDrive - Wilmar International Limited/Desktop/IPP Research/Projects/MM_Gen/ShiftParams/Py/Brand/")
for (i in 1:25){
  tr_here = transfer[transfer$spec_batch == bats[i],]
  for (j in 1:2){
    param_pno = make_shift_params_brand_pno(Knowns, tr_here,brs[[j]])
    param_mzo = make_shift_params_brand_mzo(Knowns, tr_here,brs[[j]])
    
    dfn1 = paste0(datadir, param_mzo$batch[1], "_", brs[[j]],  "_m0.csv")
    write.csv(param_mzo$m0, dfn1)
    dfn2 = paste0(datadir, param_mzo$batch[1], "_", brs[[j]],  "_a0.csv")
    write.csv(param_mzo$a0, dfn2)
    dfn3 = paste0(datadir, param_pno$batch[1], "_", brs[[j]],  "_m100.csv")
    write.csv(param_pno$m100, dfn3)
    dfn4 = paste0(datadir, param_pno$batch[1], "_", brs[[j]], "_a100.csv")
    write.csv(param_pno$a100, dfn4)
  }
}

