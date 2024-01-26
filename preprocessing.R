Batch_1_pp = readRDS(paste0(rds_data_dir,"/Batch_1_pp.rds"))
Batch_2_pp = readRDS(paste0(rds_data_dir,"/Batch_2_pp.rds"))
All = rbind(Batch_1_pp, Batch_2_pp)

Pures = All[All$pno_purity %in% c(0,100),]
Pures = Pures[Pures$all_br %in% c("01", "10", "50", "05"),]

Knowns = Pures[Pures$spec_batch == "20200831",]
Temp = Pures[Pures$spec_batch != "20200831",]
Temp$all_lab_spec = paste0(Temp$all_label, "_", Temp$spec_batch)
ind = ks_sample(Temp, "all_lab_spec", 4)

ts5 = Pures[Pures$spec_batch != "20200831",][!ind,]
transfer = Pures[Pures$spec_batch != "20200831",][ind,]

NonPures = All[!(All$pno_purity %in% c(0,100)),]
NonPures = NonPures[NonPures$all_br %in% c("11", "15", "51", "55"),]
ts1 = NonPures[NonPures$all_br == "11", ]
ts2 = NonPures[NonPures$all_br == "15", ]
ts3 = NonPures[NonPures$all_br == "51", ]
ts4 = NonPures[NonPures$all_br == "55", ]


saveRDS(Knowns, paste0(rds_data_dir, "/Knowns.rds"))
saveRDS(transfer, paste0(rds_data_dir, "/transfer.rds"))
saveRDS(ts1, paste0(rds_data_dir, "/ts1.rds"))
saveRDS(ts2, paste0(rds_data_dir, "/ts2.rds"))
saveRDS(ts3, paste0(rds_data_dir, "/ts3.rds"))
saveRDS(ts4, paste0(rds_data_dir, "/ts4.rds"))
saveRDS(ts5, paste0(rds_data_dir, "/ts5.rds"))


ts = list(Knowns, transfer, ts1, ts2, ts3, ts4, ts5)
ts_n = list("Knowns", "transfer", "ts1", "ts2", "ts3", "ts4", "ts5")
for (i in 1:7){
  df = ts[[i]]
  dfn = ts_n[[i]]
  write.csv(df$NIR, paste0("C:/Users/alagl/OneDrive - Wilmar International Limited/Desktop/IPP Research/Projects/MM_Gen/Data/Py/", dfn, ".csv"))
  write.csv(df[,2:14], paste0("C:/Users/alagl/OneDrive - Wilmar International Limited/Desktop/IPP Research/Projects/MM_Gen/Data/Py/", dfn, "_meta.csv"))
}



#3

# NP_JW = NonPures[NonPures$blender_batch == "JW",] #Only 51, 55 BR. All
# ts2 = NP_JW
# 
# NP_Jo = NonPures[NonPures$blender_batch == "Jo",] #Only 11, 15 BR. Only 90.5 to 99.5
# ts3 = NP_Jo
# 
# NP_JE = NonPures[NonPures$blender_batch == "JE",] #Only 11, 15 BR. Only 90.5 to 99.5
# ts4 = NP_JE