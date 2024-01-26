# Set the working directory to be the location where the project folder is saved
wdir <- paste0("C:/Users/alagl/OneDrive - Wilmar International Limited/Desktop/IPP Research/Projects/MM_Gen")
setwd(wdir)
# Setting the directory for the plots generated to be saved
plot_counter = 1
plot_results_dir = paste0(wdir,"/Plot_Results")
# Directory where data for the project is stored
rds_data_dir = paste0(wdir,"/Data")
# directory where the model results will be stored
model_results_dir = paste0(wdir, "/Models_Results")

dir.create(plot_results_dir)
dir.create(model_results_dir)

source("C:/Users/alagl/OneDrive - Wilmar International Limited/Desktop/IPP Research/Projects/helpers.R")

source("C:/Users/alagl/OneDrive - Wilmar International Limited/Desktop/IPP Research/Projects/colors.R")

source(paste0(wdir, "/generation.R"))

Batch_1_pp = readRDS(paste0(rds_data_dir,"/Batch_1_pp.rds"))
Batch_2_pp = readRDS(paste0(rds_data_dir,"/Batch_2_pp.rds"))
All = rbind(Batch_1_pp, Batch_2_pp)
Knowns = readRDS(paste0(rds_data_dir, "/Knowns.rds"))
transfer = readRDS(paste0(rds_data_dir, "/transfer.rds"))
testset1 = All[All$all_br == "11", ]
testset2 = All[All$all_br == "15", ]
testset3 = All[All$all_br == "51", ]
testset4 = All[All$all_br == "55", ]

###### Eval MMGEN and MMGEN (shift) ######
ts1ans1 = eval_gen(testset1, Knowns)
ts2ans1 = eval_gen(testset2, Knowns)
ts3ans1 = eval_gen(testset3, Knowns)
ts4ans1 = eval_gen(testset4, Knowns)
saveRDS(ts1ans1, paste0(model_results_dir,"/ts1ans1.rds"))
saveRDS(ts2ans1, paste0(model_results_dir,"/ts2ans1.rds"))
saveRDS(ts3ans1, paste0(model_results_dir,"/ts3ans1.rds"))
saveRDS(ts4ans1, paste0(model_results_dir,"/ts4ans1.rds"))

ts1ans2 = eval_shgenbr(testset1, Knowns)
ts2ans2 = eval_shgenbr(testset2, Knowns)
ts3ans2 = eval_shgenbr(testset3, Knowns)
ts4ans2 = eval_shgenbr(testset4, Knowns)
saveRDS(ts1ans2, paste0(model_results_dir,"/ts1ans2.rds"))
saveRDS(ts2ans2, paste0(model_results_dir,"/ts2ans2.rds"))
saveRDS(ts3ans2, paste0(model_results_dir,"/ts3ans2.rds"))
saveRDS(ts4ans2, paste0(model_results_dir,"/ts4ans2.rds"))

ts1evsum1 = eval_metric_quant(testset1, ts1ans1[[1]], ts1ans1[[2]], "1", "Normal")
ts2evsum1 = eval_metric_quant(testset2, ts2ans1[[1]], ts2ans1[[2]], "2", "Normal")
ts3evsum1 = eval_metric_quant(testset3, ts3ans1[[1]], ts3ans1[[2]], "3", "Normal")
ts4evsum1 = eval_metric_quant(testset4, ts4ans1[[1]], ts4ans1[[2]], "4", "Normal")
saveRDS(ts1evsum1, paste0(model_results_dir,"/ts1evsum1.rds"))
saveRDS(ts2evsum1, paste0(model_results_dir,"/ts2evsum1.rds"))
saveRDS(ts3evsum1, paste0(model_results_dir,"/ts3evsum1.rds"))
saveRDS(ts4evsum1, paste0(model_results_dir,"/ts4evsum1.rds"))

ts1evsum2 = eval_metric_quant(testset1, ts1ans2[[1]], ts1ans2[[2]], "1", "Shifted")
ts2evsum2 = eval_metric_quant(testset2, ts2ans2[[1]], ts2ans2[[2]], "2", "Shifted")
ts3evsum2 = eval_metric_quant(testset3, ts3ans2[[1]], ts3ans2[[2]], "3", "Shifted")
ts4evsum2 = eval_metric_quant(testset4, ts4ans2[[1]], ts4ans2[[2]], "4", "Shifted")
saveRDS(ts1evsum2, paste0(model_results_dir,"/ts1evsum2.rds"))
saveRDS(ts2evsum2, paste0(model_results_dir,"/ts2evsum2.rds"))
saveRDS(ts3evsum2, paste0(model_results_dir,"/ts3evsum2.rds"))
saveRDS(ts4evsum2, paste0(model_results_dir,"/ts4evsum2.rds"))


#illus 1#
pb = Knowns
pb = rbind(pb[pb$pno_br == "1",], pb[pb$mzo_br == "5",])
sb = transfer[transfer$spec_batch == "20201124",]
sb = rbind(sb[sb$pno_br == "1",], sb[sb$mzo_br == "5",])
ts_here = All[All$all_br == "15", ]
ts_here = ts_here[ts_here$spec_batch == "20201124",]
ts_here = ts_here[ts_here$pno_purity %in% c(86, 60),]
gen1 = eval_gen(ts_here, Knowns)
gen2 = eval_shgenbr(ts_here, Knowns)

pb$spec_batch = "Real known"
sb$spec_batch = "Real transfer"
ts_here$spec_batch = "Real target"
gen1[[1]]$spec_batch = "Gen target - MMGEN"
gen2[[1]]$spec_batch = "Gen target - MMGEN (shift)"

pb$all_label = paste0("{", pb$pno_br, ",", pb$mzo_br, ",", pb$pno_purity, ":", 100-pb$pno_purity, "}")
sb$all_label = paste0("{", sb$pno_br, ",", sb$mzo_br, ",", sb$pno_purity, ":", 100-sb$pno_purity, "}")
ts_here$all_label = paste0("{", ts_here$pno_br, ",", ts_here$mzo_br, ",", 
                           ts_here$pno_purity, ":", 100-ts_here$pno_purity, "}")
gen1[[1]]$all_label = paste0("{1,5,", gen1[[1]]$pno_purity, ":", 100-gen1[[1]]$pno_purity, "}")
gen2[[1]]$all_label = paste0("{1,5,", gen2[[1]]$pno_purity, ":", 100-gen2[[1]]$pno_purity, "}")

df = rbind(pb,sb,ts_here, gen1[[1]], gen2[[1]])
dfpca = prcomp(df$NIR, TRUE)

saveRDS(df, paste0(model_results_dir,"/illus_1_df.rds"))
saveRDS(dfpca, paste0(model_results_dir,"/illus_1_dfpca.rds"))
saveRDS(list(gen1[[2]], gen2[[2]]), paste0(model_results_dir,"/illus_1_metrics.rds"))

#illus2#
pb = Knowns
pb = rbind(pb[pb$pno_br == "5",], pb[pb$mzo_br == "1",])
All$sid = as.factor(All$spec_batch)
levels(All$sid) = paste0("D", seq(1:26))
ts_here = All[All$all_br == "51", ]
ts_here = rbind(ts_here[ts_here$pno_purity <85,],
                ts_here[ts_here$pno_purity ==90,])
ts_here = ts_here[ts_here$spec_batch!="20200831",]
ts_here_sb = ts_here$sid
sb = transfer[transfer$spec_batch %in% unique(ts_here$spec_batch),]
sb = rbind(sb[sb$pno_br == "5",], sb[sb$mzo_br == "1",])

gen1 = eval_gen(ts_here, pb)
gen2 = eval_shgenbr(ts_here, pb)

pb$spec_batch = "Real known"
sb$spec_batch = "Real transfer"
ts_here$spec_batch = "Real target"
gen1[[1]]$spec_batch = "Gen target - MMGEN"
gen2[[1]]$spec_batch = "Gen target - MMGEN (shift)"

pb$all_label = paste0("{", pb$pno_br, ",", pb$mzo_br, ",", pb$pno_purity, ":", 100-pb$pno_purity, "}")
ts_here$all_label = paste0("{", ts_here$pno_br, ",", ts_here$mzo_br, ",", 
                           ts_here$pno_purity, ":", 100-ts_here$pno_purity, "}")
gen1[[1]]$all_label = paste0("{5,1,", gen1[[1]]$pno_purity, ":", 100-gen1[[1]]$pno_purity, "}")
gen2[[1]]$all_label = paste0("{5,1,", gen2[[1]]$pno_purity, ":", 100-gen2[[1]]$pno_purity, "}")

df = rbind(pb,sb,ts_here, gen1[[1]], gen2[[1]])
df$sid = c(rep(NA, nrow(pb)), rep(NA, nrow(sb)), as.character(ts_here_sb), rep(NA, nrow(gen1[[1]])), rep(NA, nrow(gen2[[1]])))
dfpca = prcomp(df$NIR, TRUE)

saveRDS(df, paste0(model_results_dir,"/illus_2_df.rds"))
saveRDS(dfpca, paste0(model_results_dir,"/illus_2_dfpca.rds"))
saveRDS(list(gen1[[2]], gen2[[2]]), paste0(model_results_dir,"/illus_2_metrics.rds"))

############
rds_data_dir <- paste0(wdir, "/Data/3cblend")
Mixtures_pp = readRDS(paste0(rds_data_dir,"/Mixtures_pp.rds"))
Pures = readRDS(paste0(rds_data_dir, "/Pures.rds"))
Mix_2 = readRDS(paste0(rds_data_dir, "/Mix_2.rds"))
Mix_3 = readRDS(paste0(rds_data_dir, "/Mix_3.rds"))

## Illus1 ##
target = Mix_3[Mix_3$cs == "Hute (1):RSBO (1):RSFO (1)",]
target = target[target$date == "20180620",]
target = target[target$code == "50-2",]

knowns1 = rbind(Pures[Pures$C1 == "Hute (1)",],
          Pures[Pures$C2 == "RSBO (1)",],
          Pures[Pures$C3 == "RSFO (1)",])
gen3c1 = eval_gen_3c(target, knowns1)

m2 = Mix_2[Mix_2$date == "20180620",]
m2 = m2[m2$C1 == "Hute (1)",]
k1 = m2[m2$ps == "80:0:20",]
k2 = m2[m2$ps == "90:0:10",]
k2 = k2[k2$code == "49-1",]
k3 = m2[m2$ps == "90:10:0",]
knowns2 = rbind(k1,k2,k3)
gen3c2 = eval_gen_3c(target, knowns2)

target$date = "Real target D1"
knowns1$date = "Real knowns !D1"
knowns2$date = "Real knowns D1"
gen3c1[[1]]$date = "Gen target - MMGEN (!D1)"
gen3c2[[1]]$date = "Gen target - MMGEN (D1)"
df = rbind(target, knowns1, knowns2, gen3c1[[1]],gen3c2[[1]])
dfpca = prcomp(df$NIR, TRUE)
saveRDS(df, paste0(model_results_dir,"/gen3c_illus1_df.rds"))
saveRDS(dfpca, paste0(model_results_dir,"/gen3c_illus1_dfpca.rds"))
saveRDS(list(gen3c1[[2]],gen3c2[[2]]), paste0(model_results_dir,"/gen3c_illus1_metrics.rds"))


#### Illus 2 #####
target = Mix_3[Mix_3$cs == "Hute (2):RSBO (1):RSFO (1)",]
target = target[target$date == "20180620",]
target = target[target$code == "50-4",]

knowns1 = rbind(Pures[Pures$C1 == "Hute (2)",],
          Pures[Pures$C2 == "RSBO (1)",],
          Pures[Pures$C3 == "RSFO (1)",])
gen3c1 = eval_gen_3c(target, knowns1)

m2 = Mix_2[Mix_2$date == "20180620",]
m2 = m2[m2$C1 == "Hute (2)",]
k1 = m2[m2$ps == "80:0:20",]
k2 = m2[m2$ps == "90:0:10",]
k3 = m2[m2$ps == "90:10:0",]
k3 = k3[k3$code != "48-3",]
knowns2 = rbind(k1,k2,k3)
gen3c2 = eval_gen_3c(target, knowns2)

target$date = "Real target D2"
knowns1$date = "Real knowns !D2"
knowns2$date = "Real knowns D2"
gen3c1[[1]]$date = "Gen target - MMGEN (!D2)"
gen3c2[[1]]$date = "Gen target - MMGEN (D2)"
df = rbind(target, knowns1, knowns2, gen3c1[[1]],gen3c2[[1]])
dfpca = prcomp(df$NIR, TRUE)

###### Illus 3 ######

target = Mix_3[Mix_3$cs == "Hute (1):RSBO (1):RSFO (1)",]
target = target[target$date == "20180619",]
target = target[target$ps == "70:15:15",]
target = target[target$code == "24-3",]

knowns1 = rbind(Pures[Pures$C1 == "Hute (1)",],
          Pures[Pures$C2 == "RSBO (1)",],
          Pures[Pures$C3 == "RSFO (1)",])
gen3c1 = eval_gen_3c(target, knowns1)

k1 = Mix_3[Mix_3$cs == "Hute (1):RSBO (1):RSFO (1)",]
k1 = k1[k1$date == "20180619",]
k1 = k1[k1$ps == "90:5:5",]
m2 = Mix_2[Mix_2$date == "20180619",]
m2 = m2[m2$C1 == "Hute (1)",]
k2 = m2[m2$ps == "70:30:0",]
k2 = k2[k2$code != "22-3",]
k3 = m2[m2$ps == "70:0:30",]
k3 = k3[k3$code == "23-3",]
knowns2 = rbind(k1, k2, k3)
gen3c2 = eval_gen_3c(target, knowns2)

target$date = "Real target D2"
knowns1$date = "Real knowns !D2"
knowns2$date = "Real knowns D2"
gen3c1[[1]]$date = "Gen target - MMGEN (!D2)"
gen3c2[[1]]$date = "Gen target - MMGEN (D2)"
df = rbind(target, knowns1, knowns2, gen3c1[[1]],gen3c2[[1]])
dfpca = prcomp(df$NIR, TRUE)
saveRDS(df, paste0(model_results_dir,"/gen3c_illus2_df.rds"))
saveRDS(dfpca, paste0(model_results_dir,"/gen3c_illus2_dfpca.rds"))
saveRDS(list(gen3c1[[2]],gen3c2[[2]]), paste0(model_results_dir,"/gen3c_illus2_metrics.rds"))

