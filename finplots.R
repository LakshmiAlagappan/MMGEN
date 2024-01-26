###### Illustration 1 ######
df = readRDS(paste0(model_results_dir,"/illus_1_df.rds"))
dfpca = readRDS(paste0(model_results_dir,"/illus_1_dfpca.rds"))
gen12metric = readRDS(paste0(model_results_dir,"/illus_1_metrics.rds"))
p1 = plot_pca(dfpca, 1, 2, FALSE, c7, df$spec_batch, df$all_label, c("tit", "Spectra", "Blends")) +
  theme_Publication(base_size = 16) +
  ggplot2::theme(plot.title = ggplot2::element_blank())
plot_save(p1+ ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'white', color = 'white')),
          "Illus_1", 5,8)

median(gen12metric[[1]][1:6,4])
median(gen12metric[[2]][1:6,4])
median(gen12metric[[1]][7:12,4])
median(gen12metric[[2]][7:12,4])

median(gen12metric[[1]][1:6,5])
median(gen12metric[[2]][1:6,5])
median(gen12metric[[1]][7:12,5])
median(gen12metric[[2]][7:12,5])

###### Illustration 2 ######
df = readRDS(paste0(model_results_dir,"/illus_2_df.rds"))
dfpca = readRDS(paste0(model_results_dir,"/illus_2_dfpca.rds"))
gen12metric = readRDS(paste0(model_results_dir,"/illus_2_metrics.rds"))
df$all_label[df$all_label == "50_100"] = "{5,0,100:0}"
df$all_label[df$all_label == "01_0"] = "{0,1,0:100}"
p2 = plot_pca(dfpca, 1, 2, FALSE, c7, df$spec_batch, df$all_label, c("tit", "Spectra", "Blends")) +
  theme_Publication(base_size = 16) +
  ggplot2::theme(plot.title = ggplot2::element_blank())+
  directlabels::geom_dl(ggplot2::aes(label=df$sid),method="smart.grid")
plot_save(p2+ ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'white', color = 'white')),
          "Illus_2", 9,11)

gen12metric[[1]]$new = paste0(gen12metric[[1]]$Label, "_", ts_here$sid)
gen12metric[[2]]$new = paste0(gen12metric[[2]]$Label, "_", ts_here$sid)
labs = unique(gen12metric[[1]]$new)
ans = list()
for (i in 1:length(labs)){
  median_cms1 = round(median(gen12metric[[1]][gen12metric[[1]]$new == labs[i],4]),2)
  median_ds1 = round(median(gen12metric[[1]][gen12metric[[1]]$new == labs[i],5]),2)
  median_cms2 = round(median(gen12metric[[2]][gen12metric[[2]]$new == labs[i],4]),2)
  median_ds2 = round(median(gen12metric[[2]][gen12metric[[2]]$new == labs[i],5]),2)
  ans[[i]] = c(labs[i], median_cms1,  median_cms2, median_ds1, median_ds2)
}
ans_df = as.data.frame(do.call(rbind, ans))
ans_df[order(ans_df$V1),]


###### Summary ######
ts1evsum1 = readRDS(paste0(model_results_dir, "/ts1evsum1.rds"))
ts2evsum1 = readRDS(paste0(model_results_dir, "/ts2evsum1.rds"))
ts3evsum1 = readRDS(paste0(model_results_dir, "/ts3evsum1.rds"))
ts4evsum1 = readRDS(paste0(model_results_dir, "/ts4evsum1.rds"))

ts1evsum2 = readRDS(paste0(model_results_dir, "/ts1evsum2.rds"))
ts2evsum2 = readRDS(paste0(model_results_dir, "/ts2evsum2.rds"))
ts3evsum2 = readRDS(paste0(model_results_dir, "/ts3evsum2.rds"))
ts4evsum2 = readRDS(paste0(model_results_dir, "/ts4evsum2.rds"))

df1 = cbind(ts1evsum1[[3]][,c(3,2,4)],ts2evsum1[[3]][,4],ts3evsum1[[3]][,4],ts4evsum1[[3]][,4])
df2 = cbind(ts1evsum2[[3]][,c(3,2,4)],ts2evsum2[[3]][,4],ts3evsum2[[3]][,4],ts4evsum2[[3]][,4])
colnames(df1) = c("RA interval", "Generation", "{1,1,*}", "{1,5,*}","{5,1,*}","{5,5,~}")
colnames(df2) = c("RA interval", "Generation", "{1,1,*}", "{1,5,*}","{5,1,*}","{5,5,~}")
df = rbind(df1,df2)
df$`RA interval`[df$`RA interval` == "(10, INF]"] = "(10,INF]"
df$Generation[df$Generation == "Normal"] = "MMGEN"
df$Generation[df$Generation == "Shifted"] = "MMGEN (shift)"
p1 = plot_stackbar(df, c8[3:8], c(" ", "Generation model", "% of spectra within interval", "CMS interval"))+ ggplot2::theme(legend.position = "top") + ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1)) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5))+
  ggplot2::theme(plot.margin = ggplot2::unit(c(0.2,0.1,0.2,0.2), "cm"))

df1 = cbind(ts1evsum1[[3]][,c(3,2,5)],ts2evsum1[[3]][,5],ts3evsum1[[3]][,5],ts4evsum1[[3]][,5])
df2 = cbind(ts1evsum2[[3]][,c(3,2,5)],ts2evsum2[[3]][,5],ts3evsum2[[3]][,5],ts4evsum2[[3]][,5])
colnames(df1) = c("RA interval", "Generation", "{1,1,*}", "{1,5,*}","{5,1,*}","{5,5,~}")
colnames(df2) = c("RA interval", "Generation", "{1,1,*}", "{1,5,*}","{5,1,*}","{5,5,~}")
df = rbind(df1,df2)
df$`RA interval`[df$`RA interval` == "(10, INF]"] = "(10,INF]"
df$Generation[df$Generation == "Normal"] = "MMGEN"
df$Generation[df$Generation == "Shifted"] = "MMGEN (shift)"
p2 = plot_stackbar(df, c8[3:8], c(" ", "Generation model", "% of spectra within interval", "Depth score interval"))+ ggplot2::theme(legend.position = "top") + ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1)) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5))+ggplot2::theme(plot.margin = ggplot2::unit(c(0.2,0.1,0.2,0.2), "cm"))

p12 = cowplot::plot_grid(p1,p2, nrow = 2, ncol = 1, labels = c("A", "B"))
plot_save(p12+ ggplot2::theme(panel.background = 
                               ggplot2::element_rect(fill = 'white', color = 'white')),
          "summary_cms_ds", 13,11)


### gen 3c illus 1 ###
df = readRDS(paste0(model_results_dir,"/gen3c_illus1_df.rds"))
dfpca = readRDS(paste0(model_results_dir,"/gen3c_illus1_dfpca.rds"))
gen12metric = readRDS(paste0(model_results_dir,"/gen3c_illus1_metrics.rds"))
p1 = plot_pca(dfpca, 1,2,FALSE,c7, df$date, unlist(df$ps), c(" ", "Spectra", "PC of blends"))+
  theme_Publication(base_size = 16)+
  ggplot2::theme(plot.title = ggplot2::element_blank())

plot_save(p1+ ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'white', color = 'white')),
          "Gen3c_Illus_1", 6,9)

median(gen12metric[[1]][1:2,3])
median(gen12metric[[2]][1:2,3])
median(gen12metric[[1]][1:2,4])
median(gen12metric[[2]][1:2,4])

df = readRDS(paste0(model_results_dir,"/gen3c_illus2_df.rds"))
dfpca = readRDS(paste0(model_results_dir,"/gen3c_illus2_dfpca.rds"))
gen12metric = readRDS(paste0(model_results_dir,"/gen3c_illus2_metrics.rds"))
p1 = plot_pca(dfpca, 1,2,FALSE,c7, df$date, unlist(df$ps), c(" ", "Spectra", "PC of blends"))+
  theme_Publication(base_size = 16)+
  ggplot2::theme(plot.title = ggplot2::element_blank())

plot_save(p1+ ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'white', color = 'white')),
          "Gen3c_Illus_2", 6,9)

median(gen12metric[[1]][1:2,3])
median(gen12metric[[2]][1:2,3])
median(gen12metric[[1]][1:2,4])
median(gen12metric[[2]][1:2,4])
