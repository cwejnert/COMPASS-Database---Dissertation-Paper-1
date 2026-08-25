suppressPackageStartupMessages(library(tidyverse))
OUT_DIR <- "C:/Users/camwe/Documents/Codex/2026-08-22/i-a/outputs/engineered_cmt_century_broad"
MASTER <- "C:/Users/camwe/OneDrive/Documents/YSSP_CDR_wellbeing/Outputs/COMPASS_master"
norm <- function(x) iconv(x, from = "", to = "UTF-8", sub = "")
labels <- read_csv(file.path(OUT_DIR,"engineered_cmt_century_broad_labels.csv"),show_col_types=FALSE) %>%
  mutate(Model=norm(Model),Scenario=norm(Scenario)) %>% filter(!is.na(Pathway))
make <- function(a) read_csv(file.path(MASTER,paste0("approach_",a),paste0("compass_master_dataset_",a,".csv")),show_col_types=FALSE) %>%
  mutate(Model=norm(Model),Scenario=norm(Scenario),approach=a) %>%
  select(approach,Model,Scenario,Region,Ambition,net_re_jobs_per_1k,gap_GJ_pc,headcount_pct)
d <- bind_rows(make("A"),make("C")) %>% inner_join(labels %>% select(approach,Model,Scenario,Pathway),by=c("approach","Model","Scenario")) %>%
  distinct(approach,Model,Scenario,Region,Ambition,Pathway,.keep_all=TRUE)
long <- d %>% pivot_longer(c(net_re_jobs_per_1k,gap_GJ_pc,headcount_pct),names_to="outcome",values_to="value") %>%
  mutate(arm=if_else(Pathway=="High-RE","re","cmt"))
med <- long %>% group_by(approach,Region,Ambition,outcome,arm) %>% summarise(n=n(),median=median(value,na.rm=TRUE),.groups="drop") %>% pivot_wider(names_from=arm,values_from=c(n,median)) %>%
 mutate(re_minus_cmt=median_re-median_cmt)
write_csv(med,file.path(OUT_DIR,"century_outcome_medians_no_land_engineered_cmt.csv"))
within <- long %>% group_by(approach,Model,Region,Ambition,outcome,arm) %>% summarise(v=median(value,na.rm=TRUE),.groups="drop") %>% pivot_wider(names_from=arm,values_from=v) %>% filter(!is.na(re),!is.na(cmt)) %>% mutate(re_minus_cmt=re-cmt) %>% group_by(approach,Region,Ambition,outcome) %>% summarise(n_models=n(),median_model_effect=median(re_minus_cmt),n_re_lower=sum(re_minus_cmt<0),n_re_higher=sum(re_minus_cmt>0),.groups="drop")
write_csv(within,file.path(OUT_DIR,"century_outcome_within_model_no_land_engineered_cmt.csv"))
print(med %>% filter(Region=="Aggregated R10 regions"))
