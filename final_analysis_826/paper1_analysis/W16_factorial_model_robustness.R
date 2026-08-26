suppressPackageStartupMessages({library(dplyr);library(tidyr);library(purrr);library(readr)})
options(width=220)
MINN<-3
R10<-c('R10AFRICA','R10CHINA+','R10EUROPE','R10INDIA+','R10LATIN_AM','R10MIDDLE_EAST','R10NORTH_AM','R10PAC_OECD','R10REF_ECON','R10REST_ASIA')
WORLD<-'Aggregated R10 regions'; ALLR<-c(WORLD,R10)
OUTS<-c(net_re_jobs_per_1k='Employment',gap_GJ_pc='DLE gap',headcount_pct='Deprived headcount',mort_per_1k='PM2.5 mortality')
LOWER<-c('gap_GJ_pc','headcount_pct','mort_per_1k')
DEG<-'°'
norm<-function(x){x<-gsub('<U+00B0>',DEG,x,fixed=TRUE);x<-gsub('\\u00b0',DEG,x,fixed=TRUE);x<-gsub('�',DEG,x,fixed=TRUE);Encoding(x)<-'UTF-8';x}
add_pc<-function(df) df%>%mutate(mort_per_1k=cumulative_deaths_mln/pop_mln*1000,headcount_pct=mean_headcount_millions/pop_mln*100,net_re_jobs_per_1k=(jobs_Renewables-jobs_Fossil)/pop_mln,gap_GJ_pc=cumulative_gap_EJ*1000/pop_mln)
cliff_d<-function(a,b){a<-a[!is.na(a)];b<-b[!is.na(b)];if(!length(a)||!length(b))return(NA_real_);m<-outer(b,a,'-');(sum(m>0)-sum(m<0))/(length(a)*length(b))}
LP<-readRDS('LAND_PRIMARY.rds')
MORT_FILE<-Sys.getenv('COMPASS_MORTALITY_SCENARIO_VALUES','final_outcomes/mortality_reporting_complete_scenario_values_2020_2100.csv')
MORT_ALL<-read.csv(MORT_FILE,stringsAsFactors=FALSE)%>%mutate(Model=norm(Model),Scenario=norm(Scenario))

build_frame<-function(ap){
 lab<-LP$labels_land%>%filter(approach==ap)%>%select(Model,Scenario,Pathway,amb)
 ro<-read.csv(file.path('master_outputs',paste0('approach_',ap),paste0('compass_master_dataset_',ap,'.csv')),stringsAsFactors=FALSE)%>%
   mutate(Model=norm(Model),Scenario=norm(Scenario))%>%filter(Region%in%R10)%>%distinct(Model,Scenario,Region,.keep_all=TRUE)%>%
   select(Model,Scenario,Region,pop_mln,jobs_Renewables,jobs_Fossil,cumulative_gap_EJ,mean_headcount_millions,cumulative_deaths_mln)%>%add_pc()%>%
   select(Model,Scenario,Region,net_re_jobs_per_1k,gap_GJ_pc,headcount_pct)
 wld<-LP$world%>%filter(approach==ap)%>%select(Model,Scenario,Region,net_re_jobs_per_1k,gap_GJ_pc,headcount_pct)
 mort<-MORT_ALL%>%filter(approach==ap)%>%transmute(Model,Scenario,Region,mort_per_1k=cumulative_pm25_deaths_mln)
 bind_rows(ro,wld)%>%full_join(mort,by=c('Model','Scenario','Region'))%>%inner_join(lab,by=c('Model','Scenario'))%>%mutate(fam=sub('[ /-].*$','',Model),database=ifelse(ap=='A','Full','SCI-vetted'))
}
frames<-bind_rows(build_frame('A'),build_frame('C'))
designs<-crossing(database=c('Full','SCI-vetted'),ambition_view=c('All ambitions','1.5C','2C'))
pooled<-pmap_dfr(designs,function(database,ambition_view){
 db<-database; av<-ambition_view
 f<-frames%>%filter(.data$database==.env$db);if(av!='All ambitions')f<-f%>%filter(amb==.env$av)
 expand_grid(Region=ALLR,outcome=names(OUTS))%>%pmap_dfr(function(Region,outcome){
  rr<-Region; z<-f%>%filter(.data$Region==.env$rr);a<-z[[outcome]][z$Pathway=='High-CMT'];b<-z[[outcome]][z$Pathway=='High-RE'];a<-a[!is.na(a)];b<-b[!is.na(b)];sgn<-ifelse(outcome%in%LOWER,-1,1)
  tibble(database,ambition_view,Region,outcome,outcome_label=OUTS[outcome],n_cdr=length(a),n_re=length(b),median_cdr=median(a),median_re=median(b),effect=sgn*(median(b)-median(a)),favours=case_when(effect>0~'High-RE',effect<0~'High-CDR',TRUE~'Tie'))
 })
})
within<-pmap_dfr(designs,function(database,ambition_view){
 db<-database; av<-ambition_view
 f<-frames%>%filter(.data$database==.env$db);if(av!='All ambitions')f<-f%>%filter(amb==.env$av)
 expand_grid(Region=ALLR,outcome=names(OUTS))%>%pmap_dfr(function(Region,outcome){
  rr<-Region; sgn<-ifelse(outcome%in%LOWER,-1,1)
  f%>%filter(.data$Region==.env$rr)%>%group_by(fam)%>%summarise(n_cdr=sum(Pathway=='High-CMT'&!is.na(.data[[outcome]])),n_re=sum(Pathway=='High-RE'&!is.na(.data[[outcome]])),median_cdr=median(.data[[outcome]][Pathway=='High-CMT'],na.rm=TRUE),median_re=median(.data[[outcome]][Pathway=='High-RE'],na.rm=TRUE),raw_difference=median_re-median_cdr,delta=if(n_cdr>=MINN&&n_re>=MINN)sgn*cliff_d(.data[[outcome]][Pathway=='High-CMT'],.data[[outcome]][Pathway=='High-RE'])else NA_real_,.groups='drop')%>%filter(!is.na(delta))%>%mutate(database,ambition_view,Region,outcome,outcome_label=OUTS[outcome],direction_coded_difference=sgn*raw_difference,favours=case_when(delta>0~'High-RE',delta<0~'High-CDR',TRUE~'Tie'))
 })
})%>%left_join(pooled%>%select(database,ambition_view,Region,outcome,pooled_effect=effect,pooled_favours=favours),by=c('database','ambition_view','Region','outcome'))%>%mutate(agrees=sign(delta)==sign(pooled_effect))
dir.create('final_outcomes',showWarnings=FALSE)
write_csv(pooled,'final_outcomes/W16_factorial_pooled.csv')
write_csv(within,'final_outcomes/W16_factorial_within_model.csv')
summary<-within%>%group_by(database,ambition_view,outcome_label)%>%summarise(cells=n_distinct(paste(Region)),family_cell_comparisons=n(),agree_pct=100*mean(agrees),within_favours_re_pct=100*mean(delta>0),.groups='drop')
write_csv(summary,'final_outcomes/W16_factorial_summary.csv')
cat('Wrote factorial outputs\n');print(summary,n=Inf)
