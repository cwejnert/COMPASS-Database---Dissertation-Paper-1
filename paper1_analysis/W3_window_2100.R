# =============================================================================
# W3 — DOES EXTENDING THE WINDOW TO 2100 STRENGTHEN THE RESULT?
#
# READ THIS BEFORE READING THE NUMBERS.
#
# Choosing the window AFTER seeing which one gives a better answer is
# specification search. If 2100 is adopted because it scores higher, the paper
# has silently spent its degrees of freedom and any reviewer who asks "why this
# window?" has a fatal question. So the decision has to rest on a reason that
# would have been given BEFORE the numbers were seen.
#
# THERE ARE REAL ARGUMENTS ON BOTH SIDES, and they are not close to equal:
#
#   FOR 2020-2050. The two strategies diverge in exactly this period -- it is
#   when the build happens and when net zero is reached. Post-2050 scenario
#   detail is far less constrained by anything observable, employment factors
#   are extrapolated far past any data supporting them, and the DLE thresholds
#   carry an efficiency path that has been running for 80 years by 2100. The
#   window matches the policy question people actually ask.
#
#   FOR 2020-2100. It is the full scenario horizon and avoids truncating a
#   comparison that continues past 2050. If the mechanism is a permanent
#   structural difference rather than a transition surge, 2100 shows it.
#
# THE HONEST FRAMING is that 2050 is the primary window and 2100 is a
# sensitivity, which is what the paper already does -- and the SENSITIVITY IS
# MORE INFORMATIVE THAN THE HEADLINE HERE. If the jobs advantage decays toward
# 2100, that CONFIRMS the stated mechanism (a construction dividend tied to the
# rate of building) rather than undermining it. A result that got stronger at
# 2100 would actually be harder to explain.
#
# This script reports both windows, pooled and within-model, for all three
# families, and does not pick one.
#
# USAGE: Rscript W3_window_2100.R [compass_mortality_r10_noNH3.csv]
#   With the harmonised mortality file, mortality is compared on the corrected
#   basis in both windows. Without it, mortality is skipped rather than
#   reported on the uncorrected basis.
# =============================================================================
source("stratified.R.fns")
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")
set.seed(20260821)

args   <- commandArgs(trailingOnly = TRUE)
NO_NH3 <- if (length(args)) args[1] else NA_character_
B      <- 2000
DROP   <- "R10PAC_OECD"
ALLR   <- c("Aggregated R10 regions", R10_TEN)
WINS   <- c("2020-2050", "2020-2100")
OUTS   <- c(REFOSS = "Jobs", gap_GJ_pc = "Energy deprivation", mort_per_1k = "Health")

cliff_fast <- function(a, b) {
  n1 <- length(a); n2 <- length(b); if (!n1 || !n2) return(NA_real_)
  r <- rank(c(a, b)); 2*((sum(r[(n1+1):(n1+n2)]) - n2*(n2+1)/2)/(n1*n2)) - 1
}
cell <- function(d, out) {
  sgn <- ifelse(out %in% LOWER5, -1, 1)
  a <- d[[out]][d$Pathway=="High-CMT"]; b <- d[[out]][d$Pathway=="High-RE"]
  ca <- d$clus[d$Pathway=="High-CMT"];  cb <- d$clus[d$Pathway=="High-RE"]
  ka <- !is.na(a); kb <- !is.na(b); a<-a[ka]; ca<-ca[ka]; b<-b[kb]; cb<-cb[kb]
  if (length(a) < 5 || length(b) < 5)
    return(tibble(n_cmt=length(a), n_re=length(b), med_cmt=NA_real_, med_re=NA_real_,
                  pct=NA_real_, adv=NA_real_, lo=NA_real_, hi=NA_real_))
  ua <- unique(ca); ub <- unique(cb)
  ia <- split(seq_along(a), ca); ib <- split(seq_along(b), cb)
  reps <- vapply(seq_len(B), function(i) {
    sa <- unlist(ia[sample(ua, length(ua), TRUE)], use.names=FALSE)
    sb <- unlist(ib[sample(ub, length(ub), TRUE)], use.names=FALSE)
    sgn*cliff_fast(a[sa], b[sb])
  }, numeric(1))
  q <- quantile(reps, c(.025,.975), na.rm=TRUE)
  tibble(n_cmt=length(a), n_re=length(b), med_cmt=median(a), med_re=median(b),
         pct = sgn*100*(median(b)-median(a))/abs(median(a)),
         adv = sgn*cliff_fast(a,b), lo=q[[1]], hi=q[[2]])
}

# ---- optional harmonised mortality, rebuilt for BOTH windows ----------------
MORT <- NULL
if (!is.na(NO_NH3) && file.exists(NO_NH3)) {
  NA_ <- read.csv(NO_NH3, stringsAsFactors = FALSE)
  mc  <- readRDS("mort_coverage.rds")
  ORIG <- read.csv("mort_annual.csv", stringsAsFactors = FALSE)
  gate <- ORIG %>% filter(year>=2020, year<=2100) %>%
    group_by(Model=model, Scenario=scenario, Region=r10_region) %>%
    summarise(na=all(is.na(deaths_pm25)), .groups="drop") %>%
    group_by(Model, Scenario) %>% summarise(n_na=sum(na), n_reg=n(), .groups="drop") %>%
    left_join(mc %>% transmute(Model=model, Scenario=scenario, n_pm_nonzero),
              by=c("Model","Scenario")) %>%
    filter(n_na==0, n_reg==10, !is.na(n_pm_nonzero), n_pm_nonzero>=6) %>%
    select(Model, Scenario)
  pop <- readRDS("ds_A.rds") %>% filter(Variable=="Total CDR") %>%
    distinct(Model, Scenario, Region, pop_mln) %>% filter(Region %in% R10_TEN)
  POP <- bind_rows(pop, pop %>% group_by(Model,Scenario) %>% filter(n()==10) %>%
    summarise(pop_mln=sum(pop_mln), .groups="drop") %>%
    mutate(Region="Aggregated R10 regions"))
  MORT <- lapply(WINS, function(w) {
    ymax <- as.integer(sub(".*-", "", w))
    r <- NA_ %>% filter(year>=2020, year<=ymax) %>%
      group_by(Model=model, Scenario=scenario, Region=r10_region) %>%
      summarise(dm = sum(deaths_pm25*10, na.rm=TRUE)/1e6, .groups="drop")
    wr <- r %>% group_by(Model,Scenario) %>% filter(n()==10) %>%
      summarise(dm=sum(dm), .groups="drop") %>% mutate(Region="Aggregated R10 regions")
    bind_rows(r, wr) %>% semi_join(gate, by=c("Model","Scenario")) %>%
      inner_join(POP, by=c("Model","Scenario","Region")) %>%
      transmute(Model, Scenario, Region, window=w, mort_h = 1000*dm/pop_mln)
  }) %>% bind_rows()
  cat("harmonised mortality rebuilt for both windows:", nrow(MORT), "rows\n")
} else cat("[!] no harmonised mortality file supplied - Health will be skipped\n")

# ---- build both windows -----------------------------------------------------
frame <- function(w) {
  F <- load_frame("A", window = w) %>%
    mutate(stem = gsub("[-_ ]?[0-9]+(\\.[0-9]+)?[a-z]?$", "", Scenario),
           stem = sub("/.*$", "", stem), clus = paste(Model, stem),
           fam  = sub("[ /-].*$", "", Model), window = w)
  if (!is.null(MORT)) F <- F %>% select(-mort_per_1k) %>%
    left_join(MORT %>% filter(window==w) %>% select(-window),
              by=c("Model","Scenario","Region")) %>%
    rename(mort_per_1k = mort_h)
  F
}
FR <- lapply(WINS, frame); names(FR) <- WINS

OUTUSE <- if (is.null(MORT)) names(OUTS)[1:2] else names(OUTS)
R <- expand_grid(window=WINS, Region=ALLR, amb=c("1.5C","2C"), outcome=OUTUSE) %>%
  pmap_dfr(function(window, Region, amb, outcome) {
    d <- FR[[window]]; d <- d[d$Region==Region & d$amb==amb, ]
    bind_cols(tibble(window, Region, amb, outcome), cell(d, outcome))
  }) %>%
  mutate(sig = !is.na(lo) & (lo>0 | hi<0), family = OUTS[outcome],
         reg = ifelse(Region=="Aggregated R10 regions","WORLD", sub("^R10","",Region)))
saveRDS(R, "W3_WINDOWS.rds")

line("THE HEADLINE UNDER EACH WINDOW (nine regions + World)")
print(R %>% filter(Region != DROP, !is.na(adv)) %>% group_by(window, family) %>%
      summarise(cells=n(), RE=sum(adv>0), sig_for=sum(adv>0 & sig),
                sig_against=sum(adv<0 & sig), med_adv=round(median(adv),3),
                .groups="drop") %>%
      pivot_wider(names_from=window, values_from=c(RE,sig_for,sig_against,med_adv)) %>%
      as.data.frame())
cat("\ntotals:\n")
print(R %>% filter(Region != DROP, !is.na(adv)) %>% group_by(window) %>%
      summarise(cells=n(), RE=sum(adv>0), sig_for=sum(adv>0 & sig),
                sig_against=sum(adv<0 & sig), .groups="drop") %>% as.data.frame())

line("WORLD, SIDE BY SIDE")
print(R %>% filter(reg=="WORLD") %>%
      transmute(family, amb, window, med_cmt=round(med_cmt,2), med_re=round(med_re,2),
                pct=round(pct), adv=round(adv,3), sig) %>%
      arrange(family, amb, window) %>% as.data.frame())

line("WHICH CELLS CHANGE, AND IN WHICH DIRECTION")
W <- R %>% select(Region, reg, amb, outcome, family, window, adv, sig) %>%
  pivot_wider(names_from=window, values_from=c(adv, sig))
names(W) <- sub("adv_", "a", sub("sig_", "s", names(W)))
W <- W %>% mutate(shift = `a2020-2100` - `a2020-2050`)
print(W %>% filter(Region != DROP, !is.na(shift)) %>% group_by(family) %>%
      summarise(cells=n(), stronger=sum(shift>0), weaker=sum(shift<0),
                med_shift=round(median(shift),3),
                lose_sig=sum(`s2020-2050` & !`s2020-2100`),
                gain_sig=sum(!`s2020-2050` & `s2020-2100`), .groups="drop") %>%
      as.data.frame())
cat("\ncells that change sign:\n")
fl <- W %>% filter(Region != DROP, !is.na(shift),
                   sign(`a2020-2050`) != sign(`a2020-2100`))
print(if (nrow(fl)) fl %>% mutate(across(where(is.numeric), ~round(.,3))) %>%
        select(reg, amb, family, `a2020-2050`, `a2020-2100`) %>% as.data.frame() else "  none")

line("THE TEST THAT ACTUALLY MATTERS — does 2100 fix the model-composition problem?")
wm <- expand_grid(window=WINS, Region=ALLR, amb=c("1.5C","2C"), outcome=OUTUSE) %>%
  pmap_dfr(function(window, Region, amb, outcome) {
    d <- FR[[window]]; d <- d[d$Region==Region & d$amb==amb, ]
    sgn <- ifelse(outcome %in% LOWER5, -1, 1)
    d %>% group_by(fam) %>%
      summarise(na=sum(Pathway=="High-CMT" & !is.na(.data[[outcome]])),
                nb=sum(Pathway=="High-RE"  & !is.na(.data[[outcome]])),
                dlt = if (na>=3 && nb>=3)
                        sgn*cliff_d(.data[[outcome]][Pathway=="High-CMT"],
                                    .data[[outcome]][Pathway=="High-RE"]) else NA_real_,
                .groups="drop") %>% filter(!is.na(dlt)) %>%
      mutate(window=window, Region=Region, amb=amb, outcome=outcome)
  }) %>% inner_join(R %>% select(window, Region, amb, outcome, pooled=adv),
                    by=c("window","Region","amb","outcome")) %>%
  mutate(agrees = sign(dlt)==sign(pooled), family = OUTS[outcome])
print(wm %>% filter(Region != DROP) %>% group_by(family, window) %>%
      summarise(families=n(), agree_rate=round(100*mean(agrees)), .groups="drop") %>%
      pivot_wider(names_from=window, values_from=c(families, agree_rate)) %>%
      as.data.frame())
cat("\nagree_rate is the share of individual model families pointing the same way\n")
cat("as the pooled result. If 2100 does not raise it, the longer window buys\n")
cat("nothing on the problem that actually limits the paper.\n")
