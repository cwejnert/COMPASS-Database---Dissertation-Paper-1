# =============================================================================
# W8 — MORTALITY AS REPORTED, ON THE RAW BASIS, WITH THE HARMONISED VALUE BESIDE IT
#
# The paper reports the AS-REPORTED mortality (option A) as the primary
# mortality result, with the ammonia caveat stated. This computes that column on
# exactly the same footing as everything else -- median levels, cluster
# bootstrap on the raw difference -- so the two versions sit in one table and
# the reader cannot take the headline without meeting the correction.
#
# WHY THE TWO COLUMNS MUST SHARE A TABLE. A caveat in a footnote is read by the
# people who least need it. A caveat in the adjacent column is read by everyone,
# and it converts the presentation from a claim that can be falsified into a
# comparison that cannot: "AR6 as reported says X; harmonised it says Y; here is
# why they differ." That version survives a referee who re-runs the pipeline,
# because it already contains what they would find.
#
# WHAT THE CAVEAT HAS TO SAY TO HOLD UP. Three facts, all quantified elsewhere
# in this repo, and all of which a reader can check:
#   1. REMIND is 95% of the mortality-eligible High-RE arm and 0% of High-CMT.
#   2. Ammonia is 6-12% of PM2.5 mortality in the models that report it and
#      0.15% in REMIND, because REMIND's agriculture lives in MAgPIE.
#   3. Removing ammonia from every model costs High-CMT 8.0% of its mortality
#      and High-RE 0.36% -- a 22-fold asymmetry -- and the World gap goes from
#      +1.67 to -0.08 deaths per 1,000.
# Anything weaker than that is not a caveat, it is a hedge.
#
# USAGE: Rscript W8_asreported.R
# =============================================================================
source("stratified.R.fns")
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")
set.seed(20260821)

B    <- 2000
DROP <- "R10PAC_OECD"
ALLR <- c("Aggregated R10 regions", R10_TEN)

# as-reported mortality is what load_frame() already carries
F <- load_frame("A") %>%
  mutate(stem = gsub("[-_ ]?[0-9]+(\\.[0-9]+)?[a-z]?$", "", Scenario),
         stem = sub("/.*$", "", stem), clus = paste(Model, stem))

cliff_fast <- function(a, b) {
  n1 <- length(a); n2 <- length(b); if (!n1 || !n2) return(NA_real_)
  r <- rank(c(a, b)); 2*((sum(r[(n1+1):(n1+n2)]) - n2*(n2+1)/2)/(n1*n2)) - 1
}
cell <- function(d) {
  a <- d$mort_per_1k[d$Pathway=="High-CMT"]; b <- d$mort_per_1k[d$Pathway=="High-RE"]
  ca <- d$clus[d$Pathway=="High-CMT"];       cb <- d$clus[d$Pathway=="High-RE"]
  ka <- !is.na(a); kb <- !is.na(b); a<-a[ka]; ca<-ca[ka]; b<-b[kb]; cb<-cb[kb]
  if (length(a) < 5 || length(b) < 5)
    return(tibble(n_cmt=length(a), n_re=length(b), raw_cmt=NA_real_, raw_re=NA_real_,
                  gap=NA_real_, lo=NA_real_, hi=NA_real_, pct=NA_real_))
  ua <- unique(ca); ub <- unique(cb)
  ia <- split(seq_along(a), ca); ib <- split(seq_along(b), cb)
  reps <- vapply(seq_len(B), function(i) {
    sa <- unlist(ia[sample(ua, length(ua), TRUE)], use.names=FALSE)
    sb <- unlist(ib[sample(ub, length(ub), TRUE)], use.names=FALSE)
    median(a[sa]) - median(b[sb])          # + = High-RE has fewer deaths
  }, numeric(1))
  q <- quantile(reps, c(.025,.975), na.rm=TRUE)
  tibble(n_cmt=length(a), n_re=length(b), raw_cmt=median(a), raw_re=median(b),
         gap = median(a)-median(b), lo=q[[1]], hi=q[[2]],
         pct = 100*(median(a)-median(b))/median(a))
}
AR <- expand_grid(Region=ALLR, amb=c("1.5C","2C")) %>%
  pmap_dfr(function(Region, amb)
    bind_cols(tibble(Region, amb), cell(F[F$Region==Region & F$amb==amb, ]))) %>%
  mutate(sig = !is.na(lo) & (lo>0 | hi<0),
         reg = ifelse(Region=="Aggregated R10 regions","WORLD", sub("^R10","",Region)))

HAR <- readRDS("RAW_RESULTS.rds") %>%
  filter(approach=="A full database", sample=="all scenarios", outcome=="mort_per_1k") %>%
  transmute(Region, amb, h_cmt=raw_cmt, h_re=raw_re, h_gap=gap,
            h_lo=gap_lo, h_hi=gap_hi, h_sig=sig_raw)

J <- AR %>% inner_join(HAR, by=c("Region","amb")) %>%
  mutate(shown = Region != DROP)
saveRDS(J, "W8_ASREPORTED.rds")

line("MORTALITY: AS REPORTED, AND HARMONISED, SIDE BY SIDE")
cat("gap is deaths per 1,000 AVOIDED by High-RE. Positive favours High-RE.\n\n")
print(J %>% filter(shown) %>%
      transmute(reg, amb,
                `CMT`=round(raw_cmt,2), `RE`=round(raw_re,2),
                `gap`=round(gap,2), CI=sprintf("[%+.2f,%+.2f]", lo, hi),
                sig=ifelse(sig,"*",""),
                `h_gap`=round(h_gap,2), h_sig=ifelse(h_sig,"*","")) %>%
      as.data.frame())

line("HEADLINE COUNTS, AS REPORTED vs HARMONISED (nine regions + World)")
S <- J %>% filter(shown, !is.na(gap))
print(data.frame(
  version = c("as reported (option A)","ammonia harmonised (option B)"),
  cells = c(nrow(S), nrow(S)),
  favour_RE = c(sum(S$gap>0), sum(S$h_gap>0)),
  significant_for = c(sum(S$gap>0 & S$sig), sum(S$h_gap>0 & S$h_sig)),
  significant_against = c(sum(S$gap<0 & S$sig), sum(S$h_gap<0 & S$h_sig))))

line("WORLD")
print(J %>% filter(reg=="WORLD") %>%
      transmute(amb, n=paste0(n_cmt,"v",n_re),
                as_reported=sprintf("%.2f v %.2f -> %+.2f [%+.2f,%+.2f]%s",
                                    raw_cmt, raw_re, gap, lo, hi, ifelse(sig," *","")),
                harmonised =sprintf("%.2f v %.2f -> %+.2f [%+.2f,%+.2f]%s",
                                    h_cmt, h_re, h_gap, h_lo, h_hi, ifelse(h_sig," *",""))) %>%
      as.data.frame())

line("HOW MANY CELLS DOES THE CAVEAT ACTUALLY OVERTURN?")
ch <- S %>% mutate(flips = sign(gap)!=sign(h_gap),
                   loses = sig & !h_sig)
cat("cells changing sign when harmonised:", sum(ch$flips), "of", nrow(ch), "\n")
cat("cells losing significance when harmonised:", sum(ch$loses), "\n\n")
print(ch %>% filter(flips | loses) %>%
      transmute(reg, amb, as_reported=round(gap,2), sig,
                harmonised=round(h_gap,2), h_sig) %>% as.data.frame())
cat("\nThese are the rows the caveat is FOR. Reporting option A means reporting\n")
cat("that this many of its cells do not survive the correction, in the same\n")
cat("table, not in a footnote.\n")
