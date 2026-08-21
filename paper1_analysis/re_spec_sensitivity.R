# =============================================================================
# RE_SPEC DEFINITIONAL SENSITIVITY — the SI table the master asks for
#
# COMPASS_master_analysis.R defines the renewables axis three ways and says:
#   "Re-run with RE_SPEC set to each value to produce the definition-sensitivity
#    table for the SI."
# It has never been run. A reviewer will ask why nuclear and biomass are excluded
# from the renewables axis, and the paper has a principled answer -- biomass is
# the substrate of the dominant CDR technology, so a BECCS-heavy scenario scores
# on BOTH classification axes at once -- but no table behind it.
#
# THIS CANNOT BE APPROXIMATED DOWNSTREAM. ds_A.rds carries only the aggregated
# "Renewable Capacity", not the per-technology capacities, so the alternative
# axes cannot be rebuilt from the analysis files. The master has to re-run.
#
# WHAT TO RUN, on the machine that holds compass_interp.rds:
#
#   1. In COMPASS_master_analysis.R set     RE_SPEC <- "low_carbon"
#      Run it. Copy the approach-A outputs somewhere safe, e.g.
#        compass_pathway_tercile_A.rds  -> pw_A_lowcarbon.rds
#        compass_master_dataset_A.rds   -> ds_A_lowcarbon.rds
#        jobs_type.rds                  -> jobs_type_lowcarbon.rds
#
#   2. Repeat with                          RE_SPEC <- "with_biomass"
#      -> pw_A_withbiomass.rds, ds_A_withbiomass.rds, jobs_type_withbiomass.rds
#
#   3. Set RE_SPEC back to "renewables" and re-run once, so the working files are
#      the published ones again. (The master overwrites in place.)
#
#   4. Send the six copied files back. This script then builds the SI table.
#
# NOTE ON WHAT CHANGES. RE_SPEC moves BOTH the classification axis AND the jobs
# grouping -- jobs_re_group follows it deliberately, so the outcome never drifts
# from the axis. So the sensitivity is not "same scenarios, different outcome";
# it is a different comparison end to end. That is the point: it asks whether the
# result is an artefact of where the renewables boundary was drawn.
#
# USAGE once the files are back:
#   Rscript re_spec_sensitivity.R
# =============================================================================
source("stratified.R.fns")
options(width = 178)
line <- function(s) cat("\n", strrep("=",78), "\n", s, "\n", strrep("=",78), "\n", sep="")

SPECS <- c(renewables = "", low_carbon = "_lowcarbon", with_biomass = "_withbiomass")
DROP  <- "R10PAC_OECD"
ALLR  <- c("Aggregated R10 regions", R10_TEN)

have <- vapply(names(SPECS), function(k) {
  sfx <- SPECS[[k]]
  all(file.exists(paste0(c("pw_A","ds_A"), sfx, ".rds")))
}, logical(1))
cat("specs available:\n")
for (k in names(SPECS))
  cat(sprintf("  %-13s %s\n", k, ifelse(have[[k]], "present", "MISSING - see the header")))
if (!have[["renewables"]]) stop("the published files (pw_A.rds, ds_A.rds) are missing")
if (sum(have) < 2) {
  cat("\nOnly the published spec is present. Run the master at the other two\n")
  cat("RE_SPEC settings first -- the instructions are in this file's header.\n")
  quit(save = "no")
}

cliff <- function(a, b) { a<-a[!is.na(a)]; b<-b[!is.na(b)]
  if (length(a) < 5 || length(b) < 5) return(NA_real_)
  r <- rank(c(a,b)); n1<-length(a); n2<-length(b)
  2*((sum(r[(n1+1):(n1+n2)]) - n2*(n2+1)/2)/(n1*n2)) - 1 }

one_spec <- function(k) {
  sfx <- SPECS[[k]]
  pw <- readRDS(paste0("pw_A", sfx, ".rds"))
  lab <- pw %>% filter(!is.na(Pathway_excl)) %>%
    distinct(Model, Scenario, Pathway = Pathway_excl)
  # jobs must come from the matching run, because jobs_re_group follows RE_SPEC
  jt_file <- paste0("jobs_type", sfx, ".rds")
  F <- if (file.exists(jt_file) || sfx == "") load_frame("A") else NULL
  if (is.null(F)) { cat("  [!] no jobs file for", k, "- skipping\n"); return(NULL) }
  key <- F %>% select(-Pathway) %>%
    inner_join(lab, by = c("Model","Scenario")) %>%
    mutate(Pathway = factor(Pathway, levels = PATHWAYS))
  expand_grid(Region = ALLR, amb = c("1.5C","2C"),
              outcome = c("REFOSS","gap_GJ_pc","mort_per_1k")) %>%
    pmap_dfr(function(Region, amb, outcome) {
      d <- key[key$Region == Region & key$amb == amb, ]
      sgn <- ifelse(outcome %in% LOWER5, -1, 1)
      tibble(spec = k, Region, amb, outcome,
             n_cmt = sum(d$Pathway == "High-CMT" & !is.na(d[[outcome]])),
             n_re  = sum(d$Pathway == "High-RE"  & !is.na(d[[outcome]])),
             adv = sgn*cliff(d[[outcome]][d$Pathway=="High-CMT"],
                             d[[outcome]][d$Pathway=="High-RE"]))
    })
}

R <- bind_rows(lapply(names(SPECS)[have], one_spec)) %>%
  mutate(family = c(REFOSS="Jobs", gap_GJ_pc="Deprivation",
                    mort_per_1k="Health")[outcome],
         shown = Region != DROP)
saveRDS(R, "RE_SPEC_SENS.rds")

line("ARM SIZES UNDER EACH DEFINITION")
print(R %>% filter(Region == "Aggregated R10 regions", outcome == "REFOSS") %>%
      select(spec, amb, n_cmt, n_re) %>% as.data.frame())

line("THE SI TABLE — cells favouring High-RE, nine regions plus World")
print(R %>% filter(shown, !is.na(adv)) %>% group_by(family, spec) %>%
      summarise(v = paste0(sum(adv > 0), "/", n()), .groups = "drop") %>%
      pivot_wider(names_from = spec, values_from = v) %>% as.data.frame())
print(R %>% filter(shown, !is.na(adv)) %>% group_by(spec) %>%
      summarise(cells = n(), RE = sum(adv > 0), pct = round(100*mean(adv > 0)),
                med_adv = round(median(adv), 3), .groups = "drop") %>%
      as.data.frame())

line("CELLS THAT CHANGE SIGN AGAINST THE PUBLISHED DEFINITION")
base <- R %>% filter(spec == "renewables") %>% select(Region, amb, outcome, base = adv)
fl <- R %>% filter(spec != "renewables") %>% inner_join(base, by = c("Region","amb","outcome")) %>%
  filter(!is.na(adv), !is.na(base), sign(adv) != sign(base))
cat(nrow(fl), "of", sum(R$spec != "renewables" & !is.na(R$adv)), "\n")
if (nrow(fl)) print(fl %>% mutate(across(where(is.numeric), ~round(.,3))) %>%
                    select(spec, Region, amb, family, base, adv) %>% as.data.frame())
cat("\nIf jobs stays 20/20 under all three definitions, the boundary between\n")
cat("'renewable' and 'low-carbon' is not what the result rests on -- which is\n")
cat("exactly the question a reviewer will ask.\n")
