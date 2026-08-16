# =============================================================================
# Replication Script
# "Democrats Won't Earn Back Voter Trust Until They Stop the Spam"
# Author: Adam Bonica
# =============================================================================
#
# This script replicates the key empirical claims in the article using
# Federal Election Commission contribution records, FEC committee/candidate
# summary files, and donor age estimates derived from voter file linkage.
#
# Required data files (in data/):
#   - all_donors_dt.rds               | Contribution-level data with donor IDs
#   - spam_pac_directory.csv                 | Directory of identified spam PACs
#   - high_volume_vendor_spending_mat.csv    | Vendor spending matrix
#   - committee_summary_2025_year_end.csv    | FEC committee summary
#   - candidate_summary_2025_year_end.csv    | FEC candidate summary
#   - pascal_network_disb_codings_final.csv  | Pascal network disbursements
#   - comms_all_2018_2026.csv                | Committee summary panel
#   - cand_comm_directory.csv                | Candidates and Comms to Include
#
# =============================================================================


# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

## Load libraries quietly and fail with a clear message if any are missing
req_pkgs <- c(
  "data.table",
  "dplyr",
  "tidyr",
  "stringr",
  "stringi",
  "purrr",
  "glue"
)

missing_pkgs <- req_pkgs[!vapply(req_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    sprintf(
      "Missing required packages: %s\nInstall them before running this replication script.",
      paste(missing_pkgs, collapse = ", ")
    ),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(stringi)
  library(purrr)
  library(glue)
})

# Optional: set a fixed number of data.table threads for reproducibility across machines
# setDTthreads(threads = 24)


DATA_DIR   <- "data"
OUTPUT_DIR <- "output"

# Formatting helper
section_header <- function(title) {
  rule <- paste(rep("=", 75), collapse = "")
  cat("\n", rule, "\n", sep = "")
  cat("  ", title, "\n", sep = "")
  cat(rule, "\n\n", sep = "")
}


# -----------------------------------------------------------------------------
# Helper: Load spam PAC directory
# -----------------------------------------------------------------------------

get_spam_pacs <- function() {
  sp <- read.csv(file.path(DATA_DIR, "spam_pac_directory_final.csv"))
  data.frame(bonica_rid = sp$bonica_rid, cmte_id = sp$committee_id)
}

# -----------------------------------------------------------------------------
# Helper: Identify candidates using high-volume fundraising vendors
# -----------------------------------------------------------------------------

get_hvv_cands <- function() {
  hrv_file <- file.path(DATA_DIR, "high_volume_vendor_spending_mat.csv")
  hrv.mat  <- read.csv(hrv_file)
  hrv.mat  <- hrv.mat[, !(colnames(hrv.mat) %in% c("X",'cmte_id'))]

  drop.cols <- c("bonica_rid", "cycle", "name")
  keep.cols <- setdiff(names(hrv.mat), drop.cols)

  # Deduplicate by bonica_rid x cycle, summing vendor spending
  key <- paste0(hrv.mat$bonica_rid, "_", hrv.mat$cycle)
  dups <- key[duplicated(key)]
  adddups <- NULL
  if (length(dups) > 0) {
    for (d in unique(dups)) {
      rows <- hrv.mat[which(key == d), ]
      reprow <- rows[1,]
      reprow[,keep.cols] <- colSums(rows[,keep.cols],na.rm=T)
      adddups <- rbind(adddups,reprow)
    }
  }
  hrv.mat <- hrv.mat[!(key %in% dups),]
  hrv.mat <- rbind(hrv.mat,adddups)
  hrv.mat[, keep.cols] <- lapply(hrv.mat[, keep.cols], function(x) as.numeric(as.character(x)))
  key <- paste0(hrv.mat$bonica_rid, "_", hrv.mat$cycle)
  hrv.mat <- hrv.mat[!duplicated(key), ]
  rownames(hrv.mat) <- paste0(hrv.mat$bonica_rid, "_", hrv.mat$cycle)

  # Filtering on high volume vendors that have serviced spam PACs
  sp <- get_spam_pacs()
  non.spampac.vendors <- as.data.frame(hrv.mat[!(hrv.mat$bonica_rid %in% sp$bonica_rid),keep.cols])
  spampac.vendors <- as.data.frame(hrv.mat[hrv.mat$bonica_rid %in% sp$bonica_rid,keep.cols])
  vv <- colSums(spampac.vendors)

  has_spending <- rowSums(hrv.mat[grepl('cand',rownames(hrv.mat)), keep.cols]) > 10000
  cat(sprintf("  Candidates with HV vendor spending: %d of %d\n",
              sum(has_spending), length(has_spending)))

  hv.cands <-names(has_spending[has_spending==TRUE])
  non.hv.cands <-names(has_spending[has_spending==FALSE])
  hv.cands.nocyc <- str_split_fixed(hv.cands,'_',2)[,1]
  # Manually verified high-volume vendor candidates


  non.hv.cands <- non.hv.cands[!(non.hv.cands %in% hv.cands)]
  hv.cands.cyc <- hv.cands
  non.hv.cands.cyc <- non.hv.cands

  hv.cands <-  str_split_fixed(hv.cands,'_',2)[,1]
  non.hv.cands <-  str_split_fixed(non.hv.cands,'_',2)[,1]

  non.hv.cands <- non.hv.cands[!(non.hv.cands %in% hv.cands)]
  # non.hv.cands <- unique(non.hv.cands,manual_nhv)

  list(hv.cands = hv.cands, non.hv.cands = non.hv.cands,
        hv.cands.cyc = hv.cands.cyc, non.hv.cands.cyc = non.hv.cands.cyc)
}


# =============================================================================
# 1. Macro Ecosystem Stats
# =============================================================================

compute_macro_ecosystem_stats <- function(dt, sp_pacs, thresh = 10000) {


  dt <- dt[amount <= thresh]

  # Donations to spam PACs
  sp_dt <- dt[bonica_rid %in% sp_pacs$bonica_rid]

  # Identify 100+ transaction donors within the spam network
  donor_counts <- sp_dt[, .(tx_count = .N), by = bonica_cid]
  hyper_donors <- donor_counts[tx_count >= 100]$bonica_cid

  sp_dt[, is_100plus := bonica_cid %in% hyper_donors]

  total_raised <- sum(sp_dt$amount, na.rm = TRUE)
  hyper_raised <- sum(sp_dt[is_100plus == TRUE]$amount, na.rm = TRUE)
  pct_donors   <- round(length(hyper_donors) / nrow(donor_counts) * 100, 2)
  pct_funds    <- round(hyper_raised / total_raised * 100, 2)

  num_hyper_donors <- length(hyper_donors)
  num_all_donors <- dt[,uniqueN(bonica_cid)]
  pct_of_all_donors <- round((num_hyper_donors/ num_all_donors)*100,3)


  cat(sprintf("  Total donations in database:           %s\n",   format(nrow(dt), big.mark = ",")))
  cat(sprintf("  Total unique donors:                   %s\n",   format(num_all_donors, big.mark = ",")))
  cat(sprintf("  Spam PACs identified:                  %d\n\n", nrow(sp_pacs)))
  cat(sprintf("  Total Raised by Spam PACs              $%.1fm\n\n", total_raised/1e6))
  cat(sprintf("  Donors who gave 100+ times to spam PACs:\n"))
  cat(sprintf("    Count:                     %s\n",   format(num_hyper_donors, big.mark = ",")))
  cat(sprintf("    Total raised:              $%.1fm\n",   hyper_raised / 1e6))
  cat(sprintf("    Share of all spam donors:  %.2f%%\n",   pct_donors))
  cat(sprintf("    Share of all spam funds:   %.2f%%\n\n", pct_funds))

  cat(sprintf("    Share of all donors:  %.2f%%\n",   pct_of_all_donors))

  return(donor_counts)
}


# =============================================================================
# 2. Candidate/Vendor
# =============================================================================

compute_candidate_vendor_overlap <- function(dt, hvv_cands, donor_counts, thresh = 10000) {

  dt <- dt[amount <= thresh & seat %in% c('federal:house','federal:senate')]
  dt <- dt[ seat %in% c('federal:house','federal:senate')]

  hyper_donors        <- donor_counts[tx_count >= 100, bonica_cid]
  hyper_donors_10plus <- donor_counts[tx_count >= 10, bonica_cid]
  sp_donors           <- donor_counts[tx_count >= 1, bonica_cid]

  hyper_to_cands <- dt[bonica_cid %in% hyper_donors]

  hvv_raised <- hyper_to_cands[bonica_rid %in% hvv_cands$hv.cands,  sum(amount, na.rm = TRUE)]
  low_raised <- hyper_to_cands[bonica_rid %in% hvv_cands$non.hv.cands, sum(amount, na.rm = TRUE)]

  cat(sprintf("  Donations from 100+ spam PAC donors to candidates, by vendor type:\n\n"))
  cat(sprintf("    To candidates USING HV vendors:     $%.2fm\n", hvv_raised / 1e6))
  cat(sprintf("    To candidates NOT using HV vendors:  $%.2fm\n\n", low_raised / 1e6))

  list(sp_donors = sp_donors, hyper_donors = hyper_donors,
       hyper_donors_10plus = hyper_donors_10plus)
}


# =============================================================================
# 3. Candidate Donor Profile
# =============================================================================

get_cand_donor_profile <- function(dt, brid, label = brid,
                                   cycle_range = 2018:2026,
                                   donor_counts,
                                   agg_age_cycle,
                                   thresh = 10000) {

  hyper_donors        <- donor_counts[tx_count >= 100, bonica_cid]
  hyper_donors_10plus <- donor_counts[tx_count >= 10, bonica_cid]
  sp_donors           <- donor_counts[tx_count >= 1, bonica_cid]

  dt <- dt[bonica_rid == brid]

  # Donor age by cycle (from pre-aggregated counts)
  cand_age <- agg_age_cycle[bonica_rid == brid]
  cycle_age <- cand_age[, .(
    mean_donor_age     = round(weighted.mean(donor_age, count), 1),
    pct_donors_over_65 = round(sum(count[donor_age >= 65]) / sum(count), 3) * 100
  ), by = cycle][order(cycle)]

  dt <- dt[amount <= thresh & cycle %in% cycle_range]

  total_count  <- nrow(dt)
  total_amount <- sum(dt$amount, na.rm = TRUE)

  # Share from spam PAC donors (1+ transactions)
  sp_count  <- nrow(dt[bonica_cid %in% sp_donors])
  sp_amount <- dt[bonica_cid %in% sp_donors, sum(amount, na.rm = TRUE)]

  # Share from 10+ spam PAC donors
  sp10_count <- nrow(dt[bonica_cid %in% hyper_donors_10plus])

  # Share from 100+ spam PAC donors
  hyper_count  <- nrow(dt[bonica_cid %in% hyper_donors])
  hyper_amount <- dt[bonica_cid %in% hyper_donors, sum(amount, na.rm = TRUE)]

  pct_count_sp    <- round((sp_count / total_count) * 100, 1)
  pct_count_10p   <- round((sp10_count / total_count) * 100, 1)
  pct_count_hyper <- round((hyper_count / total_count) * 100, 1)
  pct_amt_sp      <- round((sp_amount / total_amount) * 100, 1)
  pct_amt_hyper   <- round((hyper_amount / total_amount) * 100, 1)


  section_header(sprintf("%s (%s)", label, brid))
  cat(sprintf("  Cycles %d-%d, donations <= $%s\n",
              min(cycle_range), max(cycle_range), format(thresh, big.mark = ",")))
  cat(sprintf("  Total donations: %s  ($%.2fm)\n\n",
              format(total_count, big.mark = ","), total_amount / 1e6))

  cat(sprintf("  Share of donations (by count):\n"))
  cat(sprintf("    From any spam PAC donor (1+):    %5.1f%%\n", pct_count_sp))
  cat(sprintf("    From frequent spam donors (10+): %5.1f%%\n", pct_count_10p))
  cat(sprintf("    From hyper spam donors (100+):   %5.1f%%\n\n", pct_count_hyper))

  cat(sprintf("  Share of dollars raised:\n"))
  cat(sprintf("    From any spam PAC donor (1+):    %5.1f%%\n", pct_amt_sp))
  cat(sprintf("    From hyper spam donors (100+):   %5.1f%%  ($%.2fm)\n\n",
              pct_amt_hyper, hyper_amount / 1e6))

  cat(sprintf("  Average donor age by cycle:\n"))
  cat(sprintf("  %-8s %-12s %s\n", "Cycle", "Mean Age", "% Over 65"))
  cat(sprintf("  %s\n", paste(rep("-", 35), collapse = "")))
  for (i in seq_len(nrow(cycle_age))) {
    r <- cycle_age[i]
    cat(sprintf("  %-8d %-12.1f %.1f%%\n", r$cycle, r$mean_donor_age, r$pct_donors_over_65))
  }
  cat("\n")

  list(pct_donation_count_from_hyper_donors = pct_count_hyper,
       pct_donation_count_from_sp_donors    = pct_count_sp,
       cycle_age = cycle_age)
}


# =============================================================================
# 4. Fundraising Totals: Spam PACs vs. Senate Candidates vs. DCCC/DSCC
# =============================================================================

compute_cycle_totals_by_group <- function() {

  sp.final <- get_spam_pacs()

  # FEC summary snapshots from March 6, 2026
  cm    <- fread(file.path(DATA_DIR, "committee_summary_2025_year_end.csv"))
  cands <- fread(file.path(DATA_DIR, "candidate_summary_2025_year_end.csv"))

  # Match spam PAC committee IDs to FEC totals
  m1 <- match(sp.final$cmte_id, cm$CMTE_ID)
  sp.final$INDV_CONTB        <- cm$INDV_CONTB[m1]
  sp.final$INDV_UNITEM_CONTB <- cm$INDV_UNITEM_CONTB[m1]
  sp.final <- sp.final %>% filter(!is.na(INDV_CONTB) & INDV_CONTB > 0)

  sp_ind    <- sum(sp.final$INDV_CONTB, na.rm = TRUE)
  sp_unitem <- sum(sp.final$INDV_UNITEM_CONTB, na.rm = TRUE)

  # Democratic Senate candidates
  sen <- cands[Cand_Office == "S" & Cand_Party_Affiliation == "DEM"]
  sen$FEC_ELECTION_YR <- cm$FEC_ELECTION_YR[match(sen$Cand_Id, cm$CAND_ID)]
  sen <- sen[!is.na(FEC_ELECTION_YR)]

  sen_ind    <- sum(sen$Individual_Contribution, na.rm = TRUE)
  sen_unitem <- sum(sen$Individual_Unitemized_Contribution, na.rm = TRUE)

  # DCCC (C00000935) and DSCC (C00042366)
  dc <- cm[CMTE_ID %in% c("C00000935", "C00042366")]
  dc_ind    <- sum(dc$INDV_CONTB, na.rm = TRUE)
  dc_unitem <- sum(dc$INDV_UNITEM_CONTB, na.rm = TRUE)



  cat(sprintf("  %-20s %-18s %s\n", "Source", "All Individual", "Unitemized"))
  cat(sprintf("  %s\n", paste(rep("-", 52), collapse = "")))
  cat(sprintf("  %-20s $%-16.1f $%.1f\n", "Spam PACs",       sp_ind/1e6,  sp_unitem/1e6))
  cat(sprintf("  %-20s $%-16.1f $%.1f\n", "Dem Senate Cands", sen_ind/1e6, sen_unitem/1e6))
  cat(sprintf("  %-20s $%-16.1f $%.1f\n", "DCCC + DSCC",      dc_ind/1e6,  dc_unitem/1e6))
  cat("\n")

  tots  <- round(c(Spam_PACs = sp_ind, Senate_Cands = sen_ind, DCCC_DSCC = dc_ind) / 1e6, 1)
  utots <- round(c(Spam_PACs = sp_unitem, Senate_Cands = sen_unitem, DCCC_DSCC = dc_unitem) / 1e6, 1)

  invisible(list(sp.final = sp.final, tots = tots, utots = utots))
}


# =============================================================================
# 5. Donor Age Distributions
# =============================================================================

get_donor_age_dist <- function(agg_age, rid, nm = NA) {
  sub <- agg_age[bonica_rid %in% rid & donor_age >= 18]
  age_freqs <- sub[, .(donation_count = sum(count)),
                   by = .(donor_age = floor(donor_age))][order(donor_age)]
  out_file <- glue("{OUTPUT_DIR}/{nm}_age_freq.csv")
  write.csv(age_freqs, file = out_file, row.names = FALSE)
  cat(sprintf("  Wrote %s age distribution -> %s\n", nm, out_file))
  return(age_freqs)
}


# =============================================================================
# 6.  Calculate 65+ Percentages For Congressional Candidates
# =============================================================================

get_65plus_dist <- function(agg_age_cycle, cand_brids) {
  # Filter to congressional candidates using their bonica_rids
  pres_cands <- unique(all_donors_dt[seat == "federal:president", .(bonica_rid, cycle)])
  cong_only_cycles <- agg_age_cycle[!pres_cands, on = c("bonica_rid", "cycle")]
  sub <- cong_only_cycles[bonica_rid %in% cand_brids]
  age_65p <- sub[, .(
    pct_age_65plus = sum(count[donor_age >= 65]) / sum(count),
    N = sum(count)
  ), by = bonica_rid][rev(order(pct_age_65plus))]
  age_65p <- age_65p[N > 100]

  cc_use <- fread('data/cand_comm_directory.csv')
  age_65p[cc_use, on = .(bonica_rid = bonica.rid), Name := i.name]
  setcolorder(age_65p, c("Name", "bonica_rid", "pct_age_65plus", "N"))
  age_65p <- age_65p[!is.na(pct_age_65plus)]
  write.csv(age_65p, file = file.path(OUTPUT_DIR, 'pct_65plus_by_cand.csv'))
  return(age_65p)
}


# =============================================================================
# 7. Pascal Network Expenditure Analysis
# =============================================================================

compute_pascal_network_financials <- function() {

  pascal_disb <- fread(file.path(DATA_DIR, "pascal_network_disb_codings_final.csv"))
  pascal_cmte_ids <- unique(pascal_disb$cmte_id)

  comms.all <- read.csv(file.path(DATA_DIR, "comms_all_2018_2026.csv"))
  comms.all <- comms.all[!duplicated(comms.all$Link_Image),]
  cc2 <- comms.all %>% filter(CMTE_ID %in% pascal_cmte_ids)

  total_fed_contribs <- sum(cc2$FED_CAND_CMTE_CONTB)
  total_ies          <- sum(cc2$INDT_EXP)
  total_refunds      <- sum(cc2$TTL_CONTB_REF)



  cat(sprintf("  FEC committee summary totals (%d PACs):\n", length(pascal_cmte_ids)))
  spend_summary <- pascal_disb[, .(amount = round(sum(transaction_amt, na.rm = TRUE))),
                                by = aggregate_category]
  spend_summary <- as.data.frame(spend_summary)

  fund.am <- spend_summary[spend_summary$aggregate_category == 'FUNDRAISING','amount']
  spend_summary[spend_summary$aggregate_category == 'FUNDRAISING','amount'] <- fund.am + total_refunds
  spend_summary <- rbind(spend_summary,
    data.frame(aggregate_category = "CONTRIBS AND IE",
               amount = round(total_ies + total_fed_contribs)))
  spend_summary$pct <- round(spend_summary$amount / sum(spend_summary$amount), 3)

  cat(sprintf("  Disbursement breakdown:\n"))
  cat(sprintf("  %-30s %-16s %s\n", "Category", "Amount", "Share"))
  cat(sprintf("  %s\n", paste(rep("-", 55), collapse = "")))
  for (i in seq_len(nrow(spend_summary))) {
    r <- spend_summary[i, ]
    cat(sprintf("  %-30s $%-14s %.1f%%\n",
                r$aggregate_category, format(r$amount, big.mark = ","), r$pct * 100))
  }
  cat("\n")

  return(spend_summary)
}


# =============================================================================
# 8. Calculate Donation Counts for Spam PACs
# =============================================================================
calc_sp_donation_counts <- function(all_donors_dt,sp){
  sp.count <- all_donors_dt[bonica_rid %in% sp$bonica_rid,.(donation_count=.N),by=bonica_rid][order(-donation_count)]
  cc_use <- fread('data/cand_comm_directory.csv')
  sp.count[cc_use, on = .(bonica_rid = bonica.rid), Name := i.name]
  write.csv(sp.count[1:20,],file='output/spam_pac_donation_count_bubble_chart.csv')

  # Print share of total spam PAC donations from top 17
  top17_donations <- sp.count[1:17, sum(donation_count)]
  total_donations <- sp.count[, sum(donation_count)]
  pct <- round(100 * top17_donations / total_donations, 1)
  cat(sprintf("Top 17 spam PACs: %s of %s total itemized donations (%.1f%%)\n",
              format(top17_donations, big.mark = ","),
              format(total_donations, big.mark = ","),
              pct))
}


# =============================================================================
# Load Data
# =============================================================================

# Download all_donors_dt.rds if not already present
donors_file <- file.path(DATA_DIR, "all_donors_dt.rds")
if (!file.exists(donors_file)) {
  donors_url <- "https://stacks.stanford.edu/file/fh317bq5617/all_donors_dt.rds"
  cat(sprintf("  Downloading all_donors_dt.rds from %s ...\n", donors_url))
  if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)
  options(timeout = max(600, getOption("timeout")))  # allow up to 10 min for large file
  if (requireNamespace("httr", quietly = TRUE)) {
    httr::GET(donors_url,
              httr::write_disk(donors_file, overwrite = TRUE),
              httr::progress())
  } else {
    download.file(donors_url, destfile = donors_file, mode = "wb", quiet = FALSE)
  }
  cat(sprintf("  Saved to %s\n", donors_file))
} else {
  cat(sprintf("  Found %s, skipping download.\n", donors_file))
}

# Contribution database
all_donors_dt <- readRDS(donors_file)
setDT(all_donors_dt)

agg_age <- fread('data/donor_age_aggregate.csv')
agg_age_cycle <- fread('data/donor_age_aggregate_by_cycle.csv')

# Spam PAC directory
sp <- get_spam_pacs()

# High-volume vendor candidate classification
section_header("0. High-Volume Vendor Classification")
hv.out <- get_hvv_cands()


# =============================================================================
# Run Analysis
# =============================================================================

# --- 1. Macro ecosystem ---
section_header("1. Spam PAC Ecosystem Overview")
donor_counts <- compute_macro_ecosystem_stats(all_donors_dt, sp, thresh = 10000)

# --- 2. Candidate/vendor overlap ---
section_header("2. Where Do 100+ Spam Donors' Candidate Donations Go?")

ccvo.out <- compute_candidate_vendor_overlap(all_donors_dt, hv.out, donor_counts)

# --- 3. Jeffries donor profile ---
section_header("3. Donor Profiles for Jeffries and Wood")

hj_profile <- get_cand_donor_profile(
  dt = all_donors_dt,
  brid = "cand43832",
  label = "Hakeem Jeffries",
  cycle_range = 2024:2026,
  donor_counts = donor_counts,
  agg_age_cycle=agg_age_cycle
)

wood_profile <- get_cand_donor_profile(
  dt = all_donors_dt,
  brid = "cand167616",
  label = "Jordan Wood",
  cycle_range = 2026,
  donor_counts = donor_counts,
  agg_age_cycle=agg_age_cycle
)


# --- 4. Age distributions ---
section_header("4. Donor Age Distributions (exported to CSV)")
jeffries.age.dist <- get_donor_age_dist(agg_age, "cand43832",  "Jeffries")
aoc.age.dist      <- get_donor_age_dist(agg_age, "cand140679", "AOC")
dscc.age.dist     <- get_donor_age_dist(agg_age, "comm7886",   "DSCC")
dccc.age.dist     <- get_donor_age_dist(agg_age, "comm6904",   "DCCC")
cat("\n")

# --- 5. Percentage 65 plus ---
section_header("5. Calculate Percentage of Donors 65+ By Candidate")
# Get congressional candidate bonica_rids for the 65+ analysis
cc_dir <- fread('data/cand_comm_directory.csv')
cong_brids <- unique(cc_dir[grepl('cand', bonica.rid)]$bonica.rid)
pct_65p <- get_65plus_dist(agg_age_cycle, cand_brids = cong_brids)

# --- 6. Cycle totals ---
section_header("6. Fundraising Totals Comparison (2026 cycle, $millions)")
comp_totals <- compute_cycle_totals_by_group()

# --- 7. Pascal Spending Summary ---
section_header("7. Pascal Network Spending Breakdown (2017-2025)")
compute_pascal_network_financials()

# --- 8. Donation count to largest spam pacs ---
calc_sp_donation_counts(all_donors_dt,sp)

cat("\n  Done.\n")
