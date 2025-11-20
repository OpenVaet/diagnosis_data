# Load data ---------------------------------------------------------------

interventions <- read.csv(
  "interventions.csv",          # path to your file
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# If your columns are named differently, adjust here:
# e.g. names(interventions)
code_col <- "Code"
text_col <- "Intervention"

# ------------------------------------------------------------------------
# 1. Define patterns that are likely pregnancy / obstetric related
# ------------------------------------------------------------------------

preg_patterns <- c(
  "pregnan",       # pregnancy, pregnant
  "obstetric",
  "labour", "labor",
  "caesarean", "cesarean",
  "amniotic", "amnio",
  "chorion", "chorionic",
  "fetal", "foetal",
  "placent",
  "uterus", "uterine",
  "postpartum",
  "conception",
  "insemination",
  "fertility", "fertilisation", "fertilization",
  "antenatal", "prenatal", "perinatal",
  "delivery",
  "forceps", "ventouse",
  "induction of labour",
  "external cephalic version",
  "scan", "ultrasound", "doppler"      # will combine with obstetric context
)

preg_regex <- paste(preg_patterns, collapse = "|")

# ------------------------------------------------------------------------
# 2. First pass: anything whose text matches *any* of those patterns
# ------------------------------------------------------------------------

is_preg_like <- grepl(preg_regex,
                      interventions[[text_col]],
                      ignore.case = TRUE)

preg_subset <- interventions[is_preg_like, ]

# ------------------------------------------------------------------------
# 3. Optional: remove obvious false-positives with negative patterns
#    (add more exclusions here if you notice bad matches)
# ------------------------------------------------------------------------

exclude_patterns <- c(
  "ectopic bone",      # example of non-pregnancy "ectopic"
  "hip", "knee", "shoulder"  # ortho examples; tweak as needed
)
if (length(exclude_patterns)) {
  exclude_regex <- paste(exclude_patterns, collapse = "|")
  bad <- grepl(exclude_regex,
               preg_subset[[text_col]],
               ignore.case = TRUE)
  preg_subset <- preg_subset[!bad, ]
}

# ------------------------------------------------------------------------
# 4. Save the result
# ------------------------------------------------------------------------

out_path <- "pregnancy_related_interventions.csv"
write.csv(preg_subset, out_path, row.names = FALSE)
message("Written pregnancy-related interventions to: ", out_path)
