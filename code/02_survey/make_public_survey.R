# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Build the public (open-text-free) copy of the survey export ##########
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# The open-text answers are the part of the survey that is NOT anonymous: a handful
# of them name their own country or institute, which would link a named country to its
# stated capacity gap. This script writes a copy with every free-text answer removed
# so it can live in a public repository.
#
# The columns are BLANKED, not deleted: code/02_survey/load_survey.R addresses raw
# columns by INDEX, so dropping columns would shift every index and silently mis-read
# the file. Blanking keeps the sheet's shape (and the question row) identical, so the
# whole pipeline runs unchanged and simply yields no free text.
#
# Run:  Rscript code/02_survey/make_public_survey.R
#   in : data/survey_deidentified.xlsx          (private, never committed publicly)
#   out: data/survey_public_notext.xlsx         (safe to publish)

source("code/01_support/setup.R")
source("code/01_support/config.R"); params <- settings()
source("code/02_survey/load_survey.R")
library(writexl)

IN  <- params$survey_xlsx
OUT <- file.path(dirname(IN), "survey_public_notext.xlsx")

raw <- suppressMessages(readxl::read_excel(IN, sheet = "Content", col_names = FALSE, .name_repair = "minimal"))

# every codebook entry typed "text" EXCEPT q1 (a yes/no confirmation, not free text)
text_cols <- survey_codebook() %>% filter(type == "text", variable != "q1_is_nfp") %>% pull(col)
say(sprintf("blanking %d free-text columns: %s", length(text_cols), paste(text_cols, collapse = ", ")))

data_rows <- (params$survey_header_row + 1L):nrow(raw)
n_before  <- sum(!is.na(unlist(raw[data_rows, text_cols])))

# keep row 4 (the question wording) -- it identifies nobody -- and clear the answers below it
for (j in text_cols) raw[[j]][data_rows] <- NA_character_

n_after <- sum(!is.na(unlist(raw[data_rows, text_cols])))
say(sprintf("free-text answers removed: %d -> %d", n_before, n_after))
stopifnot(n_after == 0)

# a NAMED list sets the sheet name -- the loader looks for sheet "Content"
writexl::write_xlsx(list(Content = raw), OUT, col_names = FALSE)
say(sprintf("wrote %s (%.0f KB)", OUT, file.info(OUT)$size / 1024))

# ---- verify the public copy still loads and yields identical aggregates ----
p2 <- params; p2$survey_xlsx <- OUT
pub  <- load_survey(p2)$responses
priv <- load_survey(params)$responses
stopifnot(nrow(pub) == nrow(priv))
# compare the CLOSED questions only -- the free-text ones are meant to differ
closed <- setdiff(names(priv), survey_codebook() %>% filter(type == "text") %>% pull(variable))
same   <- all.equal(select(pub, all_of(closed)), select(priv, all_of(closed)))
say(if (isTRUE(same)) "verified: all closed-question answers identical to the private file"
    else paste("WARNING: closed answers differ:", same))
say(sprintf("verified: free-text rows in public copy = %d",
            sum(!is.na(pub$value_choice_text)) + sum(!is.na(pub$reflections))))
