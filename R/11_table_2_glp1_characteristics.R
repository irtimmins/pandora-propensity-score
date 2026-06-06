# =============================================
# 12 - Table 2: GLP-1 usage
# Among GLP-1 users only
# =============================================

df_glp1 <- df %>%
  dplyr::filter(GLP1_use == "Yes") %>%
  haven::zap_labels() %>%
  mutate(
    glp1_drug = factor(GLP1_drug,
                       levels = c(4, 3, 1, 2),
                       labels = c("Tirzepatide (Mounjaro, Zepbound)",
                                  "Semaglutide (Ozempic, Wegovy)",
                                  "Dulaglutide (Trulicity)",
                                  "Other")),
    
    glp1_duration = factor(GLP1_duration_cat,
                           levels = c(1, 2, 3, 4, 99),
                           labels = c("<3 months",
                                      "3-6 months",
                                      "6-12 months",
                                      ">12 months",
                                      "Not known")),
    
    glp1_source = case_when(
      GLP1_source == 1         ~ "GP",
      GLP1_source %in% c(3, 4) ~
        "Private pharmacy or healthcare provider",
      GLP1_source == 5         ~
        "Weight management clinic",
      GLP1_source == 2         ~ "Not known",
      TRUE                     ~ "Not known"
    ),
    glp1_source = factor(glp1_source,
                         levels = c("GP",
                                    "Private pharmacy or healthcare provider",
                                    "Weight management clinic",
                                    "Not known")),
    
    glp1_indication = factor(GLP1_indication,
                             levels = c(1, 2, 3, 4),
                             labels = c("Weight loss",
                                        "Diabetes",
                                        "Weight loss and diabetes",
                                        "Not known")),
    
    glp1_stopped = factor(GLP1_stopped_post_adm,
                          levels = c(1, 2, 3, 99),
                          labels = c("No",
                                     "Yes - permanently",
                                     "Yes - temporarily",
                                     "Not known")),
    
    yellow_card = case_when(
      yellow_card == 1  ~ "Yes",
      yellow_card == 0  ~ "No",
      yellow_card == 99 ~ "Not known",
      TRUE              ~ NA_character_
    ),
    yellow_card = factor(yellow_card,
                         levels = c("Yes", "No", "Not known"))
  )

glp1_vars <- c("glp1_drug", "glp1_duration",
               "glp1_source", "glp1_indication",
               "glp1_stopped", "yellow_card")

glp1_vars <- glp1_vars[glp1_vars %in% names(df_glp1)]

tab_glp1 <- CreateTableOne(
  vars       = glp1_vars,
  data       = df_glp1,
  factorVars = glp1_vars
)

# Extract and format matrix
mat_glp1 <- print(tab_glp1,
                  showAllLevels = TRUE,
                  printToggle   = FALSE,
                  noSpaces      = TRUE)

# Add % symbols
add_percent <- function(cell) {
  gsub("\\(([0-9]+\\.[0-9]+)\\)",
       "(\\1%)", cell)
}

mat_glp1_fmt <- apply(mat_glp1, c(1, 2),
                      add_percent)

# Add variable and level columns
# using same pattern as table 1
add_level_rownames <- function(mat) {
  rn          <- rownames(mat)
  current_var <- ""
  new_rn      <- character(length(rn))
  for (i in seq_along(rn)) {
    if (rn[i] != "") {
      current_var <- rn[i]
      new_rn[i]   <- paste0("HEADER__", rn[i])
    } else {
      level_val <- trimws(mat[i, 1])
      new_rn[i] <- paste0(current_var,
                          "__", level_val)
    }
  }
  rownames(mat) <- new_rn
  mat
}

insert_first_levels <- function(mat) {
  out_rows <- list()
  i <- 1
  while (i <= nrow(mat)) {
    rn <- rownames(mat)[i]
    if (startsWith(rn, "HEADER__")) {
      var_name  <- sub("HEADER__", "", rn)
      first_lev <- trimws(mat[i, 1])
      header_row           <- mat[i, , drop = FALSE]
      header_row[1, ]      <- ""
      rownames(header_row) <- var_name
      level_row            <- mat[i, , drop = FALSE]
      rownames(level_row)  <- paste0(var_name,
                                     "__", first_lev)
      out_rows[[length(out_rows) + 1]] <- header_row
      out_rows[[length(out_rows) + 1]] <- level_row
    } else {
      out_rows[[length(out_rows) + 1]] <-
        mat[i, , drop = FALSE]
    }
    i <- i + 1
  }
  do.call(rbind, out_rows)
}

mat_glp1_fmt <- insert_first_levels(
  add_level_rownames(mat_glp1_fmt))

# Display labels
var_labels <- c(
  "n"                          = "N",
  "glp1_drug (%)"              = "GLP-1RA drug",
  "glp1_drug -- Tirzepatide (Mounjaro, Zepbound)"          =
    "Tirzepatide (Mounjaro, Zepbound)",
  "glp1_drug -- Semaglutide (Ozempic, Wegovy)"             =
    "Semaglutide (Ozempic, Wegovy)",
  "glp1_drug -- Dulaglutide (Trulicity)"                   =
    "Dulaglutide (Trulicity)",
  "glp1_drug -- Other"         = "Other",
  "glp1_duration (%)"          = "Duration of use",
  "glp1_duration -- <3 months" = "<3 months",
  "glp1_duration -- 3-6 months"  = "3-6 months",
  "glp1_duration -- 6-12 months" = "6-12 months",
  "glp1_duration -- >12 months"  = ">12 months",
  "glp1_duration -- Not known"   = "Not known",
  "glp1_source (%)"            = "Source of prescription",
  "glp1_source -- GP"          = "GP",
  "glp1_source -- Private pharmacy or healthcare provider" =
    "Private pharmacy or healthcare provider",
  "glp1_source -- Weight management clinic" =
    "Weight management clinic",
  "glp1_source -- Not known"   = "Not known",
  "glp1_indication (%)"        = "Indication",
  "glp1_indication -- Weight loss" = "Weight loss",
  "glp1_indication -- Diabetes"    = "Diabetes",
  "glp1_indication -- Weight loss and diabetes" =
    "Weight loss and diabetes",
  "glp1_indication -- Not known"   = "Not known",
  "glp1_stopped (%)"           =
    "GLP-1RA discontinued after admission",
  "glp1_stopped -- No"               = "No",
  "glp1_stopped -- Yes - permanently" =
    "Yes - permanently",
  "glp1_stopped -- Yes - temporarily" =
    "Yes - temporarily",
  "glp1_stopped -- Not known"        = "Not known",
  "yellow_card (%)"            =
    "Reported via Yellow Card scheme",
  "yellow_card -- Yes"       = "Yes",
  "yellow_card -- No"        = "No",
  "yellow_card -- Not known" = "Not known"
)

# Clean rownames
rownames(mat_glp1_fmt) <- gsub("__", " -- ",
                               rownames(mat_glp1_fmt))
rownames(mat_glp1_fmt) <- gsub(" \\(%\\) -- ", " -- ",
                               rownames(mat_glp1_fmt))

is_header <- rownames(mat_glp1_fmt) %in%
  c("n", names(var_labels)[
    !grepl(" -- ", names(var_labels))])

current_rn     <- rownames(mat_glp1_fmt)
new_rn         <- var_labels[current_rn]
missing_labels <- is.na(new_rn)
new_rn[missing_labels] <- current_rn[missing_labels]
rownames(mat_glp1_fmt) <- new_rn

is_header <- rownames(mat_glp1_fmt) %in%
  var_labels[!grepl(" -- ", names(var_labels))]
is_header[rownames(mat_glp1_fmt) == "N"] <- TRUE

variable_col <- character(nrow(mat_glp1_fmt))
level_col    <- character(nrow(mat_glp1_fmt))

for (r in seq_len(nrow(mat_glp1_fmt))) {
  rn <- rownames(mat_glp1_fmt)[r]
  if (rn == "N") {
    variable_col[r] <- "N"
    level_col[r]    <- ""
  } else if (is_header[r]) {
    variable_col[r] <- rn
    level_col[r]    <- ""
  } else {
    variable_col[r] <- ""
    level_col[r]    <- rn
  }
}

combined_final <- cbind(
  Variable = variable_col,
  Level    = level_col,
  mat_glp1_fmt
)

# Remove redundant level column from tableone
combined_final <- combined_final[,
                                 !colnames(combined_final) %in% "level",
                                 drop = FALSE]

# Fix N row
n_row      <- which(combined_final[, "Variable"] == "N")
n_data_row <- which(grepl("^n -- $",
                          combined_final[, "Level"]))
if (length(n_row) > 0 & length(n_data_row) > 0) {
  combined_final[n_row, 3:ncol(combined_final)] <-
    combined_final[n_data_row,
                   3:ncol(combined_final)]
  combined_final <- combined_final[-n_data_row, ,
                                   drop = FALSE]
}

rownames(combined_final) <- NULL

cat("\n-----GLP-1 characteristics (n=211)-----\n")
print(combined_final)

# -----EXPORT TO EXCEL-----
library(openxlsx)

wb <- createWorkbook()
addWorksheet(wb, "Table 2")

style_title <- createStyle(
  textDecoration = "bold",
  fontSize       = 11
)
style_subheader <- createStyle(
  textDecoration = "bold",
  halign         = "center"
)
style_text <- createStyle(numFmt = "TEXT")

# Title row
writeData(wb, "Table 2",
          x = paste0("Table 2: GLP-1RA ",
                     "characteristics"),
          startRow = 1, startCol = 1)
addStyle(wb, "Table 2",
         style = style_title,
         rows = 1, cols = 1)

# Column header row
writeData(wb, "Table 2",
          x = "Variable",
          startRow = 2, startCol = 1)
writeData(wb, "Table 2",
          x = "Level",
          startRow = 2, startCol = 2)
writeData(wb, "Table 2",
          x = paste0("n (%)"),
          startRow = 2, startCol = 3)
addStyle(wb, "Table 2",
         style      = style_subheader,
         rows       = 2, cols = 1:3,
         gridExpand = TRUE)

# Data
df_out <- as.data.frame(combined_final,
                        stringsAsFactors = FALSE)

writeData(wb, "Table 2",
          x        = df_out,
          startRow = 3,
          startCol = 1,
          colNames = FALSE)

addStyle(wb, "Table 2",
         style      = style_text,
         rows       = 3:(nrow(df_out) + 3),
         cols       = 1:ncol(df_out),
         gridExpand = TRUE)

setColWidths(wb, "Table 2",
             cols   = 1:ncol(df_out),
             widths = c(35, 35, 15))

freezePane(wb, "Table 2",
           firstActiveRow = 3)

saveWorkbook(wb,
             "Results/table_2_glp1_characteristics.xlsx",
             overwrite = TRUE)

cat("\nSaved Results/table_2_glp1_characteristics.xlsx\n")