# =============================================
# 09 - Mortality associations
# Fisher exact (sparse events)
# =============================================

cat("\nMortality full cohort:\n")
print(table(df$mort90, df$GLP1_use))
print(fisher.test(table(df$mort90, df$GLP1_use)))

cat("\nMortality MI matched (dataset 1):\n")
print(table(matched_list[[1]]$mort90,
            matched_list[[1]]$GLP1_use))
print(fisher.test(table(matched_list[[1]]$mort90,
                        matched_list[[1]]$GLP1_use)))