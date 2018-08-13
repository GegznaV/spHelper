
# Sp <- readRDS("D:/Dokumentai/_tmp_/C_visi_be_CaF2.RDS")
#
# Sp2 <- Sp[order(Sp$ID)]
#
# indices  <-
#     cvo_create_folds(
#         data = Sp,
#         stratify_by = "Degeneration",
#         block_by    = "ID",
#         k = 2,
#         returnTrain = FALSE
#     )
#
# test_folds_BS(indices,
#           data = Sp,
#           stratify_by = "Degeneration",
#           block_by    = "ID")

