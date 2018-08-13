# DataSet2 <- rbind(DataSet1,DataSet1,DataSet1,DataSet1,
#                   DataSet1,DataSet1,DataSet1,DataSet1)  %>%
#     mosaic::sample()
#
# FoldsOBJ <- cvo_create_folds(data = DataSet2,
#                           stratify_by = "gr",
#                           block_by = "ID",
#                           returnTrain = FALSE)
#
# test_folds_BS(obj = FoldsOBJ,
#           data = DataSet2,
#           stratify_by = "gr",
#           block_by = "ID")
#
# DATA = DataSet2
# stratBy = "gr"
# blockBy = "ID"
