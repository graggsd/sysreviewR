library(RISmed)
library(sysrutils)

query <- RISmed::EUtilsSummary("myeloma",
                               retmax = 100,
                               mindate = 2010,
                               maxdate = 2011,
                               type = "esearch",
                               db = "pubmed")

myeloma_recs <- EUtilsGet(query, type="efetch", db="pubmed")
form_mm_recs <- sysrutils::format_RISmed(myeloma_recs)
devtools::use_data(myeloma_recs, form_mm_recs)
