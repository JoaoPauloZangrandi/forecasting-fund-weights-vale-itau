suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
mapa <- fread(file.path(REPO, "v2 OFICIAL/data/universo_completo_gestora.csv"))
n_fundos <- mapa[, .N, by = gestora_grupo]
pg <- fread(file.path(REPO, "v2 OFICIAL/data/resumo_pares_gestora.csv"))
pg[, gestora_a := trimws(sub(" <->.*", "", par_gestora))]
pg[, gestora_b := trimws(sub(".*<-> ", "", par_gestora))]
pg <- merge(pg, n_fundos, by.x = "gestora_a", by.y = "gestora_grupo")
setnames(pg, "N", "n_fundos_a")
pg <- merge(pg, n_fundos, by.x = "gestora_b", by.y = "gestora_grupo")
setnames(pg, "N", "n_fundos_b")
pg[, taxa := n_pares_fundo / (n_fundos_a * n_fundos_b)]
setorder(pg, -taxa)
print(pg[1:10, .(par_gestora, taxa = round(taxa,2))])

cat("\n--- Itau, contagem bruta de fundos em pares (pra conferir 'domina, 671 fundos') ---\n")
itau_rows <- pg[gestora_a == "Itau" | gestora_b == "Itau"]
cat("Total pares fundo envolvendo Itau:", sum(itau_rows$n_pares_fundo), "\n")
