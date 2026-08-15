# =============================================================================
# 75_construir_setor_proxy.R  (exploracao_sinais)
#
# FRENTE (C): rotacao setorial. O painel de holdings NAO tem classificacao
# setorial pronta (checado: nao existe CNAE/GICS/segmento B3 em nenhum
# arquivo de v2 OFICIAL/data -- o unico "_cache_classif_anbima.csv"
# existente e' classificacao ANBIMA de TIPO DE FUNDO, nao de setor da
# ACAO). Nas instrucoes: "se nao houver dado setorial, pular esta frente ou
# usar proxy de grupos de tickers por prefixo/nome conhecido". Optei por
# construir a proxy (nao pular) porque o nome completo da empresa esta'
# disponivel no painel (`ativo` = "NOME EMPRESA - TICKER") e permite
# classificacao razoavel por palavra-chave para a maioria dos ~513 tickers.
#
# LIMITACAO REGISTRADA COM TRANSPARENCIA: isto e' uma proxy manual/heuristica,
# NAO uma classificacao GICS/B3 oficial. Nomes ambiguos ou empresas
# diversificadas (holdings, conglomerados) podem estar mal classificados.
# Tickers nao reconhecidos ficam em "Outros/Nao classificado" e sao
# EXCLUIDOS do teste de rotacao setorial (nao viram um "setor" artificial).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

pp <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("ativo"))
pp[, ticker := trimws(sub(".*- ", "", ativo))]
pp[, nome := trimws(sub(" - .*", "", ativo))]
u <- unique(pp[, .(ticker, nome)])
setorder(u, ticker)
cat("Tickers unicos:", nrow(u), "\n")

nome_up <- toupper(u$nome)

setor <- rep(NA_character_, nrow(u))

classifica <- function(padrao, rotulo) {
  hit <- grepl(padrao, nome_up) & is.na(setor)
  setor[hit] <<- rotulo
  invisible(sum(hit))
}

# ordem importa: padroes mais especificos primeiro, genericos depois
classifica("BRADESCO|ITAU|SANTANDER|BANRISUL|BANCO DO BRASIL|\\bBB\\b ON|BANESE|BANESTES|BANCO INTER|BTG|BANCO PAN|BANCO BMG|MERC BRASIL|MERC INVEST|MERC FINANC|AMAZONIA|NORD BRASIL|ALFA INVEST|ALFA FINANC|ALFA CONSORC|ALFA HOLDING|INDUSVAL|PINE|MODALMAIS", "Bancos")
classifica("SEGURO|SEGURID|PORTO SEGURO|SUL AMERICA|IRBBRASIL|BR INSURANCE", "Seguros")
classifica("CEMIG|COPEL|CPFL|LIGHT S|NEOENERGIA|EQUATORIAL|EQTL|TAESA|ALUPAR|ENGIE|ENEVA|OMEGA GER|AES TIETE|AES BRASIL|TIET |RENOVA|GER PARANAP|CELESC|CELPA|CELPE|COELBA|COELCE|COSERN|CEB |CEEE|ELETROPAULO|ELETROBRAS|EMAE|AMPLA ENERG|ENERGISA|REDE ENERGIA|EKTR|ENCORPAR|CBOH|MEGAENERGIA|CPFL RENOVAV", "Energia Eletrica")
classifica("PETROBRAS|PETRORIO|3R PETROLEUM|ENAUTA|PETRORECSA|DOMMO|OGX|QGEP|PET MANGUINH|OCEANPACT|NOVA OLEO|COSAN LOG|RAIZEN|COSAN ON", "Petroleo e Gas")
classifica("^VALE |BRADESPAR|SID NACIONAL|GERDAU|USIMINAS|\\bCBA\\b|CSNMINERACAO|MMX MINER|PARANAPANEMA|FERBASA|ACO ALTONA|MAGNESITA|METAL IGUACU", "Mineracao e Siderurgia")
classifica("SUZANO|KLABIN|FIBRIA|CELUL IRANI|RANI", "Papel e Celulose")
classifica("BRASKEM|UNIPAR", "Quimicos e Petroquimicos")
classifica("LOJAS AMERIC|MAGAZ LUIZA|\\bVIA \\b|VIA ON|AMERICANAS|CARREFOUR BR|P\\.ACUCAR|ASSAI|GRUPO MATEUS|PETZ|GRUPO SOMA|AREZZO|VIVARA|CENTAURO|LOJAS RENNER|LOJAS MARISA|GUARARAPES|LE LIS BLANC|QUERO-QUERO|LOJAS ARAPUA|MELIUZ|PAGUE MENOS|DIMED|D1000VFARMA|CVC BRASIL|BOA VISTA|CEA MODAS|SBF ", "Varejo")
classifica("^JBS |MARFRIG|\\bBRF \\b|MINERVA|SLC AGRICOLA|SAO MARTINHO|3TENTOS|BOA SAFRA|CAMIL|M\\.DIASBRANCO|BRASILAGRO|AGROGALAXY|JALLESMACHADO|JOSAPAR|VITTIA|OUROFINO|BK BRASIL|SMART FIT", "Alimentos, Bebidas e Agropecuaria")
classifica("CYRELA|\\bMRV \\b|EVEN |EZTEC|DIRECIONAL|TENDA |GAFISA|HELBOR|TRISUL|MOURA DUBEUX|LAVVI|MITRE REALTY|CURY S|PLANOEPLANO|ROSSI RESID|PDG REALT|JHSF|MULTIPLAN|IGUATEMI|BR MALLS|ALIANSCE|SONAE|SYN PROP|RODOBENSIMOB|TERRASANTAPA|TECNISA|SAO CARLOS|BR PROPERT|CR2 |HBR REALTY|MELNICK|ALPHAVILLE|JOAO FORTES|CCPR|GENERALSHOPP", "Construcao e Imobiliario")
classifica("\\bCCR \\b|ECORODOVIAS|RUMO |\\bJSL \\b|LOCALIZA|MOVIDA|VAMOS |TEGMA|WILSON SONS|LOG-IN|LOG COM PROP|HIDROVIAS|SIMPAR|AZUL |GOL |EMBRAER|LOCAMERICA|ARMAC|SEQUOIA LOG|PRINER", "Transporte e Logistica")
classifica("TELEF BRASIL|\\bTIM \\b|\\bOI \\b|TELEBRAS|UNIFIQUE|BRISANET|DESKTOP |INFRACOMM|AFLUENTE", "Telecomunicacoes")
classifica("HAPVIDA|DASA |FLEURY|RAIADROGASIL|IHPARDINI|ODONTOPREV|QUALICORP|HYPERMARCAS|BLAU |PROFARMA|MATER DEI|KORA SAUDE|REDE D OR|ONCOCLINICAS|DIMED", "Saude")
classifica("KROTON|ANIMA |ESTACIO|SER EDUCA|YDUQS|COGNA|SOMOS EDUCA|CRUZEIRO EDU", "Educacao")
classifica("TOTVS|LINX |LOCAWEB|POSITIVO INF|INTELBRAS|BEMOBI|NEOGRID|SINQIA|CLEARSALE|GETNINJAS|DOTZ |MOSAICO|PADTEC|BOA VISTA|MULTILASER|LWSA", "Tecnologia")
classifica("ALPARGATAS|GRENDENE|CIA HERING|VULCABRAS|CAMBUCI|GRAZZIOTIN|KARSTEN|COTEMINAS|SANTANENSE|TEX RENAUX|CTAX|TECHNOS|VULCABRAS", "Textil e Vestuario")
classifica("SABESP|SANEPAR|COPASA|COMGAS", "Saneamento e Gas")
classifica("^WEG |EMBRAER|RANDON|MARCOPOLO|METAL LEVE|TUPY|IOCHP-MAXION|KEPLER WEBER|INDS ROMI|SCHULZ|FRAS-LE|TASA|TAURUS ARMAS|FORJA TAURUS|BATTISTELLA|WHIRLPOOL|MYPK", "Industria e Bens de Capital")
classifica("B3 ON|BMFBOVESPA|BBSEGURIDADE|CIELO|CETIP|BOVESPA", "Infraestrutura de Mercado")

cat("\nDistribuicao de setores atribuidos:\n")
print(table(setor, useNA = "always"))

u[, setor := setor]
u[is.na(setor), setor := "Outros/Nao classificado"]
n_classificado <- sum(u$setor != "Outros/Nao classificado")
cat(sprintf("\n%d de %d tickers (%.1f%%) classificados em algum setor.\n", n_classificado, nrow(u), 100*n_classificado/nrow(u)))

fwrite(u, file.path(OUT, "setor_proxy_tickers.csv"))
cat("\nOK -- salvo em setor_proxy_tickers.csv\n")
