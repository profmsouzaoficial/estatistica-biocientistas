# =============================================================================
#  gerar_figuras.R — figuras didáticas do capítulo 4 (AED)
# -----------------------------------------------------------------------------
#  Gera todos os PNGs em img/cap04/. Todos os dados são SIMULADOS e os
#  parâmetros estão declarados no comentário de cada bloco — nada aqui é dado
#  real de paciente. Rodar da raiz do projeto:
#
#      Rscript img/cap04/gerar_figuras.R
#
#  Requer: ggplot2, dplyr, tidyr, scales, patchwork, survival, igraph, treemap,
#  ggrepel, viridis, ragg.
# =============================================================================

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(scales)
  library(patchwork); library(survival); library(igraph); library(treemap)
  library(ggrepel); library(viridis); library(ragg); library(grid)
})

set.seed(20261126)
outdir <- "img/cap04"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

## paleta da identidade do livro -------------------------------------------
teal      <- "#0d9488"
teal_d    <- "#0f766e"
teal_l    <- "#99f6e4"
amber     <- "#d97706"
amber_l   <- "#fde68a"
red       <- "#dc2626"
red_l     <- "#fecaca"
purple    <- "#7c3aed"
blue      <- "#2563eb"
ink       <- "#0b1220"
body      <- "#334155"
muted     <- "#64748b"
line      <- "#e6ecf5"

tema <- function(base = 11) {
  theme_minimal(base_size = base, base_family = "Inter") +
    theme(
      plot.title      = element_text(face = "bold", colour = ink, size = base + 2,
                                     margin = margin(b = 4)),
      plot.subtitle   = element_text(colour = muted, size = base - 1,
                                     margin = margin(b = 10)),
      plot.caption    = element_text(colour = muted, size = base - 3, hjust = 0),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = line, linewidth = .35),
      axis.title       = element_text(colour = body, size = base - 1),
      axis.text        = element_text(colour = muted, size = base - 2),
      legend.position  = "top",
      legend.title     = element_blank(),
      legend.text      = element_text(colour = body, size = base - 2),
      plot.background  = element_rect(fill = "white", colour = NA),
      plot.margin      = margin(14, 18, 10, 12)
    )
}

n1 <- function(v) sub("\\.", ",", sprintf("%.1f", v))

salvar <- function(p, nome, w = 7.6, h = 4.3, dpi = 220) {
  message("  >> ", nome)
  ragg::agg_png(file.path(outdir, paste0(nome, ".png")),
                width = w, height = h, units = "in", res = dpi, background = "white")
  print(p); dev.off()
  cat("  ok:", nome, "\n")
}

ok <- function(expr) {
  tryCatch(expr, error = function(e) cat("  ERRO:", conditionMessage(e), "\n"))
  invisible(NULL)
}

cat("Gerando figuras do capítulo 4...\n")

# ---------------------------------------------------------------------------
# 1. Pizza x barras — dispensação em uma farmácia-escola (dados simulados)
# ---------------------------------------------------------------------------
ok({
  d <- data.frame(
    classe = c("Analgésicos", "Anti-inflamatórios", "Antibióticos",
               "Anti-hipertensivos", "Antidiabéticos", "Vitaminas",
               "Ansiolíticos", "Outros"),
    n = c(340, 220, 150, 120, 80, 50, 30, 10)
  )
  d$classe <- factor(d$classe, levels = d$classe[order(d$n)])

  p_pizza <- ggplot(d, aes(x = "", y = n, fill = classe)) +
    geom_col(width = 1, colour = "white", linewidth = .6) +
    coord_polar(theta = "y") +
    scale_fill_viridis_d(option = "mako", begin = .25, end = .85) +
    labs(title = "Pizza: 8 fatias, 8 ângulos para comparar",
         subtitle = "Qual classe é o dobro de qual? O olho não responde.") +
    theme_void(base_family = "Inter") +
    theme(plot.title = element_text(face = "bold", colour = ink, size = 12),
          plot.subtitle = element_text(colour = muted, size = 10),
          legend.position = "right", legend.text = element_text(size = 7.5, colour = body),
          legend.key.size = unit(.7, "lines"), legend.title = element_blank(),
          plot.margin = margin(10, 6, 10, 6))

  p_barras <- ggplot(d, aes(x = n, y = classe)) +
    geom_col(fill = teal, width = .62) +
    geom_text(aes(label = n), hjust = -0.25, colour = body, size = 3.1,
              family = "Inter") +
    scale_x_continuous(expand = expansion(mult = c(0, .18))) +
    labs(title = "Barras ordenadas: a ordem faz o trabalho",
         subtitle = "Do maior para o menor, com o número na ponta.",
         x = "Dispensações no mês", y = NULL) +
    tema()

  salvar(p_pizza + p_barras + plot_layout(widths = c(0.95, 1.55)),
         "fig04_pizza_vs_barras", w = 9.6, h = 4.6)
})

# ---------------------------------------------------------------------------
# 2. O mesmo dado, errado e certo — taxa de sucesso por técnica
#    (valores simulados: 88,2% a 91,0%)
# ---------------------------------------------------------------------------
ok({
  d <- data.frame(
    tecnica = c("Convenc.", "Manual", "Instrum.", "Mista"),
    sucesso = c(88.2, 89.1, 90.4, 91.0)
  )
  d$tecnica_ord <- factor(d$tecnica, levels = d$tecnica)
  d$tecnica_alf <- factor(d$tecnica, levels = sort(d$tecnica))

  p_errado <- ggplot(d, aes(x = tecnica_alf, y = sucesso, fill = tecnica_alf)) +
    geom_col(width = .66) +
    coord_cartesian(ylim = c(87, 92)) +
    scale_fill_manual(values = c("#e11d48", "#f59e0b", "#10b981", "#3b82f6")) +
    labs(title = "Errado", subtitle = "Eixo de 87 a 92, ordem alfabética, cor sem função.",
         x = NULL, y = "Sucesso (%)") +
    tema() + theme(legend.position = "none",
                   plot.title = element_text(colour = red),
                   axis.text.x = element_text(angle = 20, hjust = 1))

  p_certo <- ggplot(d, aes(x = sucesso, y = reorder(tecnica, sucesso))) +
    geom_col(fill = teal, width = .6) +
    geom_text(aes(label = paste0(format(sucesso, decimal.mark = ","), "%")),
              hjust = -0.2, size = 3.1, colour = body, family = "Inter") +
    scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, .12))) +
    labs(title = "Certo", subtitle = "Eixo em zero, ordenado, um valor por barra.",
         x = "Sucesso (%)", y = NULL) +
    tema() + theme(plot.title = element_text(colour = teal_d))

  salvar(p_errado + p_certo, "fig04_barras_errado_certo", w = 8.6, h = 4.3)
})

# ---------------------------------------------------------------------------
# 3. Três jeitos de mostrar a mesma tabela: agrupado, empilhado, 100%
#    (simulado: resposta ao tratamento em 3 grupos, n = 40 cada)
# ---------------------------------------------------------------------------
ok({
  d <- data.frame(
    grupo = rep(c("Controle", "Fisioterapia", "Fisio + orient."), each = 3),
    resposta = factor(rep(c("Melhora", "Estável", "Piora"), 3),
                      levels = c("Melhora", "Estável", "Piora")),
    n = c(12, 20, 8,      # Controle
          21, 15, 4,      # Fisioterapia
          28, 9,  3)      # Fisioterapia + orientação
  )
  d$grupo <- factor(d$grupo, levels = c("Controle", "Fisioterapia", "Fisio + orient."))
  cols <- c("Melhora" = teal, "Estável" = "#94a3b8", "Piora" = amber)

  base_tema <- tema() +
    theme(axis.text.x = element_text(angle = 25, hjust = 1),
          plot.subtitle = element_text(size = 8.6))

  p1 <- ggplot(d, aes(grupo, n, fill = resposta)) +
    geom_col(position = position_dodge(.78), width = .7) +
    scale_fill_manual(values = cols) +
    labs(title = "Agrupado", subtitle = "Compara entre grupos",
         x = NULL, y = "Pacientes") + base_tema

  p2 <- ggplot(d, aes(grupo, n, fill = resposta)) +
    geom_col(width = .66, position = position_stack(reverse = TRUE)) +
    scale_fill_manual(values = cols) +
    labs(title = "Empilhado", subtitle = "Total e composição",
         x = NULL, y = "Pacientes") + base_tema

  p3 <- ggplot(d, aes(grupo, n, fill = resposta)) +
    geom_col(width = .66, position = position_fill(reverse = TRUE)) +
    scale_fill_manual(values = cols) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(title = "100% empilhado", subtitle = "Só proporções",
         x = NULL, y = "Proporção") + base_tema

  salvar((p1 | p2 | p3) + plot_layout(guides = "collect"), "fig04_empilhadas",
         w = 9.6, h = 4.5)
})

# ---------------------------------------------------------------------------
# 4. Série temporal: tendência, sazonalidade e intervenção
#    (simulado: casos mensais de uma arbovirose, 60 meses, pico em março)
# ---------------------------------------------------------------------------
ok({
  meses <- 1:60
  t <- seq(as.Date("2021-01-01"), by = "month", length.out = 60)
  saz <- 45 * sin(2 * pi * (meses - 1) / 12 + 1.2)
  tend <- 120 + 1.4 * meses
  intervencao <- ifelse(meses >= 40, -55, 0)
  casos <- pmax(0, round(tend + saz + intervencao + rnorm(60, 0, 9)))

  d <- data.frame(mes = t, casos = casos)
  mm <- stats::filter(d$casos, rep(1 / 12, 12), sides = 2)
  d$media_movel <- as.numeric(mm)

  p1 <- ggplot(d, aes(mes, casos)) +
    geom_col(fill = teal_l, width = 24) +
    geom_line(colour = teal, linewidth = .9) +
    geom_vline(xintercept = as.numeric(as.Date("2024-04-01")),
               linetype = "dashed", colour = red, linewidth = .6) +
    annotate("text", x = as.Date("2024-05-10"), y = max(d$casos) * .96,
             label = "campanha de\ncontrole vetorial", hjust = 0, size = 3,
             colour = red, family = "Inter", lineheight = .95) +
    labs(title = "O que a série temporal mostra",
         subtitle = "Cada barra é um mês: repare na subida anual e na queda depois da intervenção.",
         x = NULL, y = "Casos notificados") +
    tema()

  p2 <- ggplot(d, aes(mes, casos)) +
    geom_point(colour = "#cbd5e1", size = 1.7) +
    geom_line(aes(y = media_movel), colour = amber, linewidth = 1.1, na.rm = TRUE) +
    labs(title = "A mesma série, com média móvel de 12 meses",
         subtitle = "A linha amarela remove a sazonalidade e revela a tendência.",
         x = NULL, y = "Casos") +
    tema()

  salvar(p1 / p2, "fig04_serie_temporal", w = 8.4, h = 6.4)
})

# ---------------------------------------------------------------------------
# 5. Dados pareados: antes e depois no mesmo paciente (fisioterapia)
#    (simulado: EVA 0-10 em 12 pacientes)
# ---------------------------------------------------------------------------
ok({
  antes  <- c(8, 7, 9, 6, 8, 7, 9, 5, 8, 7, 6, 9)
  depois <- c(4, 5, 3, 7, 4, 2, 5, 4, 3, 6, 3, 7)
  d <- data.frame(paciente = factor(paste0("P", 1:12), levels = paste0("P", rev(1:12))),
                  antes = antes, depois = depois)
  d$melhora <- d$antes - d$depois

  p <- ggplot(d, aes(y = paciente)) +
    geom_segment(aes(x = depois, xend = antes, yend = paciente),
                 colour = "#cbd5e1", linewidth = 1.1) +
    geom_point(aes(x = antes), colour = muted, size = 2.8) +
    geom_point(aes(x = depois), colour = teal, size = 2.8) +
    geom_text(aes(x = pmax(antes, depois) + .4,
                  label = ifelse(melhora >= 0, paste0("\u2212", melhora),
                                 paste0("+", -melhora)),
                  colour = melhora >= 0), size = 2.9, family = "Inter",
              show.legend = FALSE) +
    scale_colour_manual(values = c("TRUE" = muted, "FALSE" = red)) +
    scale_x_continuous(limits = c(1, 11), breaks = 0:10) +
    labs(title = "Cada linha \u00e9 um paciente",
         subtitle = "O gr\u00e1fico pareado mostra a melhora individual \u2014 a m\u00e9dia esconde quem piorou.",
         x = "Dor na EVA (0 a 10)", y = NULL,
         caption = "C\u00edrculo cinza: antes \u00b7 verde: depois \u00b7 n\u00famero: varia\u00e7\u00e3o em pontos (vermelho: piora)") +
    tema() + theme(panel.grid.major.y = element_blank())

  salvar(p, "fig04_pares")
})

# ---------------------------------------------------------------------------
# 6. Pirâmide etária — internações por faixa de idade e sexo
#    (simulado: 1.000 internações de uma clínica-escola)
# ---------------------------------------------------------------------------
ok({
  faixas <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70+")
  hom <- c(38, 26, 44, 52, 61, 74, 88, 96)
  mul <- c(30, 31, 68, 79, 84, 92, 101, 126)
  d <- data.frame(faixa = rep(faixas, 2),
                  sexo = rep(c("Homens", "Mulheres"), each = 8),
                  n = c(hom, mul))
  d$faixa <- factor(d$faixa, levels = faixas)
  d$n_plot <- ifelse(d$sexo == "Homens", -d$n, d$n)

  p <- ggplot(d, aes(x = n_plot, y = faixa, fill = sexo)) +
    geom_col(width = .72) +
    geom_text(aes(label = abs(n_plot), hjust = ifelse(n_plot < 0, 1.25, -0.25)),
              size = 2.8, colour = body, family = "Inter") +
    scale_fill_manual(values = c("Homens" = "#0ea5e9", "Mulheres" = "#db2777")) +
    scale_x_continuous(labels = abs, limits = c(-165, 165)) +
    geom_vline(xintercept = 0, colour = muted, linewidth = .4) +
    labs(title = "Pirâmide etária das internações",
         subtitle = "Duas categorias, uma linha de base no centro: o formato aparece na hora.",
         x = "Internações", y = NULL,
         caption = sprintf("Dados simulados, %s internações.", format(sum(d$n), big.mark = "."))) +
    tema() + theme(panel.grid.major.y = element_blank(),
                   legend.position = c(.86, .12))

  salvar(p, "fig04_piramide")
})

# ---------------------------------------------------------------------------
# 7. Anatomia do boxplot — os cinco números, com os pontos à mostra
#    (simulado: 28 valores assimétricos, dois deles bem afastados)
# ---------------------------------------------------------------------------
ok({
  x <- c(10.9, 11.2, 11.4, 11.6, 11.8, 12.0, 12.1, 12.2, 12.3, 12.4,
         12.5, 12.6, 12.7, 12.8, 13.0, 13.1, 13.2, 13.4, 13.6, 13.9,
         14.2, 14.5, 14.9, 15.4, 16.2, 16.9, 21.3, 23.1)
  q <- quantile(x, c(.25, .5, .75)); iiq <- unname(q[3] - q[1])
  hi <- max(x[x <= q[3] + 1.5 * iiq])

  p <- ggplot(data.frame(x = x), aes(x = "", y = x)) +
    geom_jitter(width = .055, colour = "#94a3b8", size = 1.55, alpha = .8) +
    geom_boxplot(width = .30, outlier.shape = NA, colour = teal_d,
                 fill = "#ccfbf1", linewidth = .85) +
    annotate("segment", x = .62, xend = .62, y = q[1], yend = q[3], colour = red,
             linewidth = .55, arrow = arrow(angle = 11, length = unit(2.4, "mm"),
                                            ends = "both")) +
    annotate("text", x = .50, y = mean(q[c(1, 3)]), label = paste("IIQ =", n1(iiq)),
             angle = 90, vjust = 1.2, size = 2.8, colour = red, family = "Inter") +
    annotate("segment", x = 1.16, xend = 1.27, y = c(q[3], q[2], q[1], hi, max(x)),
             yend = c(q[3], q[2], q[1], hi, max(x)), colour = "#cbd5e1", linewidth = .3) +
    annotate("text", x = 1.30, y = q[3], label = paste("Q3 =", n1(q[3])), hjust = 0,
             size = 3, colour = ink, family = "Inter", fontface = "bold") +
    annotate("text", x = 1.30, y = q[2], label = paste("mediana =", n1(q[2])), hjust = 0,
             size = 3, colour = teal_d, family = "Inter", fontface = "bold") +
    annotate("text", x = 1.30, y = q[1], label = paste("Q1 =", n1(q[1])), hjust = 0,
             size = 3, colour = ink, family = "Inter", fontface = "bold") +
    annotate("text", x = 1.30, y = hi, label = "bigode superior", hjust = 0,
             size = 2.9, colour = body, family = "Inter") +
    annotate("text", x = 1.30, y = max(x), label = "outlier", hjust = 0,
             size = 2.9, colour = red, family = "Inter") +
    coord_cartesian(xlim = c(.55, 2.05), ylim = c(10.3, 24)) +
    labs(title = "Os cinco números de um boxplot",
         subtitle = "A caixa cobre os 50% centrais; o bigode para onde ainda há dado plausível.",
         x = NULL, y = "Hemoglobina (g/dL)") +
    tema() + theme(axis.text.x = element_blank(),
                   panel.grid.major.x = element_blank())

  salvar(p, "fig04_boxplot_anatomia", w = 7.6, h = 4.5)
})

# ---------------------------------------------------------------------------
# 8. Dispersão com sobreposição tratada — hemoglobina x idade
#    (simulado: n = 240, dois perfis)
# ---------------------------------------------------------------------------
ok({
  n <- 240
  idade <- round(rnorm(n, 52, 17))
  idade <- pmin(pmax(idade, 18), 92)
  anemi <- rbinom(n, 1, plogis(-2.4 + (idade - 50) * .04))
  hb <- 13.9 - .022 * (idade - 40) - 2.6 * anemi + rnorm(n, 0, .75)
  d <- data.frame(idade, hb, perfil = ifelse(anemi == 1, "Anemia provável", "Sem anemia"))

  p <- ggplot(d, aes(idade, hb, colour = perfil)) +
    geom_point(size = 1.9, alpha = .55) +
    geom_smooth(aes(group = 1), method = "loess", se = FALSE,
                colour = ink, linewidth = .8) +
    scale_colour_manual(values = c("Sem anemia" = "#64748b", "Anemia provável" = teal)) +
    labs(title = "A nuvem de pontos conta mais que o coeficiente",
         subtitle = "A cor separa dois grupos que a média juntaria num número só.",
         x = "Idade (anos)", y = "Hemoglobina (g/dL)",
         caption = "Linha preta: tendência geral (loess). Dados simulados, n = 240.") +
    tema()
  salvar(p, "fig04_dispersao", w = 7.6, h = 4.4)
})

# ---------------------------------------------------------------------------
# 9. Mapa de calor de correlação entre variáveis clínicas
#    (simulado: n = 300, estrutura de correlação declarada no código)
# ---------------------------------------------------------------------------
ok({
  n <- 300
  idade <- rnorm(n, 55, 14)
  imc <- 24 + .06 * idade + rnorm(n, 0, 3.4)
  pas <- 105 + .55 * idade + .9 * imc + rnorm(n, 0, 9)
  pad <- 62 + .30 * pas + rnorm(n, 0, 6)
  gli <- 92 + .45 * idade + .55 * imc + rnorm(n, 0, 11)
  hb  <- 14.2 - .03 * idade + rnorm(n, 0, 1.2)
  cre <- 0.75 + .006 * idade + rnorm(n, 0, .12)
  col <- 180 + .8 * idade + .35 * imc + rnorm(n, 0, 22)
  d <- data.frame(Idade = idade, IMC = imc, PAS = pas, PAD = pad,
                  Glicemia = gli, Hemoglobina = hb, Creatinina = cre,
                  Colesterol = col)
  m <- round(cor(d), 2)
  m[upper.tri(m)] <- NA
  df <- expand.grid(V1 = rownames(m), V2 = colnames(m), stringsAsFactors = FALSE)
  df$r <- as.vector(m)
  df$lab <- ifelse(is.na(df$r), "", format(df$r, decimal.mark = ",", nsmall = 2))
  df$V1 <- factor(df$V1, levels = colnames(m)); df$V2 <- factor(df$V2, levels = rev(colnames(m)))

  p <- ggplot(df, aes(V1, V2, fill = r)) +
    geom_tile(colour = "white", linewidth = .9) +
    geom_text(aes(label = lab), size = 2.6, colour = ink, family = "Inter") +
    scale_fill_gradient2(low = "#1d4ed8", mid = "white", high = "#b91c1c",
                         midpoint = 0, limits = c(-1, 1), na.value = "white") +
    labs(title = "Mapa de calor da correlação",
         subtitle = "Vermelho: crescem juntos. Azul: um cresce quando o outro cai.",
         x = NULL, y = NULL) +
    tema() + theme(axis.text.x = element_text(angle = 35, hjust = 1),
                   panel.grid = element_blank(),
                   legend.position = "right", legend.key.height = unit(1.6, "lines"))
  salvar(p, "fig04_heatmap_correlacao", w = 7.4, h = 5.6)
})

# ---------------------------------------------------------------------------
# 10. Volcano plot — muitos genes, dois eixos, uma decisão
#     (simulado: 3.000 genes, efeito nulo na maioria)
# ---------------------------------------------------------------------------
ok({
  set.seed(7)
  n <- 3000
  fc <- rnorm(n, 0, .75)
  se <- runif(n, .18, .45)
  z <- fc / se
  p <- 2 * pnorm(-abs(z))
  d <- data.frame(gene = sprintf("G%04d", 1:n), fc = fc, p = p)
  d$sig <- with(d, ifelse(p < .05 & abs(fc) > 1, "Significativo",
                    ifelse(p < .05, "p baixo, efeito pequeno", "Não significativo")))
  d$sig <- factor(d$sig, levels = c("Significativo", "p baixo, efeito pequeno", "Não significativo"))
  rot <- d %>% filter(sig == "Significativo") %>% arrange(p) %>% slice_min(p, n = 5)

  p <- ggplot(d, aes(fc, -log10(p), colour = sig)) +
    geom_point(size = 1.1, alpha = .75) +
    geom_vline(xintercept = c(-1, 1), linetype = "dashed", colour = muted, linewidth = .4) +
    geom_hline(yintercept = -log10(.05), linetype = "dashed", colour = muted, linewidth = .4) +
    ggrepel::geom_text_repel(data = rot, aes(label = gene), size = 2.7,
                             colour = ink, family = "Inter", min.segment.length = 0,
                             segment.colour = muted, seed = 3) +
    scale_colour_manual(values = c("Significativo" = red,
                                   "p baixo, efeito pequeno" = amber,
                                   "Não significativo" = "#cbd5e1")) +
    labs(title = "Volcano: efeito no eixo x, evidência no eixo y",
         subtitle = "S\u00f3 o canto superior \u00e9 grande e confi\u00e1vel. O resto \u00e9 ru\u00eddo ou efeito m\u00ednimo.",
         x = "log2 da razão de expressão", y = expression(-log[10]~"(valor de p)"),
         caption = "3.000 genes simulados; linhas tracejadas: |log2FC| = 1 e p = 0,05.") +
    tema() + theme(legend.position = "top")
  salvar(p, "fig04_volcano", w = 7.6, h = 4.6)
})

# ---------------------------------------------------------------------------
# 11. Curva de sobrevida (Kaplan-Meier) com censura e tabela de risco
#     (simulado: 2 braços, n = 120 cada; censura administrativa até 60 meses)
# ---------------------------------------------------------------------------
ok({
  set.seed(11)
  n <- 120
  t1 <- rexp(n, 1 / 16); t2 <- rexp(n, 1 / 30)
  c1 <- runif(n, 8, 60); c2 <- runif(n, 8, 60)
  d <- data.frame(time = c(pmin(t1, c1), pmin(t2, c2)),
                  status = c(as.integer(t1 <= c1), as.integer(t2 <= c2)),
                  braço = rep(c("Controle", "Intervenção"), each = n))
  sf <- survfit(Surv(time, status) ~ braço, data = d)
  df <- data.frame(time = sf$time, surv = sf$surv,
                   braço = rep(c("Controle", "Intervenção"), sf$strata),
                   cens = sf$n.censor)
  df <- rbind(data.frame(time = 0, surv = 1, braço = c("Controle", "Intervenção"), cens = 0), df)
  med <- summary(sf)$table[, "median"]

  p_km <- ggplot(df, aes(time, surv, colour = braço)) +
    geom_step(linewidth = .95) +
    geom_point(data = subset(df, cens > 0), shape = "|", size = 3.2, stroke = .85) +
    scale_colour_manual(values = c("Controle" = muted, "Intervenção" = teal),
                        guide = guide_legend(override.aes = list(shape = NA))) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, 12)) +
    annotate("segment", x = med[1], xend = med[1], y = 0, yend = .5,
             linetype = "dotted", colour = muted) +
    annotate("segment", x = med[2], xend = med[2], y = 0, yend = .5,
             linetype = "dotted", colour = teal) +
    annotate("text", x = med[1] - 1.2, y = .60, label = paste("mediana", round(med[1]), "meses"),
             hjust = 1, size = 2.9, colour = muted, family = "Inter") +
    annotate("text", x = med[2] + 1.2, y = .66, label = paste("mediana", round(med[2]), "meses"),
             hjust = 0, size = 2.9, colour = teal_d, family = "Inter") +
    labs(title = "Sobrevida: a data do desfecho também é dado",
         subtitle = "As curvas se abrem cedo e a diferença cresce com o tempo.",
         x = NULL, y = "Proporção sem o desfecho",
         caption = "Tique: paciente censurado (saiu do estudo antes do desfecho). Dados simulados.") +
    tema()

  risco <- summary(sf, times = seq(0, 60, 12))
  rd <- data.frame(time = risco$time, n = risco$n.risk,
                   braço = sub(".*=", "", as.character(risco$strata)))
  rd$braço <- factor(rd$braço, levels = c("Intervenção", "Controle"))
  p_risco <- ggplot(rd, aes(time, braço, label = n)) +
    geom_text(size = 2.8, colour = body, family = "Inter") +
    scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, 12),
                       expand = expansion(mult = c(.10, .02))) +
    labs(x = "Meses de acompanhamento", y = "Em risco") +
    tema() + theme(panel.grid = element_blank(),
                   axis.text.y = element_text(size = 7.4),
                   axis.text.x = element_text(size = 7.4),
                   axis.title.x = element_text(size = 8),
                   plot.caption = element_text(size = 7),
                   plot.margin = margin(2, 18, 6, 12))

  salvar(p_km / p_risco + plot_layout(heights = c(1, .26)),
         "fig04_kaplan_meier", w = 7.8, h = 5.2)
})

# ---------------------------------------------------------------------------
# 12. Forest plot — seis estudos e a estimativa combinada
#     (simulado: estudos fictícios, OR e IC95%)
# ---------------------------------------------------------------------------
ok({
  d <- data.frame(
    estudo = paste("Estudo", 1:6),
    y = 6:1,
    or = c(0.62, 0.81, 0.55, 0.74, 0.68, 0.59),
    lo = c(0.38, 0.55, 0.31, 0.52, 0.44, 0.36),
    hi = c(1.01, 1.19, 0.97, 1.05, 1.05, 0.96),
    n = c(120, 240, 90, 310, 180, 260)
  )
  diam <- data.frame(x = c(.53, .66, .82, .66), y = c(0, .26, 0, -.26))

  p <- ggplot(d, aes(or, y)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = muted, linewidth = .4) +
    geom_hline(yintercept = .5, linetype = "dotted", colour = "#cbd5e1", linewidth = .4) +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, linewidth = .7, colour = teal_d) +
    geom_point(aes(size = n), colour = teal) +
    geom_polygon(data = diam, aes(x, y), fill = ink, colour = ink, inherit.aes = FALSE) +
    scale_x_log10(breaks = c(.4, .6, .8, 1, 1.2)) +
    scale_y_continuous(breaks = 0:6,
                       labels = c("Combinado", paste("Estudo", 6:1)),
                       limits = c(-.6, 6.6)) +
    scale_size_continuous(range = c(2, 4.6), guide = "none") +
    labs(title = "Forest plot: cada estudo, seu intervalo — e a síntese",
         subtitle = "Losango: estimativa combinada. Quando o IC cruza a linha de 1, não há certeza.",
         x = "Razão de chances (escala log)", y = NULL,
         caption = "Estudos fictícios, dados simulados; tamanho do ponto = n do estudo.") +
    tema() + theme(panel.grid.major.y = element_blank())

  salvar(p, "fig04_forest", w = 7.6, h = 4.5)
})

# ---------------------------------------------------------------------------
# 13. Treemap — hierarquia que cabe na tela (mapa de árvore)
#     (simulado: dispensação por grupo e classe ATC; códigos ATC são públicos)
# ---------------------------------------------------------------------------
ok({
  d <- data.frame(
    grupo = c(rep("N — Sistema nervoso", 3), rep("M — Musculoesquelético", 3),
              rep("C — Cardiovascular", 3), rep("A — Digestivo", 3)),
    classe = c("N02 Analgésicos", "N05 Psicolépticos", "N06 Psicoanal.",
               "M01 Anti-inflam.", "M03 Relaxantes", "M05 Ossos",
               "C09 Anti-hipert.", "C10 Hipolipem.", "C03 Diuréticos",
               "A02 Antiácidos", "A03 Antiespasm.", "A11 Vitaminas"),
    n = c(340, 120, 60, 250, 70, 40, 180, 90, 55, 110, 45, 65)
  )
  png(file.path(outdir, "fig04_treemap_atc.png"), width = 7.6, height = 4.6,
      units = "in", res = 220, type = "cairo", family = "Inter", bg = "white")
  treemap(d, index = c("grupo", "classe"), vSize = "n", type = "index",
          palette = c("#99f6e4", "#5eead4", "#2dd4bf", "#14b8a6", "#0f766e"),
          border.col = "white", border.lwds = c(3.4, 1.5),
          fontsize.labels = c(10.5, 7.6), fontcolor.labels = c("#0b1220", "#0b1220"),
          fontface.labels = c(2, 1),
          align.labels = list(c("left", "top"), c("center", "center")),
          bg.labels = 0, overlap.labels = .35, title = "", title.legend = "",
          fontfamily.labels = "Inter",
          vp = viewport(x = .5, y = .45, width = .965, height = .82))
  grid.text("Dispensações por grupo e classe ATC", x = .03, y = .975, just = c("left", "top"),
            gp = gpar(fontsize = 14.5, fontface = "bold", col = ink, fontfamily = "Inter"))
  grid.text("Área = volume dispensado. Compare retângulos grandes; os pequenos só se leem no rótulo.",
            x = .03, y = .925, just = c("left", "top"),
            gp = gpar(fontsize = 9, col = muted, fontfamily = "Inter"))
  dev.off()
  cat("  ok: fig04_treemap_atc\n")
})

# ---------------------------------------------------------------------------
# 14. Nuvem de palavras — frequência vira tamanho (dados simulados de queixas)
# ---------------------------------------------------------------------------
ok({
  palavras <- data.frame(
    w = c("dor", "inchaço", "febre", "tosse", "cansaço", "náusea", "cefaleia",
          "vertigem", "coceira", "formigamento", "azia", "diarreia", "falta de ar",
          "dor lombar", "insônia", "ansiedade", "palpitação", "ardência", "rigidez",
          "vermelhidão", "perda de apetite", "tontura", "fadiga", "fratura"),
    f = c(96, 71, 64, 58, 55, 41, 39, 33, 30, 27, 24, 22,
          20, 18, 16, 15, 13, 12, 11, 10, 9, 8, 7, 5)
  )
  palavras$dor <- grepl("dor|cefaleia|rigidez|ardência", palavras$w)
  set.seed(4)
  png(file.path(outdir, "fig04_wordcloud.png"), width = 7.6, height = 4.0,
      units = "in", res = 220, type = "cairo", family = "Inter", bg = "white")
  par(mar = c(0, 0, 0, 0))
  plot.new(); par(usr = c(0, 1, 0, 1))
  wordcloud_base <- function(d, cores) {
    ocupados <- list()
    livre <- function(x, y, w, h) {
      if (x - w/2 < 0 || x + w/2 > 1 || y - h/2 < 0 || y + h/2 > 1) return(FALSE)
      for (r in ocupados) if (!(x + w/2 < r[1] || x - w/2 > r[3] || y + h/2 < r[2] || y - h/2 > r[4])) return(FALSE)
      TRUE
    }
    for (i in seq_len(nrow(d))) {
      cex <- 0.62 + 2.35 * (d$f[i] / max(d$f))^0.62
      w <- strwidth(d$w[i], cex = cex, font = 2) * 1.13
      h <- strheight(d$w[i], cex = cex, font = 2) * 1.75
      achou <- FALSE
      for (k in seq_len(2400)) {
        ang <- k * 0.40; r <- 0.0048 * k^0.52
        x <- 0.5 + r * cos(ang) * 1.72; y <- 0.5 + r * sin(ang)
        if (livre(x, y, w, h)) {
          text(x, y, d$w[i], cex = cex, col = if (d$dor[i]) amber else cores[1 + (i %% 3)],
               font = 2, family = "Inter")
          ocupados[[length(ocupados) + 1]] <- c(x - w/2, y - h/2, x + w/2, y + h/2)
          achou <- TRUE; break
        }
      }
    }
  }
  wordcloud_base(palavras, c("#0f766e", "#0d9488", "#115e59"))
  text(.02, .96, "O que os pacientes mais falam", adj = c(0, 1), cex = 1.35, font = 2, col = ink)
  text(.02, .895, "Tamanho = frequência em 240 prontuários simulados. Laranja: queixas de dor.",
       adj = c(0, 1), cex = .78, col = muted)
  dev.off()
  cat("  ok: fig04_wordcloud\n")
})

# ---------------------------------------------------------------------------
# 15. Árvore de palavras (word tree) — o discurso depois de "a dor piora quando"
#     (simulado: 120 respostas abertas)
# ---------------------------------------------------------------------------
ok({
  nos <- data.frame(
    rotulo = c("a dor piora quando", "me movimento", "fico em pé", "está frio",
               "à noite", "ando muito", "subo escada", "carrego peso",
               "durmo mal", "não tomo o remédio", "chega o fim do dia"),
    nivel = c(0, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
    n = c(120, 46, 22, 14, 38, 18, 12, 16, 9, 11, 8),
    x = c(0, .40, .40, .40, .40, .80, .80, .80, .80, .80, .80),
    y = c(.50, .88, .60, .33, .09, .99, .80, .66, .47, .36, .15)
  )
  pai <- c(NA, 1, 1, 1, 1, 2, 2, 3, 3, 4, 5)
  seg <- data.frame(x = nos$x[pai[-1]], xend = nos$x[-1],
                    y = nos$y[pai[-1]], yend = nos$y[-1], n = nos$n[-1])

  p <- ggplot() +
    geom_curve(data = seg, aes(x = x, xend = xend, y = y, yend = yend, linewidth = n),
               colour = "#cbd5e1", curvature = .25, angle = 90, ncp = 14) +
    geom_point(data = nos, aes(x, y), colour = teal, size = 2.7) +
    geom_text(data = nos, aes(x, y, label = rotulo),
              family = "Inter", hjust = -0.09, vjust = -.75,
              fontface = ifelse(nos$nivel == 0, "bold", "plain"),
              colour = ifelse(nos$nivel == 0, ink, body),
              size = ifelse(nos$nivel == 0, 3.8, 3.3)) +
    geom_text(data = nos, aes(x, y, label = paste0("n = ", n)), family = "Inter",
              hjust = -0.10, vjust = 1.9, colour = muted, size = 2.6) +
    scale_linewidth_continuous(range = c(.6, 3.4), guide = "none") +
    coord_cartesian(xlim = c(-.02, 1.32), ylim = c(-.02, 1.06), clip = "off") +
    labs(title = "Árvore de palavras: a frase inteira, ramo por ramo",
         subtitle = "Cada caminho é uma resposta; a espessura do ramo é quantos disseram aquilo.",
         caption = "120 respostas abertas simuladas, todas começando pela mesma expressão.") +
    tema() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   axis.text = element_blank(), axis.title = element_blank(),
                   axis.ticks = element_blank())

  salvar(p, "fig04_wordtree", w = 7.8, h = 4.6)
})

# ---------------------------------------------------------------------------
# 16. Rede de contatos — comunidades, hubs e o caminho da infecção
#     (simulado: 180 pessoas, 5 grupos com laços dentro e entre grupos)
# ---------------------------------------------------------------------------
ok({
  set.seed(12)
  g <- make_empty_graph(n = 180, directed = FALSE)
  grupos <- rep(1:5, length.out = 180)
  for (i in 1:180) {
    dentro <- which(grupos == grupos[i] & seq_len(180) != i)
    alvos <- sample(dentro, sample(2:7, 1), replace = TRUE)
    g <- add_edges(g, as.vector(rbind(i, alvos)))
  }
  g <- add_edges(g, as.vector(rbind(c(4, 47), c(31, 92), c(77, 140),
                                    c(110, 161), c(65, 130), c(12, 155))))
  g <- simplify(g)
  deg <- degree(g)
  set.seed(9)
  lay <- layout_with_fr(g, niter = 900)
  cols <- c("#0d9488", "#d97706", "#7c3aed", "#2563eb", "#dc2626")

  png(file.path(outdir, "fig04_rede_contatos.png"), width = 7.6, height = 4.9,
      units = "in", res = 220, type = "cairo", family = "Inter", bg = "white")
  par(mar = c(0.2, 0.2, 3.4, 0.2), family = "Inter")
  plot(g, layout = lay, rescale = TRUE,
       vertex.color = adjustcolor(cols[grupos], .85),
       vertex.size = 2.0 + 7 * (deg / max(deg)),
       vertex.frame.color = "white", vertex.frame.width = .7,
       edge.color = adjustcolor("#94a3b8", .5), edge.width = .5,
       vertex.label = NA)
  title(main = "Rede de contatos: cinco grupos e alguns pontes", adj = 0,
        cex.main = 1.35, col.main = ink, font.main = 2, line = 2.1)
  mtext("Tamanho do nó = contatos · cor = grupo · linhas entre grupos = caminho da infecção",
        side = 3, adj = 0, line = .55, cex = .60, col = muted, family = "Inter")
  dev.off()
  cat("  ok: fig04_rede_contatos\n")
})

# ---------------------------------------------------------------------------
# 17. Similaridade entre fármacos — mapa de calor + agrupamento
#     (perfis de eventos adversos SIMULADOS: cada classe tem 3 eventos
#      marcadores com frequência em torno de 30% e ruído de fundo de 2%)
# ---------------------------------------------------------------------------
ok({
  farmacos <- c("N02 Dipirona", "N02 Paracetamol", "M01 Ibuprofeno", "M01 Diclofenaco",
                "A02 Omeprazol", "A02 Pantoprazol", "C09 Losartana", "C09 Enalapril",
                "C10 Sinvastatina", "C10 Atorvastatina", "A10 Metformina", "A10 Glibenclamida")
  ea <- c("Náusea", "Cefaleia", "Tontura", "Dor abdominal", "Diarreia", "Constipação",
          "Edema", "Tosse seca", "Mialgia", "Hipotensão", "Sonolência", "Rash")
  marcadores <- list(
    "N02" = c("Tontura", "Rash", "Sonolência"),
    "M01" = c("Dor abdominal", "Edema", "Náusea"),
    "A02" = c("Diarreia", "Cefaleia", "Constipação"),
    "C09" = c("Tosse seca", "Hipotensão", "Tontura"),
    "C10" = c("Mialgia", "Cefaleia", "Náusea"),
    "A10" = c("Diarreia", "Náusea", "Dor abdominal")
  )
  set.seed(21)
  perfis <- t(sapply(farmacos, function(f) {
    v <- pmax(rnorm(length(ea), 4, 2.2), 0.4)
    v[match(marcadores[[sub(" .*", "", f)]], ea)] <- pmax(rnorm(3, 18, 4.5), 6)
    v
  }))
  colnames(perfis) <- ea; rownames(perfis) <- farmacos
  cos_sim <- function(a, b) sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
  S <- outer(1:nrow(perfis), 1:nrow(perfis),
             Vectorize(function(i, j) cos_sim(perfis[i, ], perfis[j, ])))
  dimnames(S) <- list(farmacos, farmacos)
  ord <- hclust(as.dist(1 - S), method = "ward.D2")$order
  S2 <- S[ord, ord]
  diag(S2) <- NA

  df <- expand.grid(f1 = rownames(S2), f2 = colnames(S2), stringsAsFactors = FALSE)
  df$s <- as.vector(S2)
  df$f1 <- factor(df$f1, levels = rev(rownames(S2)))
  df$f2 <- factor(df$f2, levels = colnames(S2))
  df$lab <- ifelse(is.na(df$s) | df$s < .45, "",
                   sub("\\.", ",", sprintf("%.2f", pmin(df$s, .999))))

  p <- ggplot(df, aes(f2, f1, fill = s)) +
    geom_tile(colour = "white", linewidth = .85) +
    geom_text(aes(label = lab), size = 2.3, family = "Inter",
              colour = ifelse(!is.na(df$s) & df$s >= .82, "white", ink)) +
    scale_fill_gradient2(low = "#f0fdfa", mid = "#5eead4", high = "#115e59",
                        midpoint = .55, limits = c(0, 1), na.value = "#e2e8f0",
                        name = "cosseno") +
    labs(title = "Fármacos que se parecem ficam vizinhos",
         subtitle = "Semelhança = cosseno entre os perfis de eventos adversos de cada par.",
         x = NULL, y = NULL,
         caption = paste("Perfis de eventos adversos simulados (12 fármacos, 12 eventos);",
                         "linhas e colunas ordenadas por agrupamento hierárquico.")) +
    tema() + theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 7.4),
                   axis.text.y = element_text(size = 7.4),
                   panel.grid = element_blank(),
                   legend.position = "right", legend.key.height = unit(1.4, "lines"))

  salvar(p, "fig04_similaridade_farmacos", w = 8.4, h = 6.2)
})
