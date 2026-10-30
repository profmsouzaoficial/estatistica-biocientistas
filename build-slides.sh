#!/usr/bin/env bash
# =============================================================================
#  build-slides.sh — compila as apresentações (reveal.js) para docs/slides/
# -----------------------------------------------------------------------------
#  É chamado automaticamente pelo `post-render` do livro (ver _quarto.yml),
#  de modo que `quarto render` produz o livro E os slides de uma só vez.
#
#  Uso manual:  ./build-slides.sh
# =============================================================================
set -euo pipefail

cd "$(dirname "$0")"

# --- Encontra o executável do Quarto ----------------------------------------
# Prioriza o Quarto que chamou este script (post-render define QUARTO_BIN_PATH),
# depois o PATH e, por fim, instalações conhecidas (RStudio, ~/.local, /opt).
find_quarto() {
  local c
  for c in \
      "${QUARTO_BIN_PATH:-}/quarto" \
      "$(command -v quarto 2>/dev/null || true)" \
      "/usr/lib/rstudio/resources/app/bin/quarto/bin/quarto" \
      "$HOME/.local/opt/quarto/bin/quarto" \
      "/opt/quarto/bin/quarto" \
      "/usr/local/bin/quarto"
  do
    if [ -n "$c" ] && [ -x "$c" ]; then
      printf '%s\n' "$c"
      return 0
    fi
  done
  return 1
}

QUARTO_BIN="$(find_quarto)" || {
  echo "ERRO: não encontrei o executável do Quarto." >&2
  echo "      Instale o Quarto ou adicione-o ao PATH." >&2
  exit 1
}

echo "→ Compilando apresentações (Quarto $("$QUARTO_BIN" --version)) …"

# O projeto de slides escreve FORA da árvore do projeto do livro, então o Quarto
# não consegue limpar docs/slides sozinho. Limpamos aqui para não acumular
# CSS/JS de compilações antigas.
rm -rf docs/slides

# As variáveis do post-render do projeto pai confundiriam o subprojeto.
env -u QUARTO_PROJECT_DIR \
    -u QUARTO_PROJECT_OUTPUT_DIR \
    -u QUARTO_PROJECT_OUTPUT_FILES \
    -u QUARTO_PROJECT_ROOT \
    -u QUARTO_PROJECT_RENDER_ALL \
    -u QUARTO_PROJECT_RENDER_ALL_FORMATS \
    "$QUARTO_BIN" render slides

echo "✓ Apresentações publicadas em docs/slides/"
