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

# Descobre o executável do Quarto (funciona no RStudio e no terminal)
QUARTO_BIN="${QUARTO_BIN_PATH:+$QUARTO_BIN_PATH/quarto}"
if [ ! -x "$QUARTO_BIN" ]; then
  QUARTO_BIN="$(command -v quarto || true)"
fi
if [ -z "$QUARTO_BIN" ] || [ ! -x "$QUARTO_BIN" ]; then
  echo "ERRO: não encontrei o executável do Quarto no PATH." >&2
  exit 1
fi

echo "→ Compilando apresentações em slides/ …"

# O projeto de slides fica FORA da árvore de output do livro, então o quarto
# não consegue limpar docs/slides sozinho. Limpamos aqui para evitar CSS/JS
# antigos acumulados.
if [ -d docs/slides ]; then
  rm -rf docs/slides
fi

# Variáveis do post-render do projeto pai podem confundir o subprojeto
env -u QUARTO_PROJECT_DIR \
    -u QUARTO_PROJECT_OUTPUT_DIR \
    -u QUARTO_PROJECT_RENDER_ALL \
    -u QUARTO_PROJECT_RENDER_ALL_FORMATS \
    "$QUARTO_BIN" render slides

echo "✓ Apresentações em docs/slides/"
