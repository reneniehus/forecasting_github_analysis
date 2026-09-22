#!/usr/bin/env bash
# ==============================================================================
# Weekly refresh: pull the live hubs, recompute MSPs, redraw every figure.
# ==============================================================================
# Rounds close Wednesday 23:59 CET and the ensemble runs the same night, so any run
# from Thursday onwards picks up the current week. ERVISS publishes Fridays, so a
# Friday-evening or weekend run gets both the newest forecast round and the newest
# surveillance week.
#
#   ./refresh-msp.sh              pull, recompute, redraw
#   ./refresh-msp.sh --no-pull    recompute and redraw from the clones as they are
#
# Clone locations come from the same env vars the R scripts use, so overriding one
# overrides it everywhere:
#   SYNDROMIC_DIR  FLU_DIR  ARI_DIR  COVID_DIR  COVID_ARCHIVE_DIR
set -euo pipefail
cd "$(dirname "$0")"

PULL=1
[ "${1:-}" = "--no-pull" ] && PULL=0

SYNDROMIC_DIR="${SYNDROMIC_DIR:-/workspace/emh-syndromic}"
COVID_DIR="${COVID_DIR:-/workspace/emh-covid}"
FLU_DIR="${FLU_DIR:-/workspace/emh-flu-forecast-hub_archive}"
ARI_DIR="${ARI_DIR:-/workspace/emh-ari-forecast-hub_archive}"
COVID_ARCHIVE_DIR="${COVID_ARCHIVE_DIR:-/workspace/emh-covid-archive}"
export SYNDROMIC_DIR COVID_DIR FLU_DIR ARI_DIR COVID_ARCHIVE_DIR

for d in "$SYNDROMIC_DIR" "$COVID_DIR" "$FLU_DIR" "$ARI_DIR" "$COVID_ARCHIVE_DIR"; do
  [ -d "$d/.git" ] || { echo "missing clone: $d  (run ./fetch-hubs.sh)"; exit 1; }
done

# Only the two live hubs move; the three archives are frozen and never need pulling.
if [ "$PULL" = 1 ]; then
  for d in "$SYNDROMIC_DIR" "$COVID_DIR"; do
    echo "== pulling $(basename "$d")"
    for attempt in 1 2 3 4; do
      git -C "$d" pull --ff-only --quiet && break
      echo "   pull failed (attempt $attempt), retrying"; sleep $((2 ** attempt))
    done
  done
fi

echo
echo "== latest round on disk"
for d in "$SYNDROMIC_DIR" "$COVID_DIR"; do
  printf '   %-14s %s\n' "$(basename "$d")" \
    "$(ls "$d/model-output/respicast-hubEnsemble" | tail -1 | cut -c1-10)"
done

echo
Rscript code/03_hubs/compute_msp.R
Rscript code/05_figures/fig_msp_ili.R
for ind in "ILI incidence" "ARI incidence" "COVID-19 hospitalisations"; do
  INDICATOR="$ind" Rscript code/05_figures/fig_msp_grid.R            # 500 ppi print master
  INDICATOR="$ind" DPI=150 Rscript code/05_figures/fig_msp_grid.R >/dev/null   # screen copy
done

echo
echo "done. Refreshed:"
echo "   output/msp_weekly.csv"
ls -1 output/figures/msp_*.png | sed 's/^/   /'
