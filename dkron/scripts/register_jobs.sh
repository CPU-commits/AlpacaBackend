#!/bin/sh
set -e

DKRON_URL="${DKRON_URL:-http://dkron:8080}"
# Espacios separados si quieres esperar varios endpoints
APP_WAIT_URLS="${APP_WAIT_URLS:-http://app:8000/healthz}"
# Tiempo máximo de espera por URL (segundos). 0 = infinito.
APP_WAIT_TIMEOUT="${APP_WAIT_TIMEOUT:-0}"

echo "Esperando a Dkron en ${DKRON_URL} ..."
until curl -sf "${DKRON_URL}/v1/" >/dev/null; do
  echo "  - Dkron aún no responde, reintentando..."
  sleep 2
done
echo "Dkron OK."

# Espera a que la(s) URL(s) de la app respondan 2xx/3xx
for url in $APP_WAIT_URLS; do
  echo "Esperando a que la app responda en ${url} ..."
  start=$(date +%s)
  while ! curl -sf "$url" >/dev/null 2>&1; do
    now=$(date +%s)
    if [ "$APP_WAIT_TIMEOUT" -gt 0 ] && [ $((now-start)) -ge "$APP_WAIT_TIMEOUT" ]; then
      echo "  - Timeout esperando ${url} (${APP_WAIT_TIMEOUT}s). Continúo igualmente."
      break
    fi
    echo "  - Aún no responde, reintentando..."
    sleep 2
  done
  echo "OK (o timeout) para ${url}."
done

echo "Registrando jobs…"

# Si no hay archivos, salir limpio
set -- /jobs/*.json
if [ "$1" = "/jobs/*.json" ]; then
  echo "No se encontraron archivos de job en /jobs"
  exit 0
fi

for f in "$@"; do
  echo "────────────────────────────────────────"
  echo "Procesando: ${f##*/}"
  cp "$f" /tmp/job.json

  code=$(curl -s -o /tmp/resp.txt -w "%{http_code}" \
    -H "Content-Type: application/json" \
    -X POST "${DKRON_URL}/v1/jobs" \
    --data @/tmp/job.json || true)

  if [ "$code" -ge 200 ] && [ "$code" -lt 300 ]; then
    echo "Creado OK (${code})"
    cat /tmp/resp.txt; echo
    continue
  fi

  echo "POST devolvió ${code}. Intentando actualizar (PUT)…"
  name=$(jq -r '.name // empty' /tmp/job.json)
  if [ -z "$name" ]; then
    echo "ERROR: el JSON no tiene .name. Saltando."
    continue
  fi

  code=$(curl -s -o /tmp/resp.txt -w "%{http_code}" \
    -H "Content-Type: application/json" \
    -X PUT "${DKRON_URL}/v1/jobs/${name}" \
    --data @/tmp/job.json || true)

  if [ "$code" -ge 200 ] && [ "$code" -lt 300 ]; then
    echo "Actualizado OK (${code})"
    cat /tmp/resp.txt; echo
  else
    echo "ERROR: PUT devolvió ${code}"
    cat /tmp/resp.txt; echo
  fi
done

echo "────────────────────────────────────────"
echo "Registro de jobs finalizado."
