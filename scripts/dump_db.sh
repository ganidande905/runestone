#!./bin/bash
if [ -f .env ]; then
  export $(grep -v '^#' .env | xargs)
fi


read -p "Enter database name to dump: " DB_NAME

TIMESTAMP=$(date +"%Y-%m-%d_%H-%M-%S")
FILENAME="${DB_NAME}_${TIMESTAMP}.sql"

read -p "Enter path to save dump (leave blank for default): " CUSTOM_PATH
DEFAULT_DUMP_DIR=${DEFAULT_DUMP_DIR:-./backups}
LOCAL_PATH=${CUSTOM_PATH:-$DEFAULT_DUMP_DIR/$FILENAME}

mkdir -p "$(dirname "$LOCAL_PATH")"

CONTAINER_PATH="/backups/$FILENAME"

docker exec -i "$CONTAINER_NAME" pg_dump -U "$PGUSER" -d "$DB_NAME" -f "$CONTAINER_PATH"

docker cp "$CONTAINER_NAME:$CONTAINER_PATH" "$LOCAL_PATH"

if [ $? -eq 0 ]; then
  echo "Dump saved to $LOCAL_PATH"
else
  echo "Failed to dump database '$DB_NAME'"
fi