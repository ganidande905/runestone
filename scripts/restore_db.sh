#!./bin/bash
if [ -f .env ]; then
  export $(grep -v '^#' .env | xargs)
fi

read -p "Enter target database name: " DB_NAME
read -p "Enter path to .sql file: " DUMP_FILE

DUMP_FILE=$(eval echo "$DUMP_FILE")

if [ ! -f "$DUMP_FILE" ]; then
  echo "File not found: $DUMP_FILE"
  exit 1
fi
BASENAME=$(basename "$DUMP_FILE")
docker cp "$DUMP_FILE" "$CONTAINER_NAME:/backups/$BASENAME"

docker exec -i "$CONTAINER_NAME" psql -U "$PGUSER" -d "$DB_NAME" -f "/backups/$BASENAME"

if [ $? -eq 0 ]; then
  echo "Restored into $DB_NAME from $DUMP_FILE"
else
  echo "Restore failed"
fi