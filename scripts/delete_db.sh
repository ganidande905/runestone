#!./bin/bash
if [ -f .env ]; then
  export $(grep -v '^#' .env | xargs)
fi

read -p "Enter database name to DELETE: " DB_NAME

read -p "Are you sure you want to delete '$DB_NAME'? This cannot be undone. (y/n): " confirm
if [[ "$confirm" != "y" && "$confirm" != "Y" ]]; then
  echo "Cancelled."
  exit 0
fi

CONTAINER_NAME=${CONTAINER_NAME:-runestone}

docker exec -i "$CONTAINER_NAME" psql -U "$PGUSER" -d postgres -c "DROP DATABASE IF EXISTS \"$DB_NAME\";"

if [ $? -eq 0 ]; then
  echo "Dropped database '$DB_NAME'"
else
  echo "Failed to drop database '$DB_NAME'"
fi