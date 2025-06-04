#!./bin/bash
if [ -f .env ]; then
  export $(grep -v '^#' .env | xargs)
fi

read -p "Enter new database name: " DB_NAME

CONTAINER_NAME=${CONTAINER_NAME:-runestone}

docker exec -i "$CONTAINER_NAME" psql -U "$PGUSER" -d postgres -c "CREATE DATABASE \"$DB_NAME\";"

if [ $? -eq 0 ]; then
  echo "Database '$DB_NAME' created successfully."
else
  echo "Failed to create database '$DB_NAME'."
fi