if [ -f .env ]; then
  export $(grep -v '^#' .env | xargs)
fi

read -p "Enter database name to connect: " DB_NAME

CONTAINER_NAME=${CONTAINER_NAME}

docker exec -it "$CONTAINER_NAME" psql -U "$PGUSER" -d "$DB_NAME"