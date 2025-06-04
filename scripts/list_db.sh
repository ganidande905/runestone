if [ -f .env ]; then
  export $(grep -v '^#' .env | xargs)
fi

CONTAINER_NAME=${CONTAINER_NAME}

docker exec -i "$CONTAINER_NAME" psql -U "$PGUSER" -d postgres -c "\l"