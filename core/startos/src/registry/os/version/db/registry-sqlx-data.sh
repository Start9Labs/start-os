#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
TMP_DIR=$(mktemp -d)
mkdir $TMP_DIR/pgdata
docker run -d --rm --name=tmp_postgres -e POSTGRES_PASSWORD=password -v $TMP_DIR/pgdata:/var/lib/postgresql/data postgres

(
    set -e
    ctr=0
    until docker exec tmp_postgres psql -U postgres || [ $ctr -ge 5 ]; do
    	ctr=$[ctr + 1]
    	sleep 5;
    done
    
    PG_IP=$(docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' tmp_postgres)

    SCHEMA_DUMP="registry_schema.sql"
    
    DATABASE_URL=postgres://postgres:password@$PG_IP/postgres
    psql $DATABASE_URL -f "$SCRIPT_DIR/$SCHEMA_DUMP"
    DATABASE_URL=postgres://postgres:password@$PG_IP/postgres PLATFORM=$(uname -m) cargo sqlx prepare -- --lib --profile=test --workspace
    echo "Subscript Complete"
)

docker stop tmp_postgres
sudo rm -rf $TMP_DIR
