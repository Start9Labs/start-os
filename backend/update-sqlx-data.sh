#!/bin/bash

TMP_DIR=$(mktemp -d)
mkdir $TMP_DIR/pgdata
docker run -d --rm --name=tmp_postgres -e POSTGRES_PASSWORD=password -v $TMP_DIR/pgdata:/var/lib/postgresql/data postgres

(
    set -e
    ctr=0
    while ! docker exec tmp_postgres psql -U postgres || [ $ctr -lt 5 ]; do
    	ctr=$[ctr + 1]
    	sleep 1;
    done
    
    PG_IP=$(docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' tmp_postgres)
    
    DATABASE_URL=postgres://postgres:password@$PG_IP/postgres cargo sqlx migrate run
    DATABASE_URL=postgres://postgres:password@$PG_IP/postgres cargo sqlx prepare -- --lib --profile=test
)

docker stop tmp_postgres
sudo rm -rf $TMP_DIR
