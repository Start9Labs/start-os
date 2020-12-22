#!/bin/bash
set -e

#echo "turn off mocks"
#echo "$( jq '.useMocks = false' use-mocks.json )" > use-mocks.json

echo "FILTER: rm -rf www"
rm -rf www

echo "FILTER: ionic build"
npm run build-prod

echo "FILTER: ssh + rm -rf /var/www/html/start9-ambassador/"
ssh root@start9-$1.local "rm -rf /var/www/html/start9-ambassador"

echo "FILTER: tar -zcvf ambassador.tar.gz www"
rm -rf start9-ambassador
mv www start9-ambassador
tar -zcvf ambassador.tar.gz start9-ambassador

echo "FILTER: scp ambassador.tar.gz root@start9-def09913.local:/root"
scp ambassador.tar.gz root@start9-$1.local:/root/agent

echo "FILTER: ssh root@start9-$1.local:/root 1"
ssh root@start9-$1.local "cd /root/agent && tar -C /var/www/html/ -xvf ambassador.tar.gz"

echo "FILTER: ssh root@start9-$1.local:/root 2"
ssh root@start9-$1.local "systemctl restart nginx"

echo "FILTER: fin"
