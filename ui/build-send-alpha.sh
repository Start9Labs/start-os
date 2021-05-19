#!/bin/bash
set -e

echo "turn off mocks"
echo "$( jq '.useMocks = false' use-mocks.json )" > use-mocks.json
echo "$( jq '.skipStartupAlerts = false' use-mocks.json )" > use-mocks.json

echo "FILTER: rm -rf www"
rm -rf www

echo "FILTER: ionic build"
npm run build-prod

echo "FILTER: cp client-manifest.yaml www"
cp client-manifest.yaml www

echo "FILTER: git hash"
touch git-hash.txt
git log | head -n1 > git-hash.txt
mv git-hash.txt www

echo "FILTER: removing mock icons"
rm -rf www/assets/img/service-icons

echo "FILTER: tar -zcvf ambassador-ui.tar.gz www"
tar -zcvf ambassador-ui.tar.gz www

SHA_SUM=$(sha1sum ambassador-ui.tar.gz)
echo "${SHA_SUM}"

echo "Set version"
VERSION=$(jq ".version" package.json)
echo "${VERSION}"

echo "FILTER: mkdir alpha-reg"
ssh root@alpha-registry.start9labs.com "mkdir -p /var/www/html/resources/sys/ambassador-ui.tar.gz/${VERSION}"

echo "FILTER: scp ambassador-ui.tar.gz"
scp ambassador-ui.tar.gz root@alpha-registry.start9labs.com:/var/www/html/resources/sys/ambassador-ui.tar.gz/${VERSION}/ambassador-ui.tar.gz

echo "FILTER: fin"
