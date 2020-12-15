#!/bin/bash

cat config/settings.yml | grep app-mgr-version-spec
cat package.yaml | grep version

stack --local-bin-path ./dist build --copy-bins #--flag start9-agent:disable-auth
