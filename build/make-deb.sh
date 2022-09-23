#!/bin/bash

set -e

DEB_BUILD_OPTIONS="parallel=1" debuild --preserve-envvar USER --preserve-envvar PATH --no-lintian -i -I