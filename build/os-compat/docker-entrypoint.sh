#!/bin/bash

exec /lib/systemd/systemd --unit=multi-user.target --show-status=false --log-target=journal
