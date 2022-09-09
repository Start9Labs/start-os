#!/bin/bash

function flatline {
	echo -n "0" > /sys/class/pwm/pwmchip0/export
	sleep 0.5
	echo -n "2272727" > /sys/class/pwm/pwmchip0/pwm0/period
	echo -n "1136364" > /sys/class/pwm/pwmchip0/pwm0/duty_cycle
	echo -n "1" > /sys/class/pwm/pwmchip0/pwm0/enable
}

initialization.sh 
STATUS=$?
if [ $STATUS -ne 0 ]; then
	flatline
	exit $STATUS
fi