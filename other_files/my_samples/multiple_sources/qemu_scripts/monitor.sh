#!/bin/sh

cd "$(dirname "$0")" || exit 1

CTRL_SOCKET="${PWD}/ctrl.sock"

exec nc -U "${CTRL_SOCKET}"
