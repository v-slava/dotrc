#!/usr/bin/env bash

if [ -z "$1" ]; then
	echo "Usage: $(basename $0) \"COMMAND_TO_RUN\"" 1>&2
	exit 1
fi
COMMAND_TO_RUN="$1"

VIFM_RESET_COLORS='\x1b[0m'
VIFM_COMMAND_PREFIX='\x1b[34m'
VIFM_OUTPUT_PREFIX='\x1b[33m'
VIFM_EXIT_CODE_SUCCESS='\x1b[1;32m'
VIFM_EXIT_CODE_FAIL='\x1b[1;31m'
VIFM_END='\x1b[36m'

echo -e "${VIFM_COMMAND_PREFIX}Command:${VIFM_RESET_COLORS}\n${COMMAND_TO_RUN}\n${VIFM_OUTPUT_PREFIX}Command output:${VIFM_RESET_COLORS}"

${COMMAND_TO_RUN}
EXIT_CODE=$?
if [ $EXIT_CODE -eq 0 ]; then
	echo -e "${VIFM_EXIT_CODE_SUCCESS}Command succeeded (exit code = $EXIT_CODE)"
else
	echo -e "${VIFM_EXIT_CODE_FAIL}Command failed (exit code = $EXIT_CODE)"
fi
echo -en "${VIFM_END}"

