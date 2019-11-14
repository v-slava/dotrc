ORIGINAL_FILE=(
    /home/user/arm-eabi-gcc
    /home/user/arm-eabi-g++
)
# /usr/bin/gcc

TMP_FILE=/tmp/overloading_errexit

# If LAST_INDEX=0, then let fails, and if "set -e" was used, all interrupts.
# To avoid such behavior we need save current "set +/-e", issue "set +e" and restore "set +/-e".
# Save "set -e" as 'set -o errexit', "set +e" as 'set +o errexit':
set +o | grep errexit > $TMP_FILE
chmod 666 $TMP_FILE

set +e
let "LAST_INDEX = ${#ORIGINAL_FILE[@]} - 1"

# Restore "set +/-e":
. $TMP_FILE

for i in $(seq 0 $LAST_INDEX); do
    SAVED_FILE[$i]=$(echo "${ORIGINAL_FILE[$i]}_saved")
done
