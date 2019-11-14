# The following variables are predefined:
# CURRENT_FILE      # full path and file name
# CURRENT_FILE_NAME # file name only
# ORIGINAL_FILE     # original file and full path
# OUT               # folder to store results in

# Define log file:
LOG_FILE="$(echo "$OUT/log_${CURRENT_FILE_NAME}")"

# Write command in log:
echo "`date +"%F %T"`	PWD=$PWD	$CURRENT_FILE $@" >> "$LOG_FILE"

# Execute original command:
"$ORIGINAL_FILE" "$@"
