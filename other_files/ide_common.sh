if [ $# -lt 1 ]; then
	echo "$USAGE" 1>&2
	exit 1
fi
IDE_DIR="$1"
shift
BUILD_LOG="$IDE_DIR/build_log"
set -e

