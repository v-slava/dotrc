SKIP_NEXT=false
for arg in ${@:2} ; do # always skip first cmd line argument
    if [ "$SKIP_NEXT" = "true" ]; then
        SKIP_NEXT=false
        continue
    fi
    case "$arg" in
        -l | -L | -o)
            SKIP_NEXT=true
            ;;
        -l* | -L* | -o*)
            ;;
        *)
            CMD+=("$arg")
            ;;
    esac
done
