#!/bin/bash

# i3-dmenu-desktop --dmenu='dmenu -i -fn Inconsolata LGC-16:monospace' --entry-type=command
# exec dmenu_run -fn 'Inconsolata LGC-16:monospace'

# The following implementation avoids excessive bash process:
CMD="$(dmenu_path | dmenu -fn 'Inconsolata LGC-16:monospace' "$@")"
if [ $? -eq 0 ]; then
    exec "$CMD"
fi
