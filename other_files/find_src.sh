#!/bin/bash

find "$(realpath "$PWD")" -type f -name '*.[cChHsSi]' -o -name '*.c[cp]' -o -name '*.[ch]xx' -o -name '*.[ch]pp' -o -name '*.[CH]PP' -o -name '*.[ch]++' -o -name '*.tcc' -o -name '*.ii' -o -name '*.sx' -o -name '*.h[hp]'

# if [ -d "./arch" ]; then
#     # Assume this is linux kernel. Generate list of files to be indexed:
#     find .                                     \
#             -path "./tmp/*" -prune -o              \
#             -path "./Documentation/*" -prune -o    \
#             -path "./scripts/*" -prune -o          \
#             -path "./arch/*" -prune -o             \
#             -type f -name "*.[chxsS]" -print > $FILES_LIST
#     find ./arch/arm                            \
#             -type f -name "*.[chxsS]" -print >> $FILES_LIST
# fi
