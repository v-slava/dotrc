#!/bin/bash

# Usage example:
# $0 dir_2 $dir_3 -type d -path $PWD/excluded_dir_1 -prune -o -path $dir_3/excluded_dir_2 -prune -o

find "$(realpath "$PWD")" $@ \( -type f -name '*.[cChHsSi]' -o -name '*.c[cp]' -o -name '*.[ch]xx' -o -name '*.[ch]pp' -o -name '*.[CH]PP' -o -name '*.[ch]++' -o -name '*.tcc' -o -name '*.ii' -o -name '*.sx' -o -name '*.h[hp]' \) -print

# | tee /tmp/find_src_sh_results

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
