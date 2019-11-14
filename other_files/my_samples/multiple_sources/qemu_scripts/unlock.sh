#!/bin/bash

cd "$(dirname "$0")" || exit 1

PASS_FILE="${HOME}/.samsung.conf"
HOLD_MS=50

type gawk &>/dev/null || { echo "need gawk" && exit 1; }
[ -e "${PASS_FILE}" ] || { echo "can't read '${PASS_FILE}'" && exit 1; }

sendkey()
{
	echo -en "$@" | ./monitor.sh 1>/dev/null
}

get_pass()
{
	cat "${PASS_FILE}"  | \
		gawk -F'=' -v edge="['\"]" -v apostrophe="'" '
			BEGIN {
				map[" "] = "spc"
				map["!"] = "shift-1"
				map["@"] = "shift-2"
				map["#"] = "shift-3"
				map["$"] = "shift-4"
				map["%"] = "shift-5"
				map["^"] = "shift-6"
				map["&"] = "shift-7"
				map["*"] = "shift-8"
				map["("] = "shift-9"
				map[")"] = "shift-0"
				map["/"] = "slash"
				map["\\"] = "less"
				map["*"] = "asterisk"
				map["="] = "equal"
				map["-"] = "minus"
				map["+"] = "kp_add"
				map[";"] = "semicolon"
				map[":"] = "shift-semicolon"
				map["="] = "equal"
				map["."] = "dot"
				map[","] = "comma"
				map["<"] = "shift-comma"
				map[">"] = "shift-dot"
				map[apostrophe] = "apostrophe"
				map["`"] = "grave_accent"
				map["/"] = "kp_divide"
				map["["] = "bracket_left"
				map["]"] = "bracket_right"
			}
			/^\s*[^#\s]/ && $1~/\s*password\s*/ {
				s=$2
				gsub(/^\s+/, "", s);
				gsub(/\s$/, "", s);
				pat1="^\\s*" edge "(.+?)" edge "\\s*";
				pat2="^\\s*" "(.+?)" "\\s*$";
				if (substr(s, 1, 1) == substr(s, length(s), 1) && substr(s, 1, 1) ~ edge)
					pat=pat1
				else
					pat=pat2
				pass=gensub(pat, "\\1", "g", s);
				split(pass, a, //);
				for (i=1; i <= length(pass); i++)
					if (a[i] in map)
						print map[a[i]]
					else
						print a[i]
			}
			'
}

keys="meta_l-l=100 esc=100 ctrl-alt-delete=100 $(get_pass) ret"
to_send=""
for k in ${keys}; do
	key=${k%=*}
	ms=${k#*=}
	[ "${ms}" = "${k}" ] && ms=${HOLD_MS}
	to_send="${to_send}sendkey $key ${ms}\n"
done
sendkey "${to_send}"

./spice.sh || ./run.sh
