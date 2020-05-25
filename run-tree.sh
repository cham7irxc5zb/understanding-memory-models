#!/bin/bash
set -o errexit -o pipefail

export LANG=C.utf-8 
export LC_ALL=C.utf-8 
unset LC_CTYPE LC_NUMERIC LC_TIME LC_COLLATE LC_MONETARY LC_MESSAGES LC_PAPER LC_NAME LC_ADDRESS LC_TELEPHONE LC_MEASUREMENT LC_IDENTIFICATION
i=0
while true; do
	mkdir -p run-test
	python3 ./gen-tree.py || exit 1
	let i+=1
	echo "Test $i"
	prev=
	for sct in 0 10 20 30 40 50 60 70 80 90 100; do
		curr="run-test/o-$sct"
		sed "s/@SCT@/$sct/g" < run-test/template > run-test/cur.tlang
		./target/release/umm < run-test/cur.tlang 2> run-test/log > run-test/o
		sort < run-test/o > "$curr"

		lines="$(wc -l "$curr")"
		lines="${lines%% *}"
		if [[ "$lines" == 0 ]]; then
			echo FAIL no output
			exit 1
		fi

		if [[ -n "$prev" ]] && [[ -n "$(comm -13 "$prev" "$curr")" ]]; then
			echo FAIL not subset
			exit 1
		fi
		prev="$curr"
	done
	if ! diff run-test/o-100 run-test/o-sc; then
		echo FAIL sc
		exit 1
	fi
done
