#!/bin/sh

format="cmd=%C; real=%E; user=%U; sys=%S; resident=%M"

title=$1; shift

out=performance.dat

psout=`mktemp -t time-run-ps-XXXXXX`
timeout=`mktemp -t time-run-XXXXXX`
trap "rm -f $psout $timeout" EXIT


(
    sleep 2
    # print ps field names
    ps xl  | egrep '(UID)' | grep -v grep >> $psout
    while pgrep sfdlc >/dev/null; do
	 ps xl  | grep 'sfdlc' | grep -v grep >> $psout
	 sleep 5
    done
) &


/usr/bin/time -o $timeout "$@"

cat >>$out <<EOF

<run title="$title" date="$(date)">

<uname>$(uname -a)</uname>

<cpuinfo>
$(egrep 'model name|cpu MHz' < /proc/cpuinfo)
</cpuinfo>

<meminfo>
$(egrep 'MemTotal|SwapTotal' < /proc/meminfo)
</meminfo>

<gcstat>
$(cat sfdlc.stat)
</gcstat>

<psout>
$(cat $psout)
</psout>

<timeout>
$(cat $timeout)
</timeout>

</run>

EOF
