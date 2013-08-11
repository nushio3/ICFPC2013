#!/bin/bash
# One line explanation of health-solver-server.sh.
N=0
cat ec2-instances | while read SERVER; do
    SV="$(echo "$SERVER" | perl -ne 'chomp;chop; print "$_\n";')"
    echo "server $N: $(curl "http://${SV}:31940/" 2>/dev/null)"
    N=$((N + 1))
done
