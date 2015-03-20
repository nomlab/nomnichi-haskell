#!/bin/sh

oldpid=$(netstat -a -p | awk '/:54321/{sub("/nomnichi","",$NF); print $NF}' | uniq)

if [ -n "$oldpid" ]; then
    echo "kill $oldpid"
    kill $oldpid 
fi

set +x

cabal clean && cabal configure && cabal build && \
  nohup ./dist/build/nomnichi/nomnichi Production&
