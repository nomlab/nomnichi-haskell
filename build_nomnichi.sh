#!/bin/sh

oldpid=$(netstat -a -p | awk '/:54321/{sub("/nomnichi","",$NF); print $NF}' | uniq)

if [ -n "$oldpid" ]; then
    echo "kill $oldpid"
    kill $oldpid 
fi

set +x

cabal-dev clean && cabal-dev configure && cabal-dev build && \
  nohup ./dist/build/nomnichi/nomnichi Production&
