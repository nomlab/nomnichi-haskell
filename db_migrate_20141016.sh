#!/bin/sh

# 本スクリプトは，nomnichi_production.sqlite3のmigrateを行うスクリプトである．
# 具体的には，ArticleのmemberName(Text)とCommentのcommenter(Text)をuser(UserId)に
# 変更したDBを作成し，nomnichi_production.sqlite3と置き換える．
# Create db_migrate_20141016のコミット(4927682b1fd2e82f8080765dec79c63a32eddc15)を
# 取り込んだ後，本スクリプトを実行する．

cp nomnichi_production.sqlite3 nomnichi_production.sqlite3.bak
cp nomnichi_production.sqlite3 nomnichi.sqlite3
runhaskell -package-db=.cabal-sandbox/i386-linux-ghc-7.6.3-packages.conf.d/ db_migrate_first_20141016.hs
runhaskell -package-db=.cabal-sandbox/i386-linux-ghc-7.6.3-packages.conf.d/ db_migrate_second_20141016.hs
rm temp_nomnichi.sqlite3
mv new_nomnichi.sqlite3 nomnichi_production.sqlite3
