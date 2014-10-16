#!/bin/sh
cp nomnichi_production.sqlite3 nomnichi_production.sqlite3.bak
cp nomnichi_production.sqlite3 nomnichi.sqlite3
cabal exec runhaskell db_migrate_first_20141016.hs
cabal exec runhaskell db_migrate_second_20141016.hs
rm temp_nomnichi.sqlite3
mv new_nomnichi.sqlite3 nomnichi_production.sqlite3
