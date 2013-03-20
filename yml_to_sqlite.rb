# -*- coding: utf-8 -*-

=begin

-- このプログラムについて
YAML形式のノムニチをyesodで扱えるsqlite3に登録する．
Ruby1.9.3でのみ動作確認した．

このプログラムでは，テーブルの作成は行わない．
このため，以下の手順は，ノムニチを一度起動し，テーブルが作成済みであることが前提となる．

-- 使用方法
nomnichi.sqlite3 と同じ階層に，yml_to_sqlite.rb と nomnichi.yaml を置く．
以下のコマンドを実行することで，nomnichi.sqlite3 へデータが登録される．
$ ruby yml_to_sqlite.rb
記事1つにつき，ドットが1つ画面に表示される．

nomnichi.yaml はnomnichi.yaml.sample を参考に記述されたい．

=end

require 'sqlite3'
require 'yaml'

def into_article_table(yaml_data)
  last_id = serach_biggest_id("article")
  number_of_article = 0
  @nomnichi_db.transaction do
    yaml_data.each do |d|
      last_id += 1
      number_of_article += 1
      article_sql = <<SQL
insert into article values ( #{last_id.to_s}, "#{d["member_name"]}", "#{replace_escape_character(d["title"])}", "#{d["perma_link"]}", "#{d["content"]}", "#{d["created_on"]}", "#{d["updated_on"]}", "#{d["published_on"]}", #{d["approved"].to_i}, #{d["count"].to_i}, #{d["promote_headline"].to_i});
SQL
      @nomnichi_db.execute(article_sql)
      print '.'
      d["comment"].each do |c|
        comment_sql = <<SQL
insert into comment(commenter, body, created_at, updated_at, article_id) values ( "#{c["commenter"]}", "#{c["body"]}", "#{c["created_at"]}", "#{c["updated_at"]}", #{last_id});
SQL
        @nomnichi_db.execute(comment_sql)
# insert into comment(commenter, body, created_at, update_at, article_id) values ( "murata", "コメント投稿できましたか？", "2013-03-17 20:50:02 +0900", "2013-03-17 20:50:02 +0900", 5);
      end if d["comment"]
    end
  end
  print "\nProbably registration has been succeeded. (" +
    number_of_article.to_s + " articles)\n"
end

def replace_escape_character(title)
  return title.gsub('\[','[').gsub('\]',']')
end

def serach_biggest_id(table_name)
  max = 0
  sql = "SELECT * FROM " + table_name + ";"
  @nomnichi_db.execute(sql) do |row|
    max = row[0] if row[0] > max
  end
  return max
end

@nomnichi_db = SQLite3::Database.new("nomnichi.sqlite3")
yaml_data = YAML.load_file('./nomnichi.yaml')
into_article_table(yaml_data)
@nomnichi_db.close




# 以降はメモのみ

=begin
Yesodの型とSqliteの型は以下のような関係にある．
VARCHAR

<< article Table >>
                     : "id" INTEGER PRIMARY KEY
memberName Text      : "member_name" VARCHAR NOT NULL
title Text           : "title" VARCHAR NOT NULL
permaLink Text       : "perma_link" VARCHAR NOT NULL
content Html         : "content" VARCHAR NOT NULL
createdOn UTCTime    : "created_on" TIMESTAMP NOT NULL
updatedOn UTCTime    : "updated_on" TIMESTAMP NOT NULL
publishedOn UTCTime  : "published_on" TIMESTAMP NOT NULL
approved Bool        : "approved" BOOLEAN NOT NULL
count Int            : "count" INTEGER NOT NULL
promoteHeadline Bool : "promote_headline" BOOLEAN NOT NULL

<< comment Table >>
                    : "id" INTEGER PRIMARY KEY
commenter Text      : "commenter" VARCHAR NOT NULL
body Textarea       : "body" VARCHAR NOT NULL
createdAt UTCTime   : "created_at" TIMESTAMP NOT NULL
updatedAt UTCTime   : "updated_at" TIMESTAMP NOT NULL
articleId ArticleId : "article_id" INTEGER NOT NULL REFERENCES "article"


--- Sqlite MEMO ----

sqlite > SELECT * FROM sqlite_master;

table|user|user|2|CREATE TABLE "user"("id" INTEGER PRIMARY KEY,"ident" VARCHAR NOT NULL,"password" VARCHAR NULL,CONSTRAINT "unique_user" UNIQUE ("ident"))
index|sqlite_autoindex_user_1|user|3|
table|email|email|4|CREATE TABLE "email"("id" INTEGER PRIMARY KEY,"email" VARCHAR NOT NULL,"user" INTEGER NULL REFERENCES "user","verkey" VARCHAR NULL,CONSTRAINT "unique_email" UNIQUE ("email"))
index|sqlite_autoindex_email_1|email|5|
table|article|article|6|CREATE TABLE "article"("id" INTEGER PRIMARY KEY,"member_name" VARCHAR NOT NULL,"title" VARCHAR NOT NULL,"perma_link" VARCHAR NOT NULL,"content" VARCHAR NOT NULL,"created_on" TIMESTAMP NOT NULL,"updated_on" TIMESTAMP NOT NULL,"published_on" TIMESTAMP NOT NULL,"approved" BOOLEAN NOT NULL,"count" INTEGER NOT NULL,"promote_headline" BOOLEAN NOT NULL)
table|comment|comment|7|CREATE TABLE "comment"("id" INTEGER PRIMARY KEY,"commenter" VARCHAR NOT NULL,"body" VARCHAR NOT NULL,"created_at" TIMESTAMP NOT NULL,"updated_at" TIMESTAMP NOT NULL,"article_id" INTEGER NOT NULL REFERENCES "article")
table|loginuser|loginuser|10|CREATE TABLE "loginuser"("id" INTEGER PRIMARY KEY,"idstr" VARCHAR NOT NULL,"password" VARCHAR NULL,CONSTRAINT "unique_usr" UNIQUE ("idstr"))
index|sqlite_autoindex_loginuser_1|loginuser|11|

=end
