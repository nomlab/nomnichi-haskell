# -*- coding: utf-8 -*-

require 'sqlite3'
require 'digest/sha1'

@nomnichi_db = SQLite3::Database.new("nomnichi.sqlite3")

print "User : "; user = gets.chomp
print "Pass : "; pass = gets.chomp

salt = Time.now.to_s

pass_sha = Digest::SHA1.hexdigest(salt + pass)

max_id_sql = <<SQL
select max(id) from user;
SQL
id = @nomnichi_db.execute(max_id_sql).flatten.first.to_i + 1

add_user_sql = <<SQL
insert into user values (#{id}, '#{user}', '#{pass_sha}', '#{salt}');
SQL
@nomnichi_db.execute(add_user_sql)

@nomnichi_db.close

