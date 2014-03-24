# -*- coding: utf-8 -*-

require 'sqlite3'

@database_yml = ARGV[0]

@nomnichi_db = SQLite3::Database.new("nomnichi.sqlite3")
print "User : "; user = gets.chomp
print "Pass : "; pass = gets.chomp
salt = 'sio'#Time.now.to_s
puts salt
pass_sha = `echo -n '#{salt}#{pass}' | shasum`.chomp.split(' ').first

puts pass_sha
max_id_sql = <<SQL
select max(id) from user;
SQL
id = @nomnichi_db.execute(max_id_sql).flatten.first.to_i + 1

add_user_sql = <<SQL
insert into user values (#{id}, '#{user}', '#{pass_sha}', '#{salt}');
SQL
@nomnichi_db.execute(add_user_sql)


# yaml_data = YAML.load_file("./#{@database_yml}")
# into_article_table(yaml_data)
@nomnichi_db.close

