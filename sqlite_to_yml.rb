########################################
# sqlite to yml
# ruby sqlite_to_yml nomnichi.sqlite3 > nomnichi.yaml
########################################

require 'sqlite3'
require 'yaml'

@database_sqlite = ARGV[0]

@nomnichi_db = SQLite3::Database.new("#{@database_sqlite}")

@nomnichi_db.transaction do
  article_sql = <<SQL
select * from article;
SQL

  @nomnichi_db.execute(article_sql).each do |row|
    print "-", "\n"
    print "  member_name: ", "#{row[1]}", "\n"
    print "  title: ", "#{row[2]}", "\n"
    print "  perma_link: ", "#{row[3]}", "\n"
    print "  content: |", "\n"
    string = row[4].split("\n")
    string.each do |s|
      print "    #{s}", "\n"
    end
    print "  created_on: ", "#{row[5]}", "\n"
    print "  updated_on: ", "#{row[6]}", "\n"
    print "  published_on: ", "#{row[7]}", "\n"
    print "  approved: ", "#{row[8]}", "\n"
    print "  count: ", "#{row[9]}", "\n"
    print "  promote_headline: ", "#{row[10]}", "\n"
    print "  comment: ", "\n"
  end
end
      
@nomnichi_db.close
