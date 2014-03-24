require "rubygems"
require "oauth"

CONSUMER_KEY    = "tuwAf85K7bJ6OqWG3Eomg"
CONSUMER_SECRET = "MokGhmoBR8XTa17wYkNT67rK6gHrUEgFGiPZbbGYT8"

consumer = OAuth::Consumer.new(CONSUMER_KEY,
                               CONSUMER_SECRET,
                               :site => 'http://twitter.com'
                              )

request_token = consumer.get_request_token

puts "Access this URL and approve => #{request_token.authorize_url}"

print "Input OAuth Verifier: "
oauth_verifier = gets.chomp.strip

access_token = request_token.get_access_token(
  :oauth_verifier => oauth_verifier
)

puts "Access token: #{access_token.token}"
puts "Access token secret: #{access_token.secret}"
