#
# replace "...." string by your own amazon AWS keys.
#
require 'amazon/ecs'
Amazon::Ecs.options = {
         :AWS_access_key_id => "AKIAICK5M5GN6XB7GZQA",                        
         :AWS_secret_key => "ZM/aDLMB4d4i+nDk3/TKm8KZmqatR6x1mcI/5rXz",
         :associate_tag => "nomlab22",
         :country => :jp,
         :response_group => 'Medium'
}
