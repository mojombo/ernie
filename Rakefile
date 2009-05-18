require 'rake'

ERLC_TEST_FLAGS = ""
ERLC_FLAGS = "-o ../ebin"

task :default do
  cd "elib"
  sh "erlc  #{ERLC_FLAGS} #{ERLC_TEST_FLAGS} #{Dir["**/*.erl"].join(" ")}"
end