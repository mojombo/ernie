require 'rubygems'
require 'test/unit'
require 'shoulda'

ERNIE_ROOT = File.expand_path(File.join(File.dirname(__FILE__), *%w[..]))

$:.unshift(File.join(ERNIE_ROOT, 'lib'))

require 'ernie'
Ernie.auto_start = false

begin
  require 'bertrpc'
rescue LoadError
  puts "You need bertrpc gem installed to run tests."
  exit!(1)
end
