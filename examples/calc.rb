$LOAD_PATH.unshift(File.join(File.dirname(__FILE__), '..', 'lib'))
require 'ernie'

mod(:calc) do
  fun(:add) do |a, b|
    a + b
  end
end