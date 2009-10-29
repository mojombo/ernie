$LOAD_PATH.unshift(File.join(File.dirname(__FILE__), '..', 'lib'))
require 'ernie'

state = 0

# Just about the easiest example I could thing of.
mod(:test) do
  fun(:zeronary) do
    :foo
  end

  fun(:unary) do |a|
    a
  end

  fun(:binary) do |a, b|
    a + b
  end

  fun(:ternary) do |a, b, c|
    a + b + c
  end

  fun(:set_state) do |x|
    state = x
    sleep 5
    nil
  end

  fun(:get_state) do
    state
  end

  fun(:big) do |x|
    'a' * x
  end

  fun(:cry) do
    raise "abandon hope!"
  end
end