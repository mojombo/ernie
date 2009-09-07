$LOAD_PATH.unshift(File.join(File.dirname(__FILE__), '..', 'lib'))
require 'ernie'

# Just about the easiest example I could thing of.
mod(:calc) do
  fun(:add) do |a, b|
    a + b
  end
end

# Useful for tests that need to simulate longer running functions.
mod(:slowcalc) do
  fun(:add) do |a, b|
    sleep(rand * 2)
    a + b
  end
end

# Throw an error
mod(:errorcalc) do
  fun(:add) do |a, b|
    raise "abandon hope!"
  end
end