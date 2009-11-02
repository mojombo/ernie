$LOAD_PATH.unshift(File.join(File.dirname(__FILE__), '..', 'lib'))
require 'ernie'

mod(:test) do
  # Add two numbers together
  fun(:add) do |a, b|
    a + b
  end

  # Return the given number of bytes
  fun(:bytes) do |bytes|
    'x' * bytes
  end

  # Sleep for +sec+ and then return :ok
  fun(:slow) do |sec|
    sleep(sec)
    :ok
  end

  # Throw an error
  fun(:error) do |a, b|
    raise "abandon hope!"
  end
end