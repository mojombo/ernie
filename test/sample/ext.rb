$LOAD_PATH.unshift(File.join(File.dirname(__FILE__), '..', '..', 'lib'))
require 'ernie'

module Ext
  @@state = 0

  def zeronary
    :foo
  end

  def unary(a)
    a
  end

  def binary(a, b)
    a + b
  end

  def ternary(a, b, c)
    a + b + c
  end

  def set_state(x)
    @@state = x
    sleep 5
    nil
  end

  def get_state
    @@state
  end

  def big(x)
    'a' * x
  end

  def cry
    raise "abandon hope!"
  end
end

Ernie.expose(:ext, Ext)