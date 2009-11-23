require File.dirname(__FILE__) + '/helper'

class ErnieTest < Test::Unit::TestCase
  context "mod" do
    should "add a mod to the mods hash" do
      mod(:foo) { }
      assert Ernie.mods[:foo]
      assert Ernie.mods[:foo].instance_of?(Ernie::Mod)
    end
  end

  context "fun" do
    should "add a fun to the funs hash of the mod" do
      mod(:foo) { fun(:bar) { } }
      assert Ernie.mods[:foo].funs[:bar]
    end
  end

  module TestExposingModule
    def foo
    end

    def bar(a, b, c=nil)
      [a, b, c]
    end

  protected
    def baz
    end

  private
    def bling
    end
  end

  context "expose" do
    setup { Ernie.expose :expo, TestExposingModule }
    teardown { Ernie.mods.clear }

    should "add all public methods from the module" do
      assert_not_nil Ernie.mods[:expo].funs[:foo]
      assert_not_nil Ernie.mods[:expo].funs[:bar]
    end

    should "not expose protected methods" do
      assert_nil Ernie.mods[:expo].funs[:baz]
    end

    should "not expose private methods" do
      assert_nil Ernie.mods[:expo].funs[:bling]
    end

    should "dispatch to module methods properly" do
      res = Ernie.dispatch(:expo, :bar, ['a', :b, { :fiz => 42 }])
      assert_equal ['a', :b, { :fiz => 42 }], res
    end
  end
end
