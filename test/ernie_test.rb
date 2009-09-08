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
end
