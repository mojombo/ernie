require File.dirname(__FILE__) + '/helper'

class ErnieTest < Test::Unit::TestCase
  context "mod" do
    should "yield to the block" do
      called = false
      mod(:foo) { called = true }
      assert called, "mod did not yield to the block"
    end

    should "add a mod to the mods hash" do
      mod(:foo) { }
      assert Ernie.mods[:foo]
      assert Ernie.mods[:foo].instance_of?(Ernie::Mod)
    end

    should "overwrite previous mod with the same name" do
      first_mod_object, second_mod_object = nil, nil
      mod(:foo) { first_mod_object = Ernie.current_mod }
      mod(:foo) { second_mod_object = Ernie.current_mod }
      assert_not_same second_mod_object, first_mod_object
    end

    should "set the mod's name" do
      mod(:foo) { }
      assert_equal :foo, Ernie.mods[:foo].name
    end
  end

  context "fun" do
    should "add a fun to the funs hash of the mod" do
      mod(:foo) { fun(:bar) { } }
      assert Ernie.mods[:foo].funs[:bar]
    end

    should "dispatch to a fun" do
      mod(:foo) { fun(:echo) { |arg| arg } }
      assert 'hello', Ernie.dispatch(:foo, :echo, 'hello')
    end

    should "fail when no block is provided" do
      assert_raises(TypeError) do
        mod(:foo) { fun(:bar) }
      end
    end
  end
end
