require File.dirname(__FILE__) + '/helper'

PORT = 27118

class ErnieServerTest < Test::Unit::TestCase
  context "An Ernie Server" do
    setup do
      `#{ERNIE_ROOT}/bin/ernie -h #{ERNIE_ROOT}/test/handler.rb \
                               -P /tmp/ernie.pid \
                               -p #{PORT} \
                               -d`
      @svc = BERTRPC::Service.new('localhost', PORT)
      loop do
        begin
          @svc.call.test.zeronary
          break
        rescue Object => e
          sleep 0.1
        end
      end
    end

    context "call" do
      should "handle zeronary" do
        assert_equal :foo, @svc.call.test.zeronary
      end

      should "handle unary" do
        assert_equal 5, @svc.call.test.unary(5)
      end

      should "handle binary" do
        assert_equal 7, @svc.call.test.binary(5, 2)
      end

      should "handle ternary" do
        assert_equal 10, @svc.call.test.ternary(5, 2, 3)
      end

      should "handle massive binaries" do
        assert_equal 8 * 1024 * 1024, @svc.call.test.big(8 * 1024 * 1024).size
      end

      should "get an error on missing module" do
        begin
          @svc.call.failboat.mcfail(:fail)
          fail "Expected a BERTRPC::ServerError"
        rescue BERTRPC::ServerError => e
          assert_equal "No such module 'failboat'", e.message
        end
      end

      should "get an error on missing function" do
        begin
          @svc.call.test.mcfail(:fail)
          fail "Expected a BERTRPC::ServerError"
        rescue BERTRPC::ServerError => e
          assert_equal "No such function 'test:mcfail'", e.message
        end
      end
    end

    context "cast" do
      should "be received and return immediately" do
        t0 = Time.now
        assert_equal nil, @svc.cast.test.set_state(7)
        assert Time.now - t0 < 1
        assert_equal 7, @svc.call.test.get_state
      end
    end

    teardown do
      pid = File.read('/tmp/ernie.pid')
      `kill -9 #{pid}`
    end
  end
end
