require File.dirname(__FILE__) + '/helper'

PORT = 27118

class ErnieServerTest < Test::Unit::TestCase

  # global setup
  def setup
    @servers ||= []
    Dir.chdir(ERNIE_ROOT)
    `erlc #{ERNIE_ROOT}/test/sample/intTest.erl`
    Signal.trap("INT") do
      puts "Shutting Down"
      shutdown_servers
      teardown
      exit
    end
  end
  
  def teardown
    `rm intTest.beam`
  end

  context "An Ernie Server" do
    setup do
      start_server
    end

    context "call" do
      should "handle zeronary" do
        assert_equal :foo, svc.call.ext.zeronary
        assert_equal :foo, svc.call.intTest.zeronary
      end

      should "handle unary" do
        assert_equal 5, svc.call.ext.unary(5)
        assert_equal 5, svc.call.intTest.unary(5)
      end

      should "handle binary" do
        assert_equal 7, svc.call.ext.binary(5, 2)
        assert_equal 7, svc.call.intTest.binary(5, 2)
      end

      should "handle ternary" do
        assert_equal 10, svc.call.ext.ternary(5, 2, 3)
        assert_equal 10, svc.call.intTest.ternary(5, 2, 3)
      end

      should "handle massive binaries" do
        assert_equal 8 * 1024 * 1024, svc.call.ext.big(8 * 1024 * 1024).size
        assert_equal 8 * 1024 * 1024, svc.call.intTest.big(8 * 1024 * 1024).size
      end

      should "get an error on missing module" do
        begin
          svc.call.failboat.mcfail(:fail)
          fail "Expected a BERTRPC::ServerError"
        rescue BERTRPC::ServerError => e
          assert_equal "No such module 'failboat'", e.message
        else
          assert false, 'failed to raise on missing module'
        end
      end

      should "get an error on missing function" do
        begin
          svc.call.ext.mcfail(:fail)
          fail "Expected a BERTRPC::ServerError"
        rescue BERTRPC::ServerError => e
          assert_equal "No such function 'ext:mcfail'", e.message
        else
          assert false, 'failed to raise on missing function'
        end

        begin
          svc.call.intTest.mcfail(:fail)
          fail "Expected a BERTRPC::ServerError"
        rescue BERTRPC::ServerError => e
          assert_equal "No such function 'intTest:mcfail'", e.message
        else
          assert false, 'failed to raise on missing function'
        end
      end
    end

    context "cast" do
      should "be received and return immediately" do
        t0 = Time.now
        assert_equal nil, svc.cast.ext.set_state(7)
        assert Time.now - t0 < 1
        assert_equal 7, svc.call.ext.get_state

        t0 = Time.now
        assert_equal nil, svc.cast.intTest.set_state(7)
        assert Time.now - t0 < 1
        sleep 0.25
        assert_equal 7, svc.call.intTest.get_state
      end
    end

    teardown do
      shutdown_server
    end
  end
  
  context "Two Ernie Servers" do
    setup do
      start_servers(2)
      @servers.each do |svc|
        svc.cast.intTest.connect_nodes
      end
    end
    
    context "call" do

      should "handle zeronary" do
        @servers.each do |svc|
          assert_equal :foo, svc.call.ext.zeronary
          assert_equal :foo, svc.call.intTest.zeronary
        end
      end

      should "handle unary" do
        @servers.each do |svc|
          assert_equal 5, svc.call.ext.unary(5)
          assert_equal 5, svc.call.intTest.unary(5)
        end
      end

      should "handle binary" do
        @servers.each do |svc|
          assert_equal 7, svc.call.ext.binary(5, 2)
          assert_equal 7, svc.call.intTest.binary(5, 2)
        end
      end

      should "handle ternary" do
        @servers.each do |svc|
          assert_equal 10, svc.call.ext.ternary(5, 2, 3)
          assert_equal 10, svc.call.intTest.ternary(5, 2, 3)
        end
      end

      should "handle massive binaries" do
        @servers.each do |svc|
          assert_equal 8 * 1024 * 1024, svc.call.ext.big(8 * 1024 * 1024).size
          assert_equal 8 * 1024 * 1024, svc.call.intTest.big(8 * 1024 * 1024).size
        end
      end

      should "make joined erlang nodes possible" do
        assert_equal nil, @servers.first.cast.intTest.set_state(7)
        sleep 0.25
        assert_equal 7, @servers.last.call.intTest.get_state
      end

    end

    teardown do
      shutdown_servers(2)
    end
  end
  
  protected
  
  def svc
    @servers[rand(@servers.size-1)]
  end
  
  def start_server
    start_servers(1)
  end
  
  def shutdown_server
    shutdown_servers(1)
  end
  
  def start_servers(n = 1)
    n.times do
      `#{ERNIE_ROOT}/bin/ernie -c #{ERNIE_ROOT}/test/sample/sample.cfg \
                              -P /tmp/ernie#{@servers.size}.pid \
                              -p #{PORT + @servers.size} \
                              --name ernie#{@servers.size}@127.0.0.1 \
                              -d`
    
      @servers << BERTRPC::Service.new('localhost', PORT + @servers.size)
      loop do
        begin
          @servers.last.call.ext.zeronary
          break
        rescue Object => e
          sleep 0.1
        end
      end
    end
  end
  
  def shutdown_servers(n = nil)
    start = @servers.size - 1
    last = start - (n || start)
    (start).downto(last >= 0 ? last : 0) do |i|
      pid = File.read("/tmp/ernie#{i}.pid")
      `kill -9 #{pid}`
    end
  end
  
end
