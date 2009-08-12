require 'bertrpc'

threads = []
svc = BERTRPC::Service.new('localhost', 8000)


5.times do
  threads << Thread.new do
    i = 0
    100.times { i += svc.calc.add.call(1, 2) }
    print "#{i}\n"
  end
end

threads.each { |t| t.join }