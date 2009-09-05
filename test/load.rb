require 'bertrpc'

$stdout.sync = true

threads = []
svc = BERTRPC::Service.new('localhost', 8000)

8.times do
  threads << Thread.new do
    i = 0
    10.times { i += svc.call.calc.add(1, 2); print '.'; $stdout.flush }
    print "(#{i})"
  end
end

threads.each { |t| t.join }

puts