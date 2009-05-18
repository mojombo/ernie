options = {}
OptionParser.new do |opts|
  opts.banner = "Usage: ernie command [options]"

  opts.on("-n NAME", "--name NAME", "Node name") do |x|
    options[:name] = x
  end

  opts.on("-p PORT", "--port PORT", "Port") do |x|
    options[:port] = x
  end

  opts.on("-d", "--detached", "Run as a daemon") do
    options[:detached] = true
  end

  opts.on("-P", "--pidfile PIDFILE", "Location to write pid file.") do |x|
    options[:pidfile] = x
  end
end.parse!

command = ARGV[0]

name = options[:name] || DEFAULT_NODE_NAME
port = options[:port] || 8000
pidfile = options[:pidfile] ? "-ernie_server_app pidfile \"'#{options[:pidfile]}'\"" : ''
detached = options[:detached] ? '-detached' : ''

cmd = %Q{erl -boot start_sasl \
             #{detached} \
             +Bc \
             +K true \
             -smp enable \
             #{code_paths}
             -name '#{name}' \
             #{pidfile} \
             -setcookie #{cookie_hash(name)} \
             -ernie_server_app port #{port} \
             -run ernie_server_app boot}.squeeze(' ')
puts cmd
exec(cmd)
