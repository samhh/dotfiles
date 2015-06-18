Vagrant.configure(2) do |config|

  # https://atlas.hashicorp.com/boxes/search
  config.vm.box = "smallhadroncollider/centos-6.5-lamp" # CentOS 6.5, Apache 2.2.15, MySQL 5.5.36, PHP 5.3.28
  config.vm.network :forwarded_port, guest: 80, host: 4567
  config.vm.synced_folder "./", "/var/www/public"

end
