Vagrant.configure("2") do |config|

    # Globally defined variables
    config.vm.synced_folder "./", "/var/www/public"

    # CentOS 6.5
    # Apache 2.2.15
    # MySQL 5.5.36 (-u root)
    # PHP 5.3.28
    # Note: If PHP session keys don't work, set permissions to 777 (or other more restrictive, but this is guaranteed to work) on /var/lib/php/session
    config.vm.define "php5dot3", primary:true do |php5dot3|
      php5dot3.vm.box = "smallhadroncollider/centos-6.5-lamp"
      php5dot3.vm.network :private_network, ip: "192.168.100.100"
			php5dot3.hostsupdater.aliases = [
				"php5dot3.local"
			]
    end

    # Ubuntu 14.04
    # Apache 2.4.12
    # MySQL 5.5.43 (-u root -p root)
    # PHP 5.6.10
    config.vm.define "php5dot6", autostart:false do |php5dot6|
      php5dot6.vm.box = "scotch/box"
      php5dot6.vm.network :private_network, ip: "192.168.100.101"
			php5dot6.hostsupdater.aliases = [
				"php5dot6.local"
			]
      php5dot6.ssh.username = "vagrant"
      php5dot6.ssh.password = "vagrant"
    end

end
