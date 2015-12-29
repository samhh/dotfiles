Vagrant.configure(2) do |config|

	# Ubuntu 14.04 (SSH pw: vagrant), Apache 2.4.12, MySQL 5.5.43 (-u root -p root), PHP 5.6.10
	config.vm.box = 'scotch/box'
	config.vm.network :forwarded_port, guest: 80, host: 4567
	config.vm.synced_folder './', '/var/www/public'

end
