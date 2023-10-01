let key = 'B7C77CD313EAE76366FE1D7E4667250BD56735A8'

def upload-to [server: string] {
  gpg --keyserver $server --send-keys $key
}

let servers = [
  'keys.openpgp.org'
  'pgp.mit.edu'
  'keyserver.ubuntu.com'
]

# Don't attempt to send in parallel, gpg locks the keyring.
$servers | each { |server| upload-to $server }