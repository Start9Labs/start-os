#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../../.."
node <<'NODE'
const assert = require('node:assert/strict')
const fs = require('node:fs')
const read = path => fs.readFileSync(path, 'utf8')
const base = 'projects/start-os/'
const config = read(base + 'build/lib/resolved.conf')
const values = Object.fromEntries(config.split('\n').filter(line => line.includes('=')).map(line => line.split('=')))
assert.equal(values.DNSStubListener, 'no')
assert.equal(values.MulticastDNS, 'no')
assert.equal(values.Cache, 'no')
assert.equal(values.FallbackDNS, '')
assert.equal(values.DNS, undefined, 'resolved must read the OS stub from resolv.conf')
const postinst = read(base + 'debian/postinst')
const nm = postinst.match(/cat << EOF > \/etc\/NetworkManager\/NetworkManager.conf\n([\s\S]*?)\nEOF/)[1]
assert.match(nm, /^dns=none$/m)
for (const [script, server] of [['debian/postinst', '127.0.0.1'], ['container-runtime/deb-install.sh', '10.0.3.1']]) {
  const source = read(base + script)
  assert.deepEqual([...source.matchAll(/echo "nameserver ([^"]+)"/g)].map(match => match[1]), [server])
  assert.match(source, /^cp \/usr\/lib\/startos\/resolved.conf \/etc\/systemd\/resolved.conf$/m)
}
assert.match(read(base + 'container-runtime/update-image.sh'), /^cp \.\.\/build\/lib\/resolved.conf \/tmp\/combined\/usr\/lib\/startos\/resolved.conf$/m)
assert.match(read(base + 'build.mk'), /^projects\/start-os\/container-runtime\/rootfs\.\$\(ARCH\)\.squashfs: .*projects\/start-os\/build\/lib\/resolved.conf/m)
console.log('DNS platform configuration keeps NSS, host stubs and container stubs on the OS resolver')
NODE
