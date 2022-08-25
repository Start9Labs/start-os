import { Component } from '@angular/core'
import { ApiService } from 'src/app/services/api/api.service'
import { RPCEncryptedService } from 'src/app/services/rpc-encrypted.service'
const crypto = require('crypto')

@Component({
  selector: 'app-home',
  templateUrl: 'home.page.html',
  styleUrls: ['home.page.scss'],
})
export class HomePage {
  constructor(
    private readonly unencrypted: ApiService,
    private readonly encrypted: RPCEncryptedService,
  ) {}

  async ngOnInit() {
    const keypair = (await window.crypto.subtle.generateKey(
      { name: 'Ed25519' },
      true,
      ['decrypt'],
    )) as CryptoKeyPair
    const hex = await this.unencrypted.getSecret(keypair.publicKey)
    const ascii = decodeHex(hex)
    const secret = new TextEncoder().encode(ascii)
    this.encrypted.secret = await window.crypto.subtle.decrypt(
      'Ed25519',
      keypair.privateKey,
      secret,
    )
  }
}

function decodeHex(hex: string) {
  let str = ''
  for (let n = 0; n < hex.length; n += 2) {
    str += String.fromCharCode(parseInt(hex.substring(n, 2), 16))
  }
  return str
}
