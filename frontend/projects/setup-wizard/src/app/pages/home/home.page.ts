import { Component } from '@angular/core'
import { RPCEncryptedService } from 'src/app/services/rpc-encrypted.service'

@Component({
  selector: 'app-home',
  templateUrl: 'home.page.html',
  styleUrls: ['home.page.scss'],
})
export class HomePage {
  constructor(private readonly encrypted: RPCEncryptedService) {}

  ngOnInit() {
    this.encrypted.setSecret()
  }
}
