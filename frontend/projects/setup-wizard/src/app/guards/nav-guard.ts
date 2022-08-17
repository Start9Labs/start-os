import { Injectable } from '@angular/core'
import { CanActivate, Router } from '@angular/router'
import { RPCEncryptedService } from '../services/rpc-encrypted.service'
import { StateService } from '../services/state.service'

@Injectable({
  providedIn: 'root',
})
export class NavGuard implements CanActivate {
  constructor(
    private readonly router: Router,
    private readonly encrypted: RPCEncryptedService,
  ) {}

  canActivate(): boolean {
    if (this.encrypted.productKey) {
      return true
    } else {
      this.router.navigateByUrl('product-key')
      return false
    }
  }
}

@Injectable({
  providedIn: 'root',
})
export class RecoveryNavGuard implements CanActivate {
  constructor(
    private readonly router: Router,
    private readonly encrypted: RPCEncryptedService,
    private readonly stateService: StateService,
  ) {}

  canActivate(): boolean {
    if (this.encrypted.productKey || !this.stateService.hasProductKey) {
      return true
    } else {
      this.router.navigateByUrl('product-key')
      return false
    }
  }
}
