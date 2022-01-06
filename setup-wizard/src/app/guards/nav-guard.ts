import { Injectable } from '@angular/core'
import { CanActivate, Router } from '@angular/router'
import { HttpService } from '../services/api/http.service'
import { StateService } from '../services/state.service'

@Injectable({
  providedIn: 'root',
})
export class NavGuard implements CanActivate {
  constructor (
    private readonly router: Router,
    private readonly httpService: HttpService,
  ) { }

  canActivate (): boolean {
    if (this.httpService.productKey) {
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
  constructor (
    private readonly router: Router,
    private readonly httpService: HttpService,
    private readonly stateService: StateService,
  ) { }

  canActivate (): boolean {
    if (this.httpService.productKey || !this.stateService.hasProductKey) {
      return true
    } else {
      this.router.navigateByUrl('product-key')
      return false
    }
  }
}
