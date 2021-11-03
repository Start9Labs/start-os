import { Injectable } from '@angular/core'
import { CanActivate, Router } from '@angular/router'
import { HttpService } from '../services/api/http.service'

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
