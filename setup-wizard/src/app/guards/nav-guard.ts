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
      console.log('here here here')
      console.log('here here here')

      return true
    } else {
      console.log('here here here')
      this.router.navigateByUrl('product-key')
      return false
    }
  }
}
