import { Router } from '@angular/router'
import { Injectable } from '@angular/core'
import { NavController } from '@ionic/angular'

@Injectable({
  providedIn: 'root',
})
export class PwaBackService {
  constructor (
    private readonly router: Router,
    private readonly nav: NavController,
  ) { }

  // this will strip an entry from the path on navigation
  back () {
    return this.nav.back()
    // this.router.navigate()
    // const path = this.router.url.split('/').filter(a => a !== '')
    // path.pop()
    // this.router.navigate(['/', ...path], { replaceUrl: false })
  }
}

