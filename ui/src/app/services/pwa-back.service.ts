import { Injectable } from '@angular/core'
import { NavController } from '@ionic/angular'

@Injectable({
  providedIn: 'root',
})
export class PwaBackService {
  constructor (
    private readonly nav: NavController,
  ) { }

  // this will strip an entry from the path on navigation
  back () { return this.nav.back() }
}

