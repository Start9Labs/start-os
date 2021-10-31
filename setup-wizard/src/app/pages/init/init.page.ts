import { Component } from '@angular/core'
import { NavController } from '@ionic/angular'
import { interval, Observable } from 'rxjs'
import { finalize, skip, take } from 'rxjs/operators'
import { pauseFor } from 'src/app/util/misc.util'

@Component({
  selector: 'app-init',
  templateUrl: 'init.page.html',
  styleUrls: ['init.page.scss'],
})
export class InitPage {
  progress: Observable<number>

  constructor (
    private navCtrl: NavController,
  ) { }

  ngOnInit () {
    this.progress = interval(500)
    .pipe(
      skip(1),
      take(100),
      finalize(async () => {
        await pauseFor(1000)
        this.navCtrl.navigateForward('/success')
      }),
    )
  }
}

