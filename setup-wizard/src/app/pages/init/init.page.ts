import { Component } from '@angular/core'
import { NavController } from '@ionic/angular'
import { interval, Observable } from 'rxjs'
import { finalize, take } from 'rxjs/operators'

@Component({
  selector: 'app-init',
  templateUrl: 'init.page.html',
  styleUrls: ['init.page.scss'],
})
export class InitPage {
  progress: Observable<number>

  constructor (
    private readonly navCtrl: NavController,
  ) { }

  ngOnInit () {
    this.progress = interval(500)
    .pipe(
      take(101),
      finalize(() => {
        this.navCtrl.navigateForward('/success')
      }),
    )
  }
}

