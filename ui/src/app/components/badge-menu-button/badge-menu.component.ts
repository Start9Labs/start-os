import { Component } from '@angular/core'
import { ServerModel } from '../../models/server-model'
import { Observable } from 'rxjs'
import { map } from 'rxjs/operators'
import { SplitPaneTracker } from 'src/app/services/split-pane.service'
import { isPlatform } from '@ionic/angular'

@Component({
  selector: 'badge-menu-button',
  templateUrl: './badge-menu.component.html',
  styleUrls: ['./badge-menu.component.scss'],
})

export class BadgeMenuComponent {
  badge$: Observable<boolean>
  menuFixedOpen$: Observable<boolean>
  isIos: boolean

  constructor (
    private readonly serverModel: ServerModel,
    private readonly splitPane: SplitPaneTracker,
  ) {
    this.menuFixedOpen$ = this.splitPane.$menuFixedOpenOnLeft$.asObservable()
    this.badge$ = this.serverModel.watch().badge.pipe(map(i => i > 0))
    this.isIos = isPlatform('ios')
  }
}
