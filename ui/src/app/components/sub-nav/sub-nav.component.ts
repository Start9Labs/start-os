import { Component, Input, ViewChild } from '@angular/core'
import { IonNav } from '@ionic/angular'
import { SubNavService } from 'src/app/services/sub-nav.service'

@Component({
  selector: 'sub-nav',
  templateUrl: './sub-nav.component.html',
  styleUrls: ['./sub-nav.component.scss'],
})
export class SubNavComponent {
  @Input() path: string
  @Input() rootPage: any
  @Input() rootParams: { [key: string]: any }
  @ViewChild(IonNav) nav: IonNav

  constructor (
    public readonly subNav: SubNavService,
  ) { }

  ngOnInit () {
    this.subNav.path = [this.path]
  }
}
