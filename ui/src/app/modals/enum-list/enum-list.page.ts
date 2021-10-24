import { Component, Input } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { ValueSpecListOf } from '../../pkg-config/config-types'

@Component({
  selector: 'enum-list',
  templateUrl: './enum-list.page.html',
  styleUrls: ['./enum-list.page.scss'],
})
export class EnumListPage {
  @Input() key: string
  @Input() spec: ValueSpecListOf<'enum'>
  @Input() current: string[]
  options: { [option: string]: boolean } = { }
  selectAll = true

  constructor (
    private readonly modalCtrl: ModalController,
  ) { }

  ngOnInit () {
    for (let val of this.spec.spec.values) {
      this.options[val] = this.current.includes(val)
    }
  }

  dismiss () {
    this.modalCtrl.dismiss()
  }

  save () {
    this.modalCtrl.dismiss(Object.keys(this.options).filter(key => this.options[key]))
  }

  toggleSelectAll () {
    Object.keys(this.options).forEach(k => this.options[k] = this.selectAll)
    this.selectAll = !this.selectAll
  }

  toggleSelected (key: string) {
    this.options[key] = !this.options[key]
  }

  asIsOrder () {
    return 0
  }
}
