import { Component, Input } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { ValueSpecListOf } from '../../pkg-config/config-types'
// import { Range } from '../../pkg-config/config-utilities'

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

  // min: number | undefined
  // max: number | undefined
  // minMessage: string
  // maxMessage: string

  selectAll = true

  constructor (
    private readonly modalCtrl: ModalController,
  ) { }

  ngOnInit () {
    // const range = Range.from(this.spec.range)
    // this.min = range.integralMin()
    // this.max = range.integralMax()
    // this.minMessage = `The minimum number of ${this.key} is ${this.min}.`
    // this.maxMessage = `The maximum number of ${this.key} is ${this.max}.`

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

  async toggleSelected (key: string) {
    this.options[key] = !this.options[key]
  }
}
