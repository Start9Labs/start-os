import { Component, Input } from '@angular/core'
import { AlertController } from '@ionic/angular'
import { Annotations, Range } from '../../app-config/config-utilities'
import { TrackingModalController } from 'src/app/services/tracking-modal-controller.service'
import { ConfigCursor } from 'src/app/app-config/config-cursor'
import { ValueSpecList, isValueSpecListOf } from 'src/app/app-config/config-types'
import { ModalPresentable } from 'src/app/app-config/modal-presentable'

@Component({
  selector: 'app-config-list',
  templateUrl: './app-config-list.page.html',
  styleUrls: ['./app-config-list.page.scss'],
})
export class AppConfigListPage extends ModalPresentable {
  @Input() cursor: ConfigCursor<'list'>

  spec: ValueSpecList
  value: string[] | number[] | object[]
  valueString: string[]
  annotations: Annotations<'list'>

  // enum only
  options: { value: string, checked: boolean }[] = []
  selectAll = true
  //

  min: number | undefined
  max: number | undefined

  minMessage: string
  maxMessage: string

  error: string

  constructor (
    private readonly alertCtrl: AlertController,
    trackingModalCtrl: TrackingModalController,
  ) {
    super(trackingModalCtrl)
  }

  ngOnInit () {
    this.spec = this.cursor.spec()
    this.value = this.cursor.config()
    const range = Range.from(this.spec.range)
    this.min = range.integralMin()
    this.max = range.integralMax()
    this.minMessage = `The minimum number of ${this.cursor.key()} is ${this.min}.`
    this.maxMessage = `The maximum number of ${this.cursor.key()} is ${this.max}.`
    // enum list only
    if (isValueSpecListOf(this.spec, 'enum')) {
      for (let val of this.spec.spec.values) {
        this.options.push({
          value: val,
          checked: (this.value as string[]).includes(val),
        })
      }
    }
    this.updateCaches()
  }

  async dismiss () {
    return this.dismissModal(this.value)
  }

  // enum only
  toggleSelectAll () {
    if (!isValueSpecListOf(this.spec, 'enum')) { throw new Error('unreachable') }

    this.value.length = 0
    if (this.selectAll) {
      for (let v of this.spec.spec.values) {
        (this.value as string[]).push(v)
      }
      for (let option of this.options) {
        option.checked = true
      }
    } else {
      for (let option of this.options) {
        option.checked = false
      }
    }
    this.updateCaches()
  }

  // enum only
  async toggleSelected (value: string) {
    const index = (this.value as string[]).indexOf(value)

    // if present, delete
    if (index > -1) {
      (this.value as string[]).splice(index, 1)
    // if not present, add
    } else {
      (this.value as string[]).push(value)
    }

    this.updateCaches()
  }

  async presentModalValueEdit (index?: number) {
    const nextCursor = this.cursor.seekNext(index === undefined ? this.value.length : index)
    nextCursor.createFirstEntryForList()
    return this.presentModal(nextCursor, () => this.updateCaches())
  }

  async presentAlertDelete (key: number, e: Event) {
    e.stopPropagation()
    
    const alert = await this.alertCtrl.create({
      backdropDismiss: false,
      header: 'Caution',
      message: `Are you sure you want to delete this entry?`,
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Delete',
          cssClass: 'alert-danger',
          handler: () => {
            if (typeof key === 'number') {
              (this.value as any[]).splice(key, 1)
            } else {
              delete this.value[key]
            }
            this.updateCaches()
          },
        },
      ],
    })
    await alert.present()
  }

  asIsOrder () {
    return 0
  }

  private updateCaches () {
    if (isValueSpecListOf(this.spec, 'enum')) {
      this.selectAll = this.value.length !== this.spec.spec.values.length
    }
    this.error = this.cursor.checkInvalid()
    this.annotations = this.cursor.getAnnotations()
    this.valueString = (this.value as any[]).map((_, idx) => this.cursor.seekNext(idx).toString())
  }
}
