import { Component, EventEmitter, Input, Output } from '@angular/core'
import { Annotation, Annotations } from '../../pkg-config/config-utilities'
import { TrackingModalController } from 'src/app/services/tracking-modal-controller.service'
import { ConfigCursor } from 'src/app/pkg-config/config-cursor'
import { ModalPresentable } from 'src/app/pkg-config/modal-presentable'
import { ValueSpecOf, ValueSpec } from 'src/app/pkg-config/config-types'
import { MaskPipe } from 'src/app/pipes/mask.pipe'

@Component({
  selector: 'object-config',
  templateUrl: './object-config.component.html',
  styleUrls: ['./object-config.component.scss'],
})
export class ObjectConfigComponent extends ModalPresentable {
  @Input() cursor: ConfigCursor<'object' | 'union'>
  @Output() onEdit = new EventEmitter<boolean>()
  spec: ValueSpecOf<'object' | 'union'>
  value: object
  annotations: Annotations<'object' | 'union'>

  constructor (
    trackingModalCtrl: TrackingModalController,
  ) {
    super(trackingModalCtrl)
  }

  ngOnInit () {
    this.spec = this.cursor.spec()
    this.value = this.cursor.config()
    this.annotations = this.cursor.getAnnotations()
  }

  async handleClick (key: string) {
    const nextCursor = this.cursor.seekNext(key)
    nextCursor.createFirstEntryForList()

    await this.presentModal(nextCursor, () => {
      this.onEdit.emit(true)
      this.annotations = this.cursor.getAnnotations()
    })
  }

  asIsOrder () {
    return 0
  }
}

@Component({
  selector: 'object-config-item',
  templateUrl: './object-config-item.component.html',
  styleUrls: ['./object-config.component.scss'],
})

export class ObjectConfigItemComponent {
  @Input() key: string
  @Input() spec: ValueSpec
  @Input() value: string | number
  @Input() anno: Annotation
  @Output() onClick = new EventEmitter<boolean>()
  maskPipe: MaskPipe = new MaskPipe()

  displayValue?: string | number | boolean

  ngOnChanges () {
    switch (this.spec.type) {
      case 'string':
        if (this.value) {
          if (this.spec.masked) {
            this.displayValue = this.maskPipe.transform(this.value as string, 4)
          } else {
            this.displayValue = this.value
          }
        } else {
          this.displayValue = '-'
        }
        break
      case 'boolean':
        this.displayValue = String(this.value)
        break
      case 'number':
        this.displayValue = this.value || '-'
        if (this.displayValue && this.spec.units) {
          this.displayValue = `${this.displayValue} ${this.spec.units}`
        }
        break
      case 'enum':
        this.displayValue = this.spec.valueNames[this.value]
        break
      case 'pointer':
        this.displayValue = 'System Defined'
        break
      default:
        return
    }
  }

  async handleClick (): Promise<void> {
    if (this.spec.type === 'pointer') return
    this.onClick.emit(true)
  }
}