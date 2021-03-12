import { Component, Input, ViewChild } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { ConfigCursor } from 'src/app/app-config/config-cursor'
import { ValueSpecUnion } from 'src/app/app-config/config-types'
import { ObjectConfigComponent } from 'src/app/components/object-config/object-config.component'
import { mapUnionSpec } from '../../app-config/config-utilities'

@Component({
  selector: 'app-config-union',
  templateUrl: './app-config-union.page.html',
  styleUrls: ['./app-config-union.page.scss'],
})
export class AppConfigUnionPage {
  @Input() cursor: ConfigCursor<'union'>

  @ViewChild(ObjectConfigComponent)
  objectConfig: ObjectConfigComponent

  spec: ValueSpecUnion
  value: object
  error: string

  constructor (
    private readonly modalCtrl: ModalController,
  ) { }

  ngOnInit () {
    this.spec = this.cursor.spec()
    this.value = this.cursor.config()
    this.error = this.cursor.checkInvalid()
  }

  async dismiss () {
    this.modalCtrl.dismiss(this.value)
  }

  async handleUnionChange () {
    this.value = mapUnionSpec(this.spec, this.value)
    this.objectConfig.annotations = this.objectConfig.cursor.getAnnotations()
    this.error = this.cursor.checkInvalid()
  }

  setSelectOptions () {
    return {
      header: this.spec.tag.name,
      subHeader: this.spec.changeWarning ? 'Warning!' : undefined,
      message: this.spec.changeWarning ? `${this.spec.changeWarning}` : undefined,
      cssClass: 'select-change-warning',
    }
  }

  handleObjectEdit () {
    this.error = this.cursor.checkInvalid()
  }

  asIsOrder () {
    return 0
  }
}
