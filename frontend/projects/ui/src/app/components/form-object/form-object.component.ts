import {
  Component,
  Input,
  Output,
  EventEmitter,
  ChangeDetectionStrategy,
  Inject,
  inject,
  SimpleChanges,
} from '@angular/core'
import { FormArray, UntypedFormArray, UntypedFormGroup } from '@angular/forms'
import { AlertButton, AlertController, ModalController } from '@ionic/angular'
import {
  ConfigSpec,
  ListValueSpecOf,
  ValueSpec,
  ValueSpecBoolean,
  ValueSpecEnum,
  ValueSpecList,
  ValueSpecListOf,
  ValueSpecUnion,
} from 'src/app/pkg-config/config-types'
import { FormService } from 'src/app/services/form.service'
import { EnumListPage } from 'src/app/modals/enum-list/enum-list.page'
import { THEME, pauseFor } from '@start9labs/shared'
import { v4 } from 'uuid'
import { DOCUMENT } from '@angular/common'

const Mustache = require('mustache')

interface Config {
  [key: string]: any
}
@Component({
  selector: 'form-object',
  templateUrl: './form-object.component.html',
  styleUrls: ['./form-object.component.scss'],
})
export class FormObjectComponent {
  @Input() objectSpec!: ConfigSpec
  @Input() formGroup!: UntypedFormGroup
  @Input() current?: Config
  @Input() original?: Config
  @Output() onInputChange = new EventEmitter<void>()
  @Output() hasNewOptions = new EventEmitter<void>()
  warningAck: { [key: string]: boolean } = {}
  unmasked: { [key: string]: boolean } = {}
  objectDisplay: {
    [key: string]: { expanded: boolean; hasNewOptions: boolean }
  } = {}
  objectListDisplay: {
    [key: string]: { expanded: boolean; displayAs: string }[]
  } = {}
  objectId = v4()

  readonly theme$ = inject(THEME)

  constructor(
    private readonly alertCtrl: AlertController,
    private readonly modalCtrl: ModalController,
    private readonly formService: FormService,
    @Inject(DOCUMENT) private readonly document: Document,
  ) {}

  ngOnInit() {
    this.setDisplays()

    // setTimeout hack to avoid ExpressionChangedAfterItHasBeenCheckedError
    setTimeout(() => {
      if (
        this.original &&
        Object.keys(this.current || {}).some(
          key => this.original![key] === undefined,
        )
      )
        this.hasNewOptions.emit()
    })
  }

  ngOnChanges(changes: SimpleChanges) {
    const specChanges = changes['objectSpec']

    if (!specChanges) return

    if (
      !specChanges.firstChange &&
      Object.keys({
        ...specChanges.previousValue,
        ...specChanges.currentValue,
      }).length !== Object.keys(specChanges.previousValue).length
    ) {
      this.setDisplays()
    }
  }

  private setDisplays() {
    Object.keys(this.objectSpec).forEach(key => {
      const spec = this.objectSpec[key]

      if (spec.type === 'list' && ['object', 'union'].includes(spec.subtype)) {
        this.objectListDisplay[key] = []
        this.formGroup.get(key)?.value.forEach((obj: any, index: number) => {
          const displayAs = (spec.spec as ListValueSpecOf<'object'>)[
            'display-as'
          ]
          this.objectListDisplay[key][index] = {
            expanded: false,
            displayAs: displayAs
              ? (Mustache as any).render(displayAs, obj)
              : '',
          }
        })
      } else if (spec.type === 'object') {
        this.objectDisplay[key] = {
          expanded: false,
          hasNewOptions: false,
        }
      }
    })
  }

  addListItemWrapper<T extends ValueSpec>(
    key: string,
    spec: T extends ValueSpecUnion ? never : T,
  ) {
    this.presentAlertChangeWarning(key, spec, () => this.addListItem(key))
  }

  toggleExpandObject(key: string) {
    this.objectDisplay[key].expanded = !this.objectDisplay[key].expanded
  }

  toggleExpandListObject(key: string, i: number) {
    this.objectListDisplay[key][i].expanded =
      !this.objectListDisplay[key][i].expanded
  }

  updateLabel(key: string, i: number, displayAs: string) {
    this.objectListDisplay[key][i].displayAs = displayAs
      ? Mustache.render(displayAs, this.formGroup.get(key)?.value[i])
      : ''
  }

  handleInputChange() {
    this.onInputChange.emit()
  }

  setHasNew(key: string) {
    this.hasNewOptions.emit()
    setTimeout(() => {
      this.objectDisplay[key].hasNewOptions = true
    }, 100)
  }

  handleBooleanChange(key: string, spec: ValueSpecBoolean) {
    if (spec.warning) {
      const current = this.formGroup.get(key)?.value
      const cancelFn = () => this.formGroup.get(key)?.setValue(!current)
      this.presentAlertChangeWarning(key, spec, undefined, cancelFn)
    }
  }

  async presentModalEnumList(
    key: string,
    spec: ValueSpecListOf<'enum'>,
    current: string[],
  ) {
    const modal = await this.modalCtrl.create({
      componentProps: {
        key,
        spec,
        current,
      },
      component: EnumListPage,
    })

    modal.onWillDismiss<string[]>().then(({ data }) => {
      if (!data) return
      this.updateEnumList(key, current, data)
    })

    await modal.present()
  }

  async presentAlertChangeWarning<T extends ValueSpec>(
    key: string,
    spec: T extends ValueSpecUnion ? never : T,
    okFn?: Function,
    cancelFn?: Function,
  ) {
    if (!spec.warning || this.warningAck[key]) return okFn ? okFn() : null
    this.warningAck[key] = true

    const buttons: AlertButton[] = [
      {
        text: 'Ok',
        handler: () => {
          if (okFn) okFn()
        },
        cssClass: 'enter-click',
      },
    ]

    if (okFn || cancelFn) {
      buttons.unshift({
        text: 'Cancel',
        handler: () => {
          if (cancelFn) cancelFn()
        },
      })
    }

    const alert = await this.alertCtrl.create({
      header: 'Warning',
      subHeader: `Editing ${spec.name} has consequences:`,
      message: spec.warning,
      buttons,
    })
    await alert.present()
  }

  async presentAlertDelete(key: string, index: number) {
    const alert = await this.alertCtrl.create({
      header: 'Confirm',
      message: 'Are you sure you want to delete this entry?',
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Delete',
          handler: () => {
            this.deleteListItem(key, index)
          },
          cssClass: 'enter-click',
        },
      ],
    })
    await alert.present()
  }

  async presentAlertBoolEnumDescription(
    event: Event,
    spec: ValueSpecBoolean | ValueSpecEnum,
  ) {
    event.stopPropagation()
    const { name, description } = spec

    const alert = await this.alertCtrl.create({
      header: name,
      message: description,
      buttons: [
        {
          text: 'OK',
          cssClass: 'enter-click',
        },
      ],
    })
    await alert.present()
  }

  private addListItem(key: string): void {
    const arr = this.formGroup.get(key) as UntypedFormArray
    const listSpec = this.objectSpec[key] as ValueSpecList
    const newItem = this.formService.getListItem(listSpec, undefined)!

    const index = arr.length
    arr.insert(index, newItem)

    if (['object', 'union'].includes(listSpec.subtype)) {
      const displayAs = (listSpec.spec as ListValueSpecOf<'object'>)[
        'display-as'
      ]
      this.objectListDisplay[key].push({
        expanded: false,
        displayAs: displayAs ? Mustache.render(displayAs, newItem.value) : '',
      })
    }

    setTimeout(() => {
      const element = this.document.getElementById(
        getElementId(this.objectId, key, index),
      )
      element?.parentElement?.scrollIntoView({ behavior: 'smooth' })

      if (['object', 'union'].includes(listSpec.subtype)) {
        pauseFor(250).then(() => this.toggleExpandListObject(key, index))
      }
    }, 100)

    arr.markAsDirty()
  }

  private deleteListItem(key: string, index: number, markDirty = true): void {
    // if (this.objectListDisplay[key])
    //   this.objectListDisplay[key][index].height = '0px'
    const arr = this.formGroup.get(key) as UntypedFormArray
    if (markDirty) arr.markAsDirty()
    pauseFor(250).then(() => {
      if (this.objectListDisplay[key])
        this.objectListDisplay[key].splice(index, 1)
      arr.removeAt(index)
    })
  }

  private updateEnumList(key: string, current: string[], updated: string[]) {
    const arr = this.formGroup.get(key) as FormArray

    for (let i = current.length - 1; i >= 0; i--) {
      if (!updated.includes(current[i])) {
        arr.removeAt(i)
      }
    }

    const listSpec = this.objectSpec[key] as ValueSpecList

    updated.forEach(val => {
      if (!current.includes(val)) {
        const newItem = this.formService.getListItem(listSpec, val)!
        arr.insert(arr.length, newItem)
      }
    })

    arr.markAsDirty()
  }

  asIsOrder() {
    return 0
  }
}

@Component({
  selector: 'form-union',
  templateUrl: './form-union.component.html',
  styleUrls: ['./form-object.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class FormUnionComponent {
  @Input() formGroup!: UntypedFormGroup
  @Input() spec!: ValueSpecUnion
  @Input() current?: Config
  @Input() original?: Config

  get unionValue() {
    return this.formGroup.get(this.spec.tag.id)?.value
  }

  get isNew() {
    return !this.original
  }

  get hasNewOptions() {
    const tagId = this.spec.tag.id
    return (
      this.original?.[tagId] === this.current?.[tagId] &&
      !!Object.keys(this.current || {}).find(
        key => this.original![key] === undefined,
      )
    )
  }

  objectId = v4()

  constructor(private readonly formService: FormService) {}

  updateUnion(e: any): void {
    const tagId = this.spec.tag.id

    Object.keys(this.formGroup.controls).forEach(control => {
      if (control === tagId) return
      this.formGroup.removeControl(control)
    })

    const unionGroup = this.formService.getUnionObject(
      this.spec as ValueSpecUnion,
      e.detail.value,
    )

    Object.keys(unionGroup.controls).forEach(control => {
      if (control === tagId) return
      this.formGroup.addControl(control, unionGroup.controls[control])
    })
  }
}

@Component({
  selector: 'form-label',
  templateUrl: './form-label.component.html',
  styleUrls: ['./form-object.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class FormLabelComponent {
  @Input() data!: {
    name: string
    new: boolean
    edited: boolean
    description?: string
    required?: boolean
    newOptions?: boolean
  }

  constructor(private readonly alertCtrl: AlertController) {}

  async presentAlertDescription(event: Event) {
    event.stopPropagation()
    const { name, description } = this.data

    const alert = await this.alertCtrl.create({
      header: name,
      message: description,
      buttons: [
        {
          text: 'OK',
          cssClass: 'enter-click',
        },
      ],
    })
    await alert.present()
  }
}

export function getElementId(objectId: string, key: string, index = 0): string {
  return `${key}-${index}-${objectId}`
}
