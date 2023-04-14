import {
  Component,
  Input,
  Output,
  EventEmitter,
  Inject,
  inject,
  SimpleChanges,
} from '@angular/core'
import { UntypedFormArray, UntypedFormGroup } from '@angular/forms'
import { AlertButton, AlertController } from '@ionic/angular'
import {
  InputSpec,
  ListValueSpecOf,
  ValueSpec,
  ValueSpecBoolean,
  ValueSpecList,
  ValueSpecUnion,
} from 'start-sdk/lib/config/configTypes'
import { FormService } from 'src/app/services/form.service'
import { THEME, pauseFor } from '@start9labs/shared'
import { v4 } from 'uuid'
import { DOCUMENT } from '@angular/common'

const Mustache = require('mustache')

@Component({
  selector: 'form-object',
  templateUrl: './form-object.component.html',
  styleUrls: ['./form-object.component.scss'],
})
export class FormObjectComponent {
  @Input() objectSpec!: InputSpec
  @Input() formGroup!: UntypedFormGroup
  @Input() current?: Record<string, any>
  @Input() original?: Record<string, any>
  @Output() onInputChange = new EventEmitter<void>()
  @Output() hasNewOptions = new EventEmitter<void>()
  warningAck: { [key: string]: boolean } = {}
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
    private readonly formService: FormService,
    @Inject(DOCUMENT) private readonly document: Document,
  ) {}

  ngOnInit() {
    this.setDisplays()

    // setTimeout hack to avoid ExpressionChangedAfterItHasBeenCheckedError
    // setTimeout(() => {
    //   if (this.original && Object.values(this.objectSpec).some(spec => spec['is-new'])) this.hasNewOptions.emit()
    // })
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

      if (spec.type === 'list' && spec.spec.type === 'object') {
        this.objectListDisplay[key] = []
        this.formGroup.get(key)?.value.forEach((obj: any, index: number) => {
          const displayAs = (spec.spec as ListValueSpecOf<'object'>).displayAs
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
    })
  }

  handleBooleanChange(key: string, spec: ValueSpecBoolean) {
    if (spec.warning) {
      const current = this.formGroup.get(key)?.value
      const cancelFn = () => this.formGroup.get(key)?.setValue(!current)
      this.presentAlertChangeWarning(key, spec, undefined, cancelFn)
    }
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

  private addListItem(key: string): void {
    const arr = this.formGroup.get(key) as UntypedFormArray
    const listSpec = this.objectSpec[key] as ValueSpecList
    const newItem = this.formService.getListItem(listSpec, undefined)!

    const index = arr.length
    arr.insert(index, newItem)

    if (listSpec.spec.type === 'object') {
      const displayAs = listSpec.spec.displayAs
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

      if (listSpec.spec.type === 'object') {
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

  asIsOrder() {
    return 0
  }
}

export function getElementId(objectId: string, key: string, index = 0): string {
  return `${key}-${index}-${objectId}`
}
