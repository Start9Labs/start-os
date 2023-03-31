import { Component } from '@angular/core'
import {
  ActionSheetButton,
  ActionSheetController,
  AlertController,
  LoadingController,
  ModalController,
} from '@ionic/angular'
import {
  GenericInputComponent,
  GenericInputOptions,
} from 'src/app/modals/generic-input/generic-input.component'
import { PatchDB } from 'patch-db-client'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { InputSpec } from 'start-sdk/lib/config/config-types'
import * as yaml from 'js-yaml'
import { v4 } from 'uuid'
import { DataModel, DevData } from 'src/app/services/patch-db/data-model'
import { ErrorToastService } from '@start9labs/shared'
import { TuiDestroyService } from '@taiga-ui/cdk'
import { takeUntil } from 'rxjs/operators'

@Component({
  selector: 'developer-list',
  templateUrl: 'developer-list.page.html',
  styleUrls: ['developer-list.page.scss'],
  providers: [TuiDestroyService],
})
export class DeveloperListPage {
  devData: DevData = {}

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly api: ApiService,
    private readonly loadingCtrl: LoadingController,
    private readonly errToast: ErrorToastService,
    private readonly alertCtrl: AlertController,
    private readonly destroy$: TuiDestroyService,
    private readonly patch: PatchDB<DataModel>,
    private readonly actionCtrl: ActionSheetController,
  ) {}

  ngOnInit() {
    this.patch
      .watch$('ui', 'dev')
      .pipe(takeUntil(this.destroy$))
      .subscribe(dd => {
        this.devData = dd
      })
  }

  async openCreateProjectModal() {
    const projNumber = Object.keys(this.devData).length + 1
    const options: GenericInputOptions = {
      title: 'Add new project',
      message: 'Create a new dev project.',
      label: 'New project',
      useMask: false,
      placeholder: `Project ${projNumber}`,
      required: false,
      initialValue: `Project ${projNumber}`,
      buttonText: 'Save',
      submitFn: (value: string) => this.createProject(value),
    }

    const modal = await this.modalCtrl.create({
      componentProps: { options },
      cssClass: 'alertlike-modal',
      presentingElement: await this.modalCtrl.getTop(),
      component: GenericInputComponent,
    })

    await modal.present()
  }

  async presentAction(id: string, event: Event) {
    event.stopPropagation()
    event.preventDefault()
    const buttons: ActionSheetButton[] = [
      {
        text: 'Edit Name',
        icon: 'pencil',
        handler: () => {
          this.openEditNameModal(id)
        },
      },
      {
        text: 'Delete',
        icon: 'trash',
        role: 'destructive',
        handler: () => {
          this.presentAlertDelete(id)
        },
      },
    ]

    const action = await this.actionCtrl.create({
      header: this.devData[id].name,
      subHeader: 'Manage project',
      mode: 'ios',
      buttons,
    })

    await action.present()
  }

  async openEditNameModal(id: string) {
    const curName = this.devData[id].name
    const options: GenericInputOptions = {
      title: 'Edit Name',
      message: 'Edit the name of your project.',
      label: 'Name',
      useMask: false,
      placeholder: curName,
      required: false,
      initialValue: curName,
      buttonText: 'Save',
      submitFn: (value: string) => this.editName(id, value),
    }

    const modal = await this.modalCtrl.create({
      componentProps: { options },
      cssClass: 'alertlike-modal',
      presentingElement: await this.modalCtrl.getTop(),
      component: GenericInputComponent,
    })

    await modal.present()
  }

  async createProject(name: string) {
    // fail silently if duplicate project name
    if (
      Object.values(this.devData)
        .map(v => v.name)
        .includes(name)
    )
      return

    const loader = await this.loadingCtrl.create({
      message: 'Creating Project...',
    })
    await loader.present()

    try {
      const id = v4()
      const config = yaml
        .dump(SAMPLE_CONFIG)
        .replace(/warning:/g, '# Optional\n  warning:')

      const def = { name, config, instructions: SAMPLE_INSTUCTIONS }
      await this.api.setDbValue<{
        name: string
        config: string
        instructions: string
      }>(['dev', id], def)
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  async presentAlertDelete(id: string) {
    const alert = await this.alertCtrl.create({
      header: 'Caution',
      message: `Are you sure you want to delete this project?`,
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Delete',
          handler: () => {
            this.delete(id)
          },
          cssClass: 'enter-click',
        },
      ],
    })
    await alert.present()
  }

  async editName(id: string, newName: string) {
    const loader = await this.loadingCtrl.create({
      message: 'Saving...',
    })
    await loader.present()

    try {
      await this.api.setDbValue<string>(['dev', id, 'name'], newName)
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  async delete(id: string) {
    const loader = await this.loadingCtrl.create({
      message: 'Removing Project...',
    })
    await loader.present()

    try {
      const devDataToSave: DevData = JSON.parse(JSON.stringify(this.devData))
      delete devDataToSave[id]
      await this.api.setDbValue<DevData>(['dev'], devDataToSave)
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }
}

const SAMPLE_INSTUCTIONS = `# Create Instructions using Markdown! :)`

const SAMPLE_CONFIG: InputSpec = {
  'sample-string': {
    type: 'string',
    name: 'Example String Input',
    inputmode: 'text',
    required: true,
    masked: false,
    // optional
    description: 'Example description for required string input.',
    placeholder: 'Enter string value',
    pattern: '^[a-zA-Z0-9! _]+$',
    patternDescription: 'Must be alphanumeric (may contain underscore).',
    default: null,
    warning: null,
  },
  'sample-number': {
    type: 'number',
    name: 'Example Number Input',
    inputmode: 'decimal',
    required: true,
    range: '[5,1000000]',
    integral: true,
    // optional
    warning: 'Example warning to display when changing this number value.',
    units: 'ms',
    description: 'Example description for optional number input.',
    placeholder: 'Enter number value',
    default: null,
  },
  'sample-boolean': {
    type: 'boolean',
    name: 'Example Boolean Toggle',
    // optional
    description: 'Example description for boolean toggle',
    default: true,
    warning: null,
  },
  'sample-select': {
    type: 'multiselect',
    name: 'Example Enum Select',
    values: {
      red: 'Red',
      blue: 'Blue',
      green: 'Green',
    },
    // optional
    warning: 'Example warning to display when changing this select value.',
    description: 'Example description for select select',
    range: '[0, 2)',
    default: ['red'],
  },
}
