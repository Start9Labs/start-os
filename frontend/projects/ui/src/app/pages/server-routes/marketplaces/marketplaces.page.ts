import { Component, Inject } from '@angular/core'
import {
  ActionSheetController,
  AlertController,
  LoadingController,
  ModalController,
} from '@ionic/angular'
import { ActionSheetButton } from '@ionic/core'
import {
  DestroyService,
  ErrorToastService,
  getUrlHostname,
} from '@start9labs/shared'
import {
  AbstractMarketplaceService,
  Marketplace,
} from '@start9labs/marketplace'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ValueSpecObject } from 'src/app/pkg-config/config-types'
import { GenericFormPage } from 'src/app/modals/generic-form/generic-form.page'
import { PatchDB } from 'patch-db-client'
import { v4 } from 'uuid'
import { DataModel } from '../../../services/patch-db/data-model'
import { ConfigService } from '../../../services/config.service'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { distinctUntilChanged, takeUntil } from 'rxjs/operators'
import { getServerInfo } from '../../../util/get-server-info'
import { url } from 'inspector'

type MarketplaceWithId = Marketplace & { id: string }

@Component({
  selector: 'marketplaces',
  templateUrl: 'marketplaces.page.html',
  styleUrls: ['marketplaces.page.scss'],
  providers: [DestroyService],
})
export class MarketplacesPage {
  selectedId?: string
  standardMarketplaces: MarketplaceWithId[] = []
  altMarketplaces: MarketplaceWithId[] = []

  constructor(
    private readonly api: ApiService,
    private readonly loadingCtrl: LoadingController,
    private readonly modalCtrl: ModalController,
    private readonly errToast: ErrorToastService,
    private readonly actionCtrl: ActionSheetController,
    @Inject(AbstractMarketplaceService)
    private readonly marketplaceService: MarketplaceService,
    private readonly config: ConfigService,
    private readonly patch: PatchDB<DataModel>,
    private readonly destroy$: DestroyService,
    private readonly alertCtrl: AlertController,
  ) {}

  ngOnInit() {
    this.standardMarketplaces = Object.entries(this.config.marketplaces).map(
      ([id, val]) => {
        return { id, ...val }
      },
    )

    this.patch
      .watch$('ui', 'marketplace')
      .pipe(distinctUntilChanged(), takeUntil(this.destroy$))
      .subscribe(mp => {
        if (mp) {
          this.altMarketplaces = Object.entries(mp['known-hosts']).map(
            ([k, v]) => {
              return {
                id: k,
                name: v.name,
                url: v.url,
              }
            },
          )
          this.selectedId = mp['selected-id']
        } else {
          this.selectedId = 'start9'
        }
      })
  }

  async presentModalAdd() {
    const { name, spec } = getMarketplaceValueSpec()
    const modal = await this.modalCtrl.create({
      component: GenericFormPage,
      componentProps: {
        title: name,
        spec,
        buttons: [
          {
            text: 'Save for Later',
            handler: (value: { url: string }) => {
              this.saveOnly(new URL(value.url))
            },
          },
          {
            text: 'Save and Connect',
            handler: (value: { url: string }) => {
              this.saveAndConnect(new URL(value.url))
            },
            isSubmit: true,
          },
        ],
      },
      cssClass: 'alertlike-modal',
    })

    await modal.present()
  }

  async presentAction(id: string) {
    if (id === this.selectedId) return

    const buttons: ActionSheetButton[] = [
      {
        text: 'Connect',
        handler: () => {
          this.connect(id)
        },
      },
    ]

    if (id !== 'start9' && id !== 'community') {
      buttons.unshift({
        text: 'Delete',
        role: 'destructive',
        handler: () => {
          this.presentAlertDelete(id)
        },
      })
    }

    const action = await this.actionCtrl.create({
      header: this.getMarketplace(id).name,
      mode: 'ios',
      buttons,
    })

    await action.present()
  }

  private async presentAlertDelete(id: string) {
    const name = this.altMarketplaces.find(m => m.id === id)?.name

    const alert = await this.alertCtrl.create({
      header: 'Confirm',
      message: `Are you sure you want to delete ${name}?`,
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Delete',
          handler: () => this.delete(id),
          cssClass: 'enter-click',
        },
      ],
    })

    await alert.present()
  }

  private async connect(
    id: string,
    loader?: HTMLIonLoadingElement,
  ): Promise<void> {
    const message = 'Changing Marketplace...'
    if (!loader) {
      loader = await this.loadingCtrl.create({ message })
      await loader.present()
    } else {
      loader.message = message
    }

    try {
      await this.api.setDbValue({
        pointer: `/marketplace/selected-id`,
        value: id,
      })
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async saveOnly(url: URL): Promise<void> {
    const loader = await this.loadingCtrl.create()

    try {
      await this.validateAndSave(url, loader)
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async saveAndConnect(url: URL): Promise<void> {
    const loader = await this.loadingCtrl.create()

    try {
      const id = await this.validateAndSave(url, loader)
      await this.connect(id, loader)
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async validateAndSave(
    url: URL,
    loader: HTMLIonLoadingElement,
  ): Promise<string> {
    console.error('URL', url.toString(), url)
    // Error on duplicates
    const currentUrls = this.standardMarketplaces
      .concat(this.altMarketplaces)
      .map(mp => getUrlHostname(mp.url))
    if (currentUrls.includes(url.hostname))
      throw new Error('marketplace already added')

    // Validate
    loader.message = 'Validating marketplace...'
    await loader.present()

    const { id: serverId } = await getServerInfo(this.patch)
    const { name } = await this.marketplaceService.getMarketplaceData(
      { 'server-id': serverId },
      url.toString(),
    )

    // Save
    loader.message = 'Saving...'

    const id = v4()
    const marketplaces = this.altMarketplaces
      .concat({ id, name, url: url.toString() })
      .reduce((prev, curr) => {
        return {
          ...prev,
          [curr.id]: {
            name: curr.name,
            url: curr.url,
          },
        }
      }, {})
    await this.api.setDbValue({
      pointer: `/marketplace/known-hosts`,
      value: marketplaces,
    })
    return id
  }

  private async delete(id: string): Promise<void> {
    const loader = await this.loadingCtrl.create({
      message: 'Deleting...',
    })
    await loader.present()

    try {
      const filtered = this.altMarketplaces
        .filter(m => m.id !== id)
        .reduce((prev, curr) => {
          return {
            ...prev,
            [curr.id]: {
              name: curr.name,
              url: curr.url,
            },
          }
        }, {})
      await this.api.setDbValue({
        pointer: `/marketplace/known-hosts`,
        value: filtered,
      })
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private getMarketplace(id: string): MarketplaceWithId {
    return this.standardMarketplaces
      .concat(this.altMarketplaces)
      .find(m => m.id === id)!
  }
}

function getMarketplaceValueSpec(): ValueSpecObject {
  return {
    type: 'object',
    name: 'Add Marketplace',
    spec: {
      url: {
        type: 'string',
        name: 'URL',
        description: 'The fully-qualified URL of the alt marketplace.',
        nullable: false,
        masked: false,
        copyable: false,
        pattern: `https?:\/\/[a-zA-Z0-9][a-zA-Z0-9-\.]+[a-zA-Z0-9]\.[^\s]{2,}`,
        'pattern-description': 'Must be a valid URL',
        placeholder: 'e.g. https://example.org',
      },
    },
  }
}
