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
  MarketplaceData,
} from '@start9labs/marketplace'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ValueSpecObject } from 'src/app/pkg-config/config-types'
import { GenericFormPage } from 'src/app/modals/generic-form/generic-form.page'
import { PatchDB } from 'patch-db-client'
import { v4 } from 'uuid'
import {
  DataModel,
  UIMarketplaceData,
} from '../../../services/patch-db/data-model'
import { ConfigService } from '../../../services/config.service'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import {
  distinctUntilChanged,
  finalize,
  first,
  takeUntil,
} from 'rxjs/operators'
import { getServerInfo } from '../../../util/get-server-info'
import { getMarketplace } from '../../../util/get-marketplace'

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
  loader?: HTMLIonLoadingElement

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
      })
  }

  async presentModalAdd() {
    const marketplaceSpec = getMarketplaceValueSpec()
    const modal = await this.modalCtrl.create({
      component: GenericFormPage,
      componentProps: {
        title: marketplaceSpec.name,
        spec: marketplaceSpec.spec,
        buttons: [
          {
            text: 'Save for Later',
            handler: (value: { url: string }) => {
              this.saveOnly(value.url)
            },
          },
          {
            text: 'Save and Connect',
            handler: (value: { url: string }) => {
              this.saveAndConnect(value.url)
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

  private async validate(url: string): Promise<MarketplaceData> {
    // Error on duplicates
    const currentUrls = this.standardMarketplaces
      .concat(this.altMarketplaces)
      .map(mp => getUrlHostname(mp.url))
    if (currentUrls.includes(getUrlHostname(url)))
      throw new Error('marketplace already added')

    this.loader = await this.loadingCtrl.create({
      message: 'Validating Marketplace...',
    })
    await this.loader.present()

    const { id: serverId } = await getServerInfo(this.patch)
    return this.marketplaceService.getMarketplaceData(
      { 'server-id': serverId },
      url,
    )
  }

  private async save(url: string): Promise<string> {
    const { name } = await this.validate(url)
    if (this.loader) this.loader.message = 'Saving...'
    const id = v4()
    const marketplaces = this.altMarketplaces
      .concat({ id, name, url })
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

  private async connect(id: string): Promise<void> {
    const marketplace = this.getMarketplace(id)

    try {
      await this.validate(marketplace.url)
      if (this.loader) this.loader.message = 'Changing Marketplace...'
      await this.api.setDbValue({
        pointer: `/marketplace/selected-id`,
        value: id,
      })
      if (this.loader) this.loader.message = 'Syncing...'
      this.marketplaceService
        .getPackages()
        .pipe(
          first(),
          finalize(() => this.loader?.dismiss()),
        )
        .subscribe()
    } catch (e: any) {
      this.errToast.present(e)
      this.loader?.dismiss()
      return
    }
  }

  private async saveOnly(url: string): Promise<void> {
    try {
      await this.save(url)
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      this.loader?.dismiss()
    }
  }

  private async saveAndConnect(url: string): Promise<void> {
    try {
      const id = await this.save(url)
      await this.connect(id)
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      this.loader?.dismiss()
    }
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

  private dismissLoader() {
    if (this.loader) {
      this.loader.dismiss()
      this.loader = undefined
    }
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
