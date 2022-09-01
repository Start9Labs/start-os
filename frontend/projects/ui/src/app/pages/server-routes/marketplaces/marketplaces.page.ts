import { Component, Inject } from '@angular/core'
import {
  ActionSheetController,
  AlertController,
  LoadingController,
  ModalController,
} from '@ionic/angular'
import { ActionSheetButton } from '@ionic/core'
import { DestroyService, ErrorToastService } from '@start9labs/shared'
import { AbstractMarketplaceService } from '@start9labs/marketplace'
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

type Marketplaces = {
  id: string | null
  name: string
  url: string
}[]

@Component({
  selector: 'marketplaces',
  templateUrl: 'marketplaces.page.html',
  styleUrls: ['marketplaces.page.scss'],
  providers: [DestroyService],
})
export class MarketplacesPage {
  selectedId: string | null = null
  marketplaces: Marketplaces = []

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
    this.patch
      .watch$('ui', 'marketplace')
      .pipe(distinctUntilChanged(), takeUntil(this.destroy$))
      .subscribe((mp: UIMarketplaceData | undefined) => {
        let marketplaces: Marketplaces = [
          {
            id: null,
            name: this.config.marketplace.name,
            url: this.config.marketplace.url,
          },
        ]
        if (mp) {
          this.selectedId = mp['selected-id']
          const alts = Object.entries(mp['known-hosts']).map(([k, v]) => {
            return {
              id: k,
              name: v.name,
              url: v.url,
            }
          })
          marketplaces = marketplaces.concat(alts)
        }
        this.marketplaces = marketplaces
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
              this.save(value.url)
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

  async presentAction(id: string | null) {
    // no need to view actions if is selected marketplace
    const marketplace = await getMarketplace(this.patch)

    if (id === marketplace['selected-id']) return

    const buttons: ActionSheetButton[] = [
      {
        text: 'Connect',
        handler: () => {
          this.connect(id)
        },
      },
    ]

    if (id) {
      buttons.unshift({
        text: 'Delete',
        role: 'destructive',
        handler: () => {
          this.presentAlertDelete(id)
        },
      })
    }

    const action = await this.actionCtrl.create({
      header: this.marketplaces.find(mp => mp.id === id)?.name,
      mode: 'ios',
      buttons,
    })

    await action.present()
  }

  private async connect(id: string | null): Promise<void> {
    const marketplace = await getMarketplace(this.patch)

    const url = id
      ? marketplace['known-hosts'][id].url
      : this.config.marketplace.url

    const loader = await this.loadingCtrl.create({
      message: 'Validating Marketplace...',
    })
    await loader.present()

    try {
      const { id } = await getServerInfo(this.patch)
      await this.marketplaceService.getMarketplaceData({ 'server-id': id }, url)
    } catch (e: any) {
      this.errToast.present(e)
      loader.dismiss()
      return
    }

    loader.message = 'Changing Marketplace...'

    const value: UIMarketplaceData = {
      ...marketplace,
      'selected-id': id,
    }

    try {
      await this.api.setDbValue({ pointer: `/marketplace`, value })
    } catch (e: any) {
      this.errToast.present(e)
      loader.dismiss()
    }

    loader.message = 'Syncing store...'

    this.marketplaceService
      .getPackages()
      .pipe(
        first(),
        finalize(() => loader.dismiss()),
      )
      .subscribe()
  }

  private async presentAlertDelete(id: string) {
    const name = this.marketplaces.find(m => m.id === id)?.name

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

  private async delete(id: string): Promise<void> {
    const data = await getMarketplace(this.patch)
    const marketplace: UIMarketplaceData = JSON.parse(JSON.stringify(data))

    const loader = await this.loadingCtrl.create({
      message: 'Deleting...',
    })
    await loader.present()

    try {
      delete marketplace['known-hosts'][id]
      await this.api.setDbValue({ pointer: `/marketplace`, value: marketplace })
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async save(url: string): Promise<void> {
    const data = await getMarketplace(this.patch)
    const marketplace: UIMarketplaceData = data
      ? JSON.parse(JSON.stringify(data))
      : {
          'selected-id': null,
          'known-hosts': {},
        }

    // no-op on duplicates
    const currentUrls = this.marketplaces.map(mp => mp.url)
    if (currentUrls.includes(new URL(url).hostname)) return

    const loader = await this.loadingCtrl.create({
      message: 'Validating Marketplace...',
    })

    await loader.present()

    try {
      const id = v4()
      const { id: serverId } = await getServerInfo(this.patch)
      const { name } = await this.marketplaceService.getMarketplaceData(
        { 'server-id': serverId },
        url,
      )
      marketplace['known-hosts'][id] = { name, url }
    } catch (e: any) {
      this.errToast.present(e)
      loader.dismiss()
      return
    }

    loader.message = 'Saving...'

    try {
      await this.api.setDbValue({ pointer: `/marketplace`, value: marketplace })
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async saveAndConnect(url: string): Promise<void> {
    const data = await getMarketplace(this.patch)
    const marketplace: UIMarketplaceData = data
      ? JSON.parse(JSON.stringify(data))
      : {
          'selected-id': null,
          'known-hosts': {},
        }

    // no-op on duplicates
    const currentUrls = this.marketplaces.map(mp => mp.url)
    if (currentUrls.includes(new URL(url).hostname)) return

    const loader = await this.loadingCtrl.create({
      message: 'Validating Marketplace...',
    })
    await loader.present()

    try {
      const id = v4()
      const { id: serverId } = await getServerInfo(this.patch)
      const { name } = await this.marketplaceService.getMarketplaceData(
        { 'server-id': serverId },
        url,
      )
      marketplace['known-hosts'][id] = { name, url }
      marketplace['selected-id'] = id
    } catch (e: any) {
      this.errToast.present(e)
      loader.dismiss()
      return
    }

    loader.message = 'Saving...'

    try {
      await this.api.setDbValue({ pointer: `/marketplace`, value: marketplace })
    } catch (e: any) {
      this.errToast.present(e)
      loader.dismiss()
      return
    }

    loader.message = 'Syncing marketplace data...'

    this.marketplaceService
      .getPackages()
      .pipe(
        first(),
        finalize(() => loader.dismiss()),
      )
      .subscribe()
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
