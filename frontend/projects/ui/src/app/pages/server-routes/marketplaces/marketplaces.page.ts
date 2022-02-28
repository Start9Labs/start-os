import { Component } from '@angular/core'
import {
  ActionSheetController,
  LoadingController,
  ModalController,
} from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ActionSheetButton } from '@ionic/core'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { ValueSpecObject } from 'src/app/pkg-config/config-types'
import { GenericFormPage } from 'src/app/modals/generic-form/generic-form.page'
import { PatchDbService } from '../../../services/patch-db/patch-db.service'
import { v4 } from 'uuid'
import { MarketplaceService } from '../../marketplace-routes/marketplace.service'
import { UIMarketplaceData } from '../../../services/patch-db/data-model'
import { ConfigService } from '../../../services/config.service'

@Component({
  selector: 'marketplaces',
  templateUrl: 'marketplaces.page.html',
  styleUrls: ['marketplaces.page.scss'],
})
export class MarketplacesPage {
  selectedId: string | undefined
  marketplaces: { id: string | undefined; name: string; url: string }[] = []

  constructor(
    private readonly api: ApiService,
    private readonly loadingCtrl: LoadingController,
    private readonly modalCtrl: ModalController,
    private readonly errToast: ErrorToastService,
    private readonly actionCtrl: ActionSheetController,
    private readonly marketplaceService: MarketplaceService,
    private readonly config: ConfigService,
    public readonly patch: PatchDbService,
  ) {}

  ngOnInit() {
    this.patch.watch$('ui', 'marketplace').subscribe(mp => {
      const marketplaces = [
        {
          id: undefined,
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
        marketplaces.push.apply(marketplaces, alts)
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

  async presentAction(id: string) {
    // no need to view actions if is selected marketplace
    if (id === this.patch.getData().ui.marketplace?.['selected-id']) return

    const buttons: ActionSheetButton[] = [
      {
        text: 'Forget',
        icon: 'trash',
        role: 'destructive',
        handler: () => {
          this.delete(id)
        },
      },
      {
        text: 'Connect to marketplace',
        handler: () => {
          this.connect(id)
        },
      },
    ]

    if (!id) {
      buttons.shift()
    }

    const action = await this.actionCtrl.create({
      header: id,
      subHeader: 'Manage marketplaces',
      mode: 'ios',
      buttons,
    })

    await action.present()
  }

  private async connect(id: string): Promise<void> {
    const marketplace: UIMarketplaceData = JSON.parse(
      JSON.stringify(this.patch.getData().ui.marketplace),
    )

    const url = id
      ? marketplace['known-hosts'][id].url
      : this.config.marketplace.url

    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Validating Marketplace...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.marketplaceService.getMarketplaceData(
        { 'server-id': this.patch.getData()['server-info'].id },
        url,
      )
    } catch (e) {
      this.errToast.present(e)
      loader.dismiss()
      return
    }

    loader.message = 'Changing Marketplace...'

    try {
      marketplace['selected-id'] = id
      await this.api.setDbValue({ pointer: `/marketplace`, value: marketplace })
    } catch (e) {
      this.errToast.present(e)
      loader.dismiss()
    }

    loader.message = 'Syncing store...'

    try {
      await this.marketplaceService.load()
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async delete(id: string): Promise<void> {
    if (!id) return
    const marketplace: UIMarketplaceData = JSON.parse(
      JSON.stringify(this.patch.getData().ui.marketplace),
    )

    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Deleting...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      delete marketplace['known-hosts'][id]
      await this.api.setDbValue({ pointer: `/marketplace`, value: marketplace })
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async save(url: string): Promise<void> {
    const marketplace = this.patch.getData().ui.marketplace
      ? (JSON.parse(
          JSON.stringify(this.patch.getData().ui.marketplace),
        ) as UIMarketplaceData)
      : { 'selected-id': undefined, 'known-hosts': {} }

    // no-op on duplicates
    const currentUrls = this.marketplaces.map(mp => mp.url)
    if (currentUrls.includes(new URL(url).hostname)) return

    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Validating Marketplace...',
      cssClass: 'loader',
    })

    await loader.present()

    try {
      const id = v4()
      const { name } = await this.marketplaceService.getMarketplaceData(
        { 'server-id': this.patch.getData()['server-info'].id },
        url,
      )
      marketplace['known-hosts'][id] = { name, url }
    } catch (e) {
      this.errToast.present(e)
      loader.dismiss()
      return
    }

    loader.message = 'Saving...'

    try {
      await this.api.setDbValue({ pointer: `/marketplace`, value: marketplace })
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async saveAndConnect(url: string): Promise<void> {
    await this.save(url)

    const marketplace = this.patch.getData().ui.marketplace
      ? (JSON.parse(
          JSON.stringify(this.patch.getData().ui.marketplace),
        ) as UIMarketplaceData)
      : { 'selected-id': undefined, 'known-hosts': {} }

    // no-op on duplicates
    const currentUrls = this.marketplaces.map(mp => mp.url)
    if (currentUrls.includes(new URL(url).hostname)) return

    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Validating Marketplace...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      const id = v4()
      const { name } = await this.marketplaceService.getMarketplaceData(
        { 'server-id': this.patch.getData()['server-info'].id },
        url,
      )
      marketplace['known-hosts'][id] = { name, url }
      marketplace['selected-id'] = id
    } catch (e) {
      this.errToast.present(e)
      loader.dismiss()
      return
    }

    loader.message = 'Saving...'

    try {
      await this.api.setDbValue({ pointer: `/marketplace`, value: marketplace })
    } catch (e) {
      this.errToast.present(e)
      loader.dismiss()
      return
    }

    loader.message = 'Syncing marketplace data...'

    try {
      await this.marketplaceService.load()
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }
}

function getMarketplaceValueSpec(): ValueSpecObject {
  return {
    type: 'object',
    name: 'Add Marketplace',
    'unique-by': null,
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
