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
    if (id === this.patch.data.ui.marketplace['selected-id']) return

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
      JSON.stringify(this.patch.data.ui.marketplace),
    )
    const newMarketplace = marketplace['known-hosts'][id]

    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Validating Marketplace...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.api.getMarketplaceData({}, newMarketplace.url)
    } catch (e) {
      this.errToast.present({
        message: `Could not connect to ${newMarketplace.url}`,
      } as any)
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
      this.errToast.present({
        message: `Error syncing marketplace data`,
      } as any)
    } finally {
      loader.dismiss()
    }
  }

  private async delete(id: string): Promise<void> {
    const marketplace: UIMarketplaceData = JSON.parse(
      JSON.stringify(this.patch.data.ui.marketplace),
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
    const marketplace = JSON.parse(
      JSON.stringify(this.patch.data.ui.marketplace),
    ) as UIMarketplaceData

    // no-op on duplicates
    const currentUrls = Object.values(marketplace['known-hosts']).map(
      u => new URL(u.url).hostname,
    )
    if (currentUrls.includes(new URL(url).hostname)) return

    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Validating Marketplace...',
      cssClass: 'loader',
    })

    await loader.present()

    try {
      const id = v4()
      const { name } = await this.api.getMarketplaceData({}, url)
      marketplace['known-hosts'][id] = { name, url }
    } catch (e) {
      this.errToast.present({ message: `Could not connect to ${url}` } as any)
      loader.dismiss()
      return
    }

    loader.message = 'Saving...'

    try {
      await this.api.setDbValue({ pointer: `/marketplace`, value: marketplace })
    } catch (e) {
      this.errToast.present({ message: `Error saving marketplace data` } as any)
    } finally {
      loader.dismiss()
    }
  }

  private async saveAndConnect(url: string): Promise<void> {
    const marketplace = JSON.parse(
      JSON.stringify(this.patch.data.ui.marketplace),
    ) as UIMarketplaceData

    // no-op on duplicates
    const currentUrls = Object.values(marketplace['known-hosts']).map(
      u => new URL(u.url).hostname,
    )
    if (currentUrls.includes(new URL(url).hostname)) return

    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Validating Marketplace...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      const id = v4()
      const { name } = await this.api.getMarketplaceData({}, url)
      marketplace['known-hosts'][id] = { name, url }
      marketplace['selected-id'] = id
    } catch (e) {
      this.errToast.present({ message: `Could not connect to ${url}` } as any)
      loader.dismiss()
      return
    }

    loader.message = 'Saving...'

    try {
      await this.api.setDbValue({ pointer: `/marketplace`, value: marketplace })
    } catch (e) {
      this.errToast.present({ message: `Error saving marketplace data` } as any)
      loader.dismiss()
      return
    }

    loader.message = 'Syncing store...'

    try {
      await this.marketplaceService.load()
    } catch (e) {
      this.errToast.present({
        message: `Error syncing marketplace data`,
      } as any)
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
        description: 'The fully-qualified URL of the alternative marketplace.',
        nullable: false,
        masked: false,
        copyable: false,
        pattern: `https?:\/\/[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\.[^\s]{2,}`,
        'pattern-description': 'Must be a valid URL',
        placeholder: 'e.g. https://example.org',
      },
    },
  }
}
