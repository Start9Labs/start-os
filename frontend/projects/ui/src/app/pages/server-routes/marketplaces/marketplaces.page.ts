import { Component } from '@angular/core'
import {
  ActionSheetController,
  AlertController,
  LoadingController,
  ModalController,
  ToastController,
} from '@ionic/angular'
import { AlertInput } from '@ionic/core'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ActionSheetButton } from '@ionic/core'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { ValueSpecObject } from 'src/app/pkg-config/config-types'
import { RR } from 'src/app/services/api/api.types'
import { pauseFor } from 'src/app/util/misc.util'
import { GenericFormPage } from 'src/app/modals/generic-form/generic-form.page'
import { ConfigService } from 'src/app/services/config.service'
import { PatchDbService } from '../../../services/patch-db/patch-db.service'
import { v4 } from 'uuid'

@Component({
  selector: 'marketplaces',
  templateUrl: 'marketplaces.page.html',
  styleUrls: ['marketplaces.page.scss'],
})
export class MarketplacesPage {
  constructor(
    private readonly api: ApiService,
    private readonly toastCtrl: ToastController,
    private readonly alertCtrl: AlertController,
    private readonly loadingCtrl: LoadingController,
    private readonly modalCtrl: ModalController,
    private readonly errToast: ErrorToastService,
    private readonly actionCtrl: ActionSheetController,
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
            handler: async (value: { url: string }) => {
              await this.save(value.url)
            },
          },
          {
            text: 'Save and Use',
            handler: async (value: { url: string }) => {
              await this.saveAndUse(value.url)
            },
            isSubmit: true,
          },
        ],
      },
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
        text: 'Use Marketplace',
        icon: 'storefront',
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
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Verifying marketplace',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      const marketplace = this.patch.data.ui.marketplace
      marketplace['selected-id'] = id
      await this.api.setDbValue({ pointer: `marketplace`, value: marketplace })
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async delete(id: string): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Deleting...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      const marketplace = this.patch.data.ui.marketplace
      delete marketplace.options[id]
      await this.api.setDbValue({ pointer: `marketplace`, value: marketplace })
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async save(url: string): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Saving...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      const id = v4()
      const { name } = await this.api.getMarketplaceData({})
      const marketplace = this.patch.data.ui.marketplace
      marketplace.options[id] = { name, url }
      await this.api.setDbValue({ pointer: `marketplace`, value: marketplace })
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async saveAndUse(url: string): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Connecting. This could take a while...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      const id = v4()
      const { name } = await this.api.getMarketplaceData({})
      const marketplace = this.patch.data.ui.marketplace
      marketplace.options[id] = { name, url }
      marketplace['selected-id'] = id
      await this.api.setDbValue({ pointer: `marketplace`, value: marketplace })
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
    description: 'Enter marketplace info.',
    'unique-by': null,
    spec: {
      url: {
        type: 'string',
        name: 'URL',
        nullable: false,
        masked: false,
        copyable: false,
      },
    },
  }
}
