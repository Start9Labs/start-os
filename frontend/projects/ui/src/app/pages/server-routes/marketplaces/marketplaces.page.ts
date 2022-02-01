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
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Connecting...',
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
      await this.api.setDbValue({ pointer: `/marketplace`, value: marketplace })
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async saveAndConnect(url: string): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Connecting...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      const id = v4()
      const { name } = await this.api.getMarketplaceData({})
      loader.message = 'Saving...'
      const marketplace = this.patch.data.ui.marketplace
      marketplace.options[id] = { name, url }
      marketplace['selected-id'] = id
      await this.api.setDbValue({ pointer: `/marketplace`, value: marketplace })
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
        description: 'The fully-qualified URL of the alternative marketplace.',
        nullable: false,
        masked: false,
        copyable: false,
        placeholder: 'e.g. https://example.org',
      },
    },
  }
}
