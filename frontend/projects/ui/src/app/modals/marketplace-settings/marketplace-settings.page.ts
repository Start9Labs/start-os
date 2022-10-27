import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import {
  ActionSheetController,
  AlertController,
  LoadingController,
  ModalController,
} from '@ionic/angular'
import { ActionSheetButton } from '@ionic/core'
import { ErrorToastService } from '@start9labs/shared'
import { AbstractMarketplaceService } from '@start9labs/marketplace'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ValueSpecObject } from 'src/app/pkg-config/config-types'
import { GenericFormPage } from 'src/app/modals/generic-form/generic-form.page'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { map } from 'rxjs/operators'
import { firstValueFrom } from 'rxjs'

@Component({
  selector: 'marketplace-settings',
  templateUrl: 'marketplace-settings.page.html',
  styleUrls: ['marketplace-settings.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MarketplaceSettingsPage {
  marketplace$ = this.patch.watch$('ui', 'marketplace').pipe(
    map(m => {
      const selected = m['selected-url']
      const hosts = Object.entries(m['known-hosts'])

      const standard = hosts
        .map(([url, name]) => {
          return { url, name }
        })
        .slice(0, 2) // 0 and 1 will always be prod and community

      const alt = hosts
        .map(([url, name]) => {
          return { url, name }
        })
        .slice(2) // 2 and beyond will always be alts

      return { selected, standard, alt }
    }),
  )

  constructor(
    private readonly api: ApiService,
    private readonly loadingCtrl: LoadingController,
    private readonly modalCtrl: ModalController,
    private readonly errToast: ErrorToastService,
    private readonly actionCtrl: ActionSheetController,
    @Inject(AbstractMarketplaceService)
    private readonly marketplaceService: MarketplaceService,
    private readonly patch: PatchDB<DataModel>,
    private readonly alertCtrl: AlertController,
  ) {}

  async dismiss() {
    this.modalCtrl.dismiss()
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

  async presentAction(
    { url, name }: { url: string; name: string },
    canDelete = false,
  ) {
    const buttons: ActionSheetButton[] = [
      {
        text: 'Connect',
        handler: () => {
          this.connect(url)
        },
      },
    ]

    if (canDelete) {
      buttons.unshift({
        text: 'Delete',
        role: 'destructive',
        handler: () => {
          this.presentAlertDelete(url, name)
        },
      })
    }

    const action = await this.actionCtrl.create({
      header: name,
      mode: 'ios',
      buttons,
    })

    await action.present()
  }

  private async presentAlertDelete(url: string, name: string) {
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
          handler: () => this.delete(url),
          cssClass: 'enter-click',
        },
      ],
    })

    await alert.present()
  }

  private async connect(
    url: string,
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
      await this.api.setDbValue(['marketplace', 'selected-url'], url)
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
      await this.validateAndSave(url, loader)
      await this.connect(url.toString(), loader)
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async validateAndSave(
    urlObj: URL,
    loader: HTMLIonLoadingElement,
  ): Promise<void> {
    const url = urlObj.toString()
    // Error on duplicates
    const hosts = await firstValueFrom(
      this.patch.watch$('ui', 'marketplace', 'known-hosts'),
    )
    const currentUrls = Object.keys(hosts)
    if (currentUrls.includes(url)) throw new Error('marketplace already added')

    // Validate
    loader.message = 'Validating marketplace...'
    await loader.present()

    const { name } = await firstValueFrom(
      this.marketplaceService.fetchInfo$(url),
    )

    // Save
    loader.message = 'Saving...'

    await this.api.setDbValue(['marketplace', 'known-hosts', url], name)
  }

  private async delete(url: string): Promise<void> {
    const loader = await this.loadingCtrl.create({
      message: 'Deleting...',
    })
    await loader.present()

    const hosts = await firstValueFrom(
      this.patch.watch$('ui', 'marketplace', 'known-hosts'),
    )

    const filtered = Object.keys(hosts)
      .filter(key => key !== url)
      .reduce((prev, curr) => {
        const name = hosts[curr]
        return {
          ...prev,
          [curr]: name,
        }
      }, {})

    try {
      await this.api.setDbValue(['marketplace', 'known-hosts'], filtered)
    } catch (e: any) {
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
