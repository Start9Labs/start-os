import { Component } from '@angular/core'
import { AlertController } from '@ionic/angular'
import { ErrorToastService } from '@start9labs/shared'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TuiDialogOptions } from '@taiga-ui/core'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { DomainForm, domainSpec } from './domain.const'
import { ConnectionService } from 'src/app/services/connection.service'
import { filter, switchMap } from 'rxjs'
import { LoadingService } from 'src/app/common/loading/loading.service'
import { FormContext, FormPage } from '../../../modals/form/form.page'

@Component({
  selector: 'domains',
  templateUrl: 'domains.page.html',
  styleUrls: ['domains.page.scss'],
})
export class DomainsPage {
  readonly docsUrl = 'https://docs.start9.com/latest/user-manual/domains'
  readonly server$ = this.connectionService.connected$.pipe(
    filter(Boolean),
    switchMap(() => this.patch.watch$('server-info')),
  )

  constructor(
    private readonly errToast: ErrorToastService,
    private readonly alertCtrl: AlertController,
    private readonly api: ApiService,
    private readonly loader: LoadingService,
    private readonly formDialog: FormDialogService,
    private readonly connectionService: ConnectionService,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  presentModalAdd() {
    const options: Partial<TuiDialogOptions<FormContext<DomainForm>>> = {
      label: 'Custom Domain',
      data: {
        spec: domainSpec,
        buttons: [
          {
            text: 'Save',
            handler: async value => this.save(value),
          },
        ],
      },
    }
    this.formDialog.open(FormPage, options)
  }

  async presentAlertClaimStart9MeDomain() {
    const alert = await this.alertCtrl.create({
      header: 'Confirm',
      message: 'Claim your start9.me domain?',
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Claim',
          handler: () => {
            this.claimStart9MeDomain()
          },
          cssClass: 'enter-click',
        },
      ],
    })
    await alert.present()
  }

  async presentAlertDelete(hostname: string) {
    const alert = await this.alertCtrl.create({
      header: 'Confirm',
      message: 'Delete domain?',
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Delete',
          handler: () => {
            this.delete(hostname)
          },
          cssClass: 'enter-click',
        },
      ],
    })
    await alert.present()
  }

  async presentAlertDeleteStart9Me() {
    const alert = await this.alertCtrl.create({
      header: 'Confirm',
      message: 'Delete start9.me domain?',
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Delete',
          handler: () => {
            this.deleteStart9MeDomain()
          },
          cssClass: 'enter-click',
        },
      ],
    })
    await alert.present()
  }

  private async claimStart9MeDomain(): Promise<boolean> {
    const loader = this.loader.open('Saving...').subscribe()

    try {
      await this.api.claimStart9MeDomain({})
      return true
    } catch (e: any) {
      this.errToast.present(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }

  private async save(value: DomainForm): Promise<boolean> {
    const loader = this.loader.open('Saving...').subscribe()

    try {
      await this.api.addDomain(value)
      return true
    } catch (e: any) {
      this.errToast.present(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }

  private async delete(hostname: string): Promise<void> {
    const loader = this.loader.open('Deleting...').subscribe()

    try {
      await this.api.deleteDomain({ hostname })
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async deleteStart9MeDomain(): Promise<void> {
    const loader = this.loader.open('Deleting...').subscribe()

    try {
      await this.api.deleteStart9MeDomain({})
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
