import { Injectable } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { ModalController } from '@ionic/angular'
import { DependentInfo } from 'src/app/util/misc.util'
import { AppConfigPage } from 'src/app/modals/app-config/app-config.page'

@Injectable({
  providedIn: 'root',
})
export class ModalService {
  constructor(
    private readonly route: ActivatedRoute,
    private readonly modalCtrl: ModalController,
  ) {}

  async presentModalConfig(componentProps: ComponentProps): Promise<void> {
    const modal = await this.modalCtrl.create({
      component: AppConfigPage,
      componentProps,
    })
    await modal.present()
  }
}

interface ComponentProps {
  pkgId: string
  dependentInfo?: DependentInfo
}
