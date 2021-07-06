import { ConfigCursor } from './config-cursor'
import { TrackingModalController } from '../services/tracking-modal-controller.service'

export class ModalPresentable {

  constructor (
    private readonly trackingModalCtrl: TrackingModalController,
  ) { }

  async presentModal (cursor: ConfigCursor<any>, callback: () => any) {
    const modal = await this.trackingModalCtrl.createConfigModal({
      backdropDismiss: false,
      presentingElement: await this.trackingModalCtrl.getTop(),
      componentProps: {
        cursor,
      },
    }, cursor.spec().type)

    modal.onWillDismiss().then(res => {
      cursor.injectModalData(res)
      callback()
    })

    await modal.present()
  }

  dismissModal (a: any) {
    return this.trackingModalCtrl.dismiss(a)
  }
}
