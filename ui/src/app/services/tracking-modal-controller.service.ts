import { Inject, Injectable } from '@angular/core'
import { Observable, Subject } from 'rxjs'
import { ModalController } from '@ionic/angular'
import { ModalOptions } from '@ionic/core'
import { APP_CONFIG_COMPONENT_MAPPING } from '../modals/app-config-injectable/modal-injectable-token'
import { AppConfigComponentMapping } from '../modals/app-config-injectable/modal-injectable-type'
import { ValueSpec } from '../app-config/config-types'

@Injectable({
  providedIn: 'root',
})
export class TrackingModalController {
  private modals: { [modalId: string] : HTMLIonModalElement} = { }

  private readonly $onDismiss$ = new Subject<string>()
  private readonly $onCreate$ = new Subject<string>()

  constructor (
    private readonly modalCtrl: ModalController,
    @Inject(APP_CONFIG_COMPONENT_MAPPING) private readonly appConfigComponentMapping: AppConfigComponentMapping,
  ) { }

  async createConfigModal (o: Omit<ModalOptions, 'component'>, type: ValueSpec['type']) {
    const component = this.appConfigComponentMapping[type]
    return this.create({ ...o, component })
  }

  async create (a: ModalOptions): Promise<HTMLIonModalElement> {
    const modal = await this.modalCtrl.create(a)
    this.modals[modal.id] = modal
    this.$onCreate$.next(modal.id)

    modal.onWillDismiss().then(() => {
      delete this.modals[modal.id]
      this.$onDismiss$.next(modal.id)
    })
    return modal
  }

  dismissAll (): Promise<boolean[]> {
    return Promise.all(
      Object.values(this.modals).map(m => m.dismiss()),
    )
  }


  dismiss (val?: any): Promise<boolean> {
    return this.modalCtrl.dismiss(val)
  }

  onCreateAny$ (): Observable<string> {
    return this.$onCreate$.asObservable()
  }

  onDismissAny$ (): Observable<string> {
    return this.$onDismiss$.asObservable()
  }

  async getTop (): Promise<HTMLIonModalElement> {
    return this.modalCtrl.getTop()
  }

  get anyModals (): boolean {
    return Object.keys(this.modals).length !== 0
  }

  get modalCount (): number {
    return Object.keys(this.modals).length
  }
}
