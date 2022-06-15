import {
  Component,
  Input,
  QueryList,
  ViewChild,
  ViewChildren,
} from '@angular/core'
import { IonContent, ModalController } from '@ionic/angular'
import { CompleteComponent } from './complete/complete.component'
import { DependentsComponent } from './dependents/dependents.component'
import { AlertComponent } from './alert/alert.component'
import { WizardAction } from './wizard-types'
import SwiperCore, { Swiper } from 'swiper'
import { IonicSlides } from '@ionic/angular'
import { BaseSlide } from './wizard-types'

SwiperCore.use([IonicSlides])

@Component({
  selector: 'app-wizard',
  templateUrl: './app-wizard.component.html',
  styleUrls: ['./app-wizard.component.scss'],
})
export class AppWizardComponent {
  @Input() params: {
    action: WizardAction
    title: string
    slides: SlideDefinition[]
    submitBtn: string
    version?: string
  }

  // content container so we can scroll to top between slide transitions
  @ViewChild(IonContent) content: IonContent

  swiper: Swiper

  //a slide component gives us hook into a slide. Allows us to call load when slide comes into view
  @ViewChildren('components')
  slideComponentsQL: QueryList<BaseSlide>

  get slideComponents(): BaseSlide[] {
    return this.slideComponentsQL.toArray()
  }

  get currentSlide(): BaseSlide {
    return this.slideComponents[this.currentIndex]
  }

  get currentIndex(): number {
    return this.swiper.activeIndex
  }

  initializing = true
  error = ''

  constructor(private readonly modalController: ModalController) {}

  ionViewDidEnter() {
    this.initializing = false
    this.swiper.allowTouchMove = false
    this.loadSlide()
  }

  setSwiperInstance(swiper: any) {
    this.swiper = swiper
  }

  dismiss(role = 'cancelled') {
    this.modalController.dismiss(null, role)
  }

  async next() {
    await this.content.scrollToTop()
    this.swiper.slideNext(500)
  }

  setError(e: any) {
    this.error = e
  }

  async loadSlide() {
    this.currentSlide.load()
  }
}

export type SlideDefinition =
  | { selector: 'alert'; params: AlertComponent['params'] }
  | { selector: 'dependents'; params: DependentsComponent['params'] }
  | { selector: 'complete'; params: CompleteComponent['params'] }

export async function wizardModal(
  modalController: ModalController,
  params: AppWizardComponent['params'],
): Promise<boolean> {
  const modal = await modalController.create({
    backdropDismiss: false,
    cssClass: 'wizard-modal',
    component: AppWizardComponent,
    componentProps: { params },
  })

  await modal.present()
  return modal.onDidDismiss().then(({ role }) => role === 'success')
}
