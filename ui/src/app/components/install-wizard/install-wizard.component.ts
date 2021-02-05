import { Component, Input, NgZone, OnInit, QueryList, ViewChild, ViewChildren } from '@angular/core'
import { IonContent, IonSlides, ModalController } from '@ionic/angular'
import { BehaviorSubject, from, Observable, of } from 'rxjs'
import { Cleanup } from 'src/app/util/cleanup'
import { capitalizeFirstLetter, pauseFor } from 'src/app/util/misc.util'
import { CompleteComponent } from './complete/complete.component'
import { DependenciesComponent } from './dependencies/dependencies.component'
import { DependentsComponent } from './dependents/dependents.component'
import { NotesComponent } from './notes/notes.component'
import { Loadable } from './loadable'
import { WizardAction } from './wizard-types'
import { concatMap, switchMap } from 'rxjs/operators'

@Component({
  selector: 'install-wizard',
  templateUrl: './install-wizard.component.html',
  styleUrls: ['./install-wizard.component.scss'],
})
export class InstallWizardComponent extends Cleanup implements OnInit {
  @Input() params: {
    // defines each slide along with bottom bar
    slideDefinitions: SlideDefinition[]
    toolbar: TopbarParams
  }

  // content container so we can scroll to top between slide transitions
  @ViewChild(IonContent) contentContainer: IonContent
  // slide container gives us hook into transitioning slides
  @ViewChild(IonSlides) slideContainer: IonSlides

  //a slide component gives us hook into a slide. Allows us to call load when slide comes into view
  @ViewChildren('components')
  slideComponentsQL: QueryList<Loadable>
  get slideComponents (): Loadable[] { return this.slideComponentsQL.toArray() }

  private slideIndex = 0
  get currentSlide (): Loadable {
    return this.slideComponents[this.slideIndex]
  }
  get currentSlideLoading$ (): Observable<boolean> {
    return this.$initializing$.pipe(switchMap(
      i => i ? of(true) : this.currentSlide.$loading$,
    ))
  }
  get currentBottomBar (): SlideDefinition['bottomBar'] {
    return this.params.slideDefinitions[this.slideIndex].bottomBar
  }

  $initializing$ = new BehaviorSubject(true)
  $error$ = new BehaviorSubject(undefined)

  constructor (private readonly modalController: ModalController, private readonly zone: NgZone) { super() }
  ngOnInit () { }

  ngAfterViewInit () {
    this.currentSlide.load()
    this.slideContainer.update()
    this.slideContainer.lockSwipes(true)
  }

  ionViewDidEnter () {
    this.$initializing$.next(false)
  }

  // process bottom bar buttons
  private transition = (info: { error?: Error, cancelled?: true, final?: true }) => {
    if (info.cancelled) this.currentSlide.$cancel$.next()
    if (info.final || info.cancelled) return this.modalController.dismiss(info)
    if (info.error) return this.$error$.next(capitalizeFirstLetter(info.error.message))

    this.moveToNextSlide()
  }

  // bottom bar button callbacks
  transitions = {
    cancel: () => this.transition({ cancelled: true }),
    next: () => this.transition({ }),
    final: () => this.transition({ final: true }),
    error: e => this.transition({ error: e }),
  }

  private async moveToNextSlide () {
    if (this.slideComponents[this.slideIndex + 1] === undefined) { return this.transition({ final: true }) }
    this.zone.run(async () => {
      this.slideComponents[this.slideIndex + 1].load()
      await pauseFor(50)
      this.slideIndex += 1
      await this.slideContainer.lockSwipes(false)
      await this.contentContainer.scrollToTop()
      await this.slideContainer.slideNext(500)
      await this.slideContainer.lockSwipes(true)
    })
  }
}

export interface SlideDefinition {
  slide:
    { selector: 'dependencies', params: DependenciesComponent['params'] } |
    { selector: 'dependents', params: DependentsComponent['params'] } |
    { selector: 'complete', params: CompleteComponent['params'] } |
    { selector: 'notes', params: NotesComponent['params'] }
  bottomBar: {
    cancel: {
      // indicates the existence of a cancel button, and whether to have text or an icon 'x' by default.
      afterLoading?: { text?: string },
      whileLoading?: { text?: string }
    }
    next?: string
    finish?: string
  }
}

export type TopbarParams = { action: WizardAction, title: string, version: string }

export async function wizardModal (
  modalController: ModalController, params: InstallWizardComponent['params'],
): Promise<{ cancelled?: true, final?: true, modal: HTMLIonModalElement }> {
  const modal = await modalController.create({
    backdropDismiss: false,
    cssClass: 'wizard-modal',
    component: InstallWizardComponent,
    componentProps: { params },
  })

  await modal.present()
  return modal.onWillDismiss().then(({ data }) => ({ ...data, modal }))
}
