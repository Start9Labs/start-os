import {
  AfterViewInit,
  ChangeDetectionStrategy,
  Component,
  ContentChildren,
  ElementRef,
  EventEmitter,
  Input,
  OnDestroy,
  Output,
  QueryList,
  ViewChild,
} from '@angular/core'
import { AlertController, AlertOptions, IonicSafeString } from '@ionic/angular'
import { OverlayEventDetail } from '@ionic/core'
import { AlertButtonDirective } from './alert-button.directive'
import { AlertInputDirective } from './alert-input.directive'

@Component({
  selector: 'alert',
  template: `
    <div #message><ng-content></ng-content></div>
    <ng-content select="[alertInput]"></ng-content>
    <ng-content select="[alertButton]"></ng-content>
  `,
  styles: [':host { display: none !important; }'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AlertComponent<T> implements AfterViewInit, OnDestroy {
  @Output()
  readonly dismiss = new EventEmitter<OverlayEventDetail<T>>()

  @Input()
  header = ''

  @Input()
  subHeader = ''

  @Input()
  backdropDismiss = true

  @ViewChild('message', { static: true })
  private readonly content?: ElementRef<HTMLElement>

  @ContentChildren(AlertButtonDirective)
  private readonly buttons: QueryList<AlertButtonDirective> = new QueryList()

  @ContentChildren(AlertInputDirective)
  private readonly inputs: QueryList<AlertInputDirective<any>> = new QueryList()

  private alert?: HTMLIonAlertElement

  constructor(
    private readonly elementRef: ElementRef<HTMLElement>,
    private readonly controller: AlertController,
  ) {}

  get cssClass(): string[] {
    return Array.from(this.elementRef.nativeElement.classList)
  }

  get message(): IonicSafeString {
    return new IonicSafeString(this.content?.nativeElement.innerHTML || '')
  }

  async ngAfterViewInit() {
    this.alert = await this.controller.create(this.getOptions())
    this.alert.onDidDismiss().then(event => {
      this.dismiss.emit(event)
    })

    await this.alert.present()
  }

  async ngOnDestroy() {
    await this.alert?.dismiss()
  }

  private getOptions(): AlertOptions {
    const {
      header,
      subHeader,
      message,
      cssClass,
      buttons,
      inputs,
      backdropDismiss,
    } = this
    return {
      header,
      subHeader,
      message,
      cssClass,
      backdropDismiss,
      buttons: buttons.toArray(),
      inputs: inputs.toArray(),
    }
  }
}
