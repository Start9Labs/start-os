import {
  AfterViewInit,
  Component,
  ElementRef,
  Input,
  ViewChild,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { IonicModule, ModalController } from '@ionic/angular'
import { TuiTextfieldComponent } from '@taiga-ui/core'
import { TuiInputPasswordModule } from '@taiga-ui/kit'

export interface PromptOptions {
  title: string
  message: string
  label: string
  placeholder: string
  buttonText: string
}

@Component({
  standalone: true,
  template: `
    <ion-header>
      <ion-toolbar>
        <ion-title>{{ options.title }}</ion-title>
        <ion-buttons slot="end">
          <ion-button (click)="cancel()">
            <ion-icon slot="icon-only" name="close"></ion-icon>
          </ion-button>
        </ion-buttons>
      </ion-toolbar>
    </ion-header>

    <ion-content class="ion-padding">
      <p>{{ options.message }}</p>
      <p>
        <tui-input-password [(ngModel)]="password" (keydown.enter)="confirm()">
          {{ options.label }}
          <input tuiTextfield [placeholder]="options.placeholder" />
        </tui-input-password>
      </p>
    </ion-content>

    <ion-footer>
      <ion-toolbar>
        <ion-button
          class="ion-padding-end"
          slot="end"
          color="dark"
          (click)="cancel()"
        >
          Cancel
        </ion-button>
        <ion-button
          class="ion-padding-end"
          slot="end"
          color="primary"
          strong="true"
          [disabled]="!password"
          (click)="confirm()"
        >
          {{ options.buttonText }}
        </ion-button>
      </ion-toolbar>
    </ion-footer>
  `,
  imports: [IonicModule, FormsModule, TuiInputPasswordModule],
})
export class PasswordPromptComponent implements AfterViewInit {
  @ViewChild(TuiTextfieldComponent, { read: ElementRef })
  input?: ElementRef<HTMLInputElement>

  @Input()
  options!: PromptOptions

  password = ''

  constructor(private modalCtrl: ModalController) {}

  ngAfterViewInit() {
    setTimeout(() => {
      this.input?.nativeElement.focus({ preventScroll: true })
    }, 300)
  }

  cancel() {
    return this.modalCtrl.dismiss(null, 'cancel')
  }

  confirm() {
    return this.modalCtrl.dismiss(this.password, 'confirm')
  }
}
