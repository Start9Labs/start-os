import { Component } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { IonicModule, ModalController } from '@ionic/angular'
import { TuiInputPasswordModule } from '@taiga-ui/kit'

@Component({
  standalone: true,
  template: `
    <ion-header>
      <ion-toolbar>
        <ion-title>Decrypt Backup</ion-title>
        <ion-buttons slot="end">
          <ion-button (click)="cancel()">
            <ion-icon slot="icon-only" name="close"></ion-icon>
          </ion-button>
        </ion-buttons>
      </ion-toolbar>
    </ion-header>

    <ion-content class="ion-padding">
      <p>
        Enter the password that was used to encrypt this backup. On the next
        screen, you will select the individual services you want to restore.
      </p>
      <p>
        <tui-input-password [(ngModel)]="password">
          Enter password
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
          Next
        </ion-button>
      </ion-toolbar>
    </ion-footer>
  `,
  imports: [IonicModule, FormsModule, TuiInputPasswordModule],
})
export class PasswordPromptModal {
  password = ''

  constructor(private modalCtrl: ModalController) {}

  cancel() {
    return this.modalCtrl.dismiss(null, 'cancel')
  }

  confirm() {
    return this.modalCtrl.dismiss(this.password, 'confirm')
  }
}
