import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { BackupsComponent } from './backups.component'
import { BackupColorPipeModule } from 'src/app/pipes/backup-color/backup-color.module'
import { BackupColorPipe } from 'src/app/pipes/backup-color/backup-color.pipe'
import { TuiSvgModule } from '@taiga-ui/core'
import { TUI_SANITIZER } from '@taiga-ui/core'
import { NgDompurifySanitizer } from '@tinkoff/ng-dompurify'

@NgModule({
  imports: [CommonModule, IonicModule, BackupColorPipeModule, TuiSvgModule],
  declarations: [BackupsComponent],
  providers: [
    BackupColorPipe,
    {
      provide: TUI_SANITIZER,
      useClass: NgDompurifySanitizer,
    },
  ],
  exports: [BackupsComponent],
})
export class BackupsModule {}
