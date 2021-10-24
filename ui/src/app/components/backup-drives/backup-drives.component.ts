import { Component, EventEmitter, Input, Output } from '@angular/core'
import { BackupService } from './backup.service'
import { MappedPartitionInfo } from 'src/app/util/misc.util'

@Component({
  selector: 'backup-drives',
  templateUrl: './backup-drives.component.html',
  styleUrls: ['./backup-drives.component.scss'],
})
export class BackupDrivesComponent {
  @Input() type: 'backup' | 'restore'
  @Output() onSelect: EventEmitter<MappedPartitionInfo> = new EventEmitter()
  message: string

  constructor (
    public readonly backupService: BackupService,
  ) { }

  ngOnInit () {
    if (this.type === 'backup') {
      this.message = 'Select the drive where you want to create a backup of your Embassy.'
    } else {
      this.message = 'Select the drive containing backups you would like to restore.'
    }
    this.backupService.getExternalDrives()
  }

  handleSelect (partition: MappedPartitionInfo): void {
    this.onSelect.emit(partition)
  }
}


@Component({
  selector: 'backup-drives-header',
  templateUrl: './backup-drives-header.component.html',
  styleUrls: ['./backup-drives.component.scss'],
})
export class BackupDrivesHeaderComponent {
  @Input() title: string
  @Output() onClose: EventEmitter<void> = new EventEmitter()

  constructor (
    public readonly backupService: BackupService,
  ) { }

  refresh () {
    this.backupService.getExternalDrives()
  }
}
