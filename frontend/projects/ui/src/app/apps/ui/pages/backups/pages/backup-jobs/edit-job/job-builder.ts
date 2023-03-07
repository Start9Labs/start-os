import { BackupJob, BackupTarget, RR } from 'src/app/services/api/api.types'

export class BackupJobBuilder {
  name: string
  target: BackupTarget
  cron: string
  'package-ids': string[]
  now = false

  constructor(readonly job: Partial<BackupJob>) {
    const { name, target, cron } = job
    this.name = name || ''
    this.target = target || ({} as BackupTarget)
    this.cron = cron || '0 2 * * *'
    this['package-ids'] = job['package-ids'] || []
  }

  buildCreate(): RR.CreateBackupJobReq {
    const { name, target, cron, now } = this

    return {
      name,
      'target-id': target.id,
      cron,
      'package-ids': this['package-ids'],
      now,
    }
  }

  buildUpdate(id: string): RR.UpdateBackupJobReq {
    const { name, target, cron } = this

    return {
      id,
      name,
      'target-id': target.id,
      cron,
      'package-ids': this['package-ids'],
    }
  }
}
