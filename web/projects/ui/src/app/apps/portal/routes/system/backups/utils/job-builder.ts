import { BackupJob, BackupTarget, RR } from 'src/app/services/api/api.types'

export class BackupJobBuilder {
  name: string
  target: BackupTarget
  cron: string
  packageIds: string[]
  now = false

  constructor(readonly job: Partial<BackupJob>) {
    const { name, target, cron } = job
    this.name = name || ''
    this.target = target || ({} as BackupTarget)
    this.cron = cron || '0 2 * * *'
    this.packageIds = job.packageIds || []
  }

  buildCreate(): RR.CreateBackupJobReq {
    const { name, target, cron, now } = this

    return {
      name,
      targetId: target.id,
      cron,
      packageIds: this.packageIds,
      now,
    }
  }

  buildUpdate(id: string): RR.UpdateBackupJobReq {
    const { name, target, cron } = this

    return {
      id,
      name,
      targetId: target.id,
      cron,
      packageIds: this.packageIds,
    }
  }
}
