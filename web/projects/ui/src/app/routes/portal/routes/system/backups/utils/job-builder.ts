import { BackupJob, BackupTarget, RR } from 'src/app/services/api/api.types'

export class BackupJobBuilder {
  name: string
  targetId: string
  cron: string
  packageIds: string[]
  now = false

  constructor(readonly job: Partial<BackupJob>) {
    const {
      name = '',
      targetId = '',
      cron = '0 2 * * *',
      packageIds = [],
    } = job

    this.name = name
    this.targetId = targetId
    this.cron = cron
    this.packageIds = packageIds
  }

  buildCreate(): RR.CreateBackupJobReq {
    const { name, targetId, cron, now, packageIds } = this

    return {
      name,
      targetId,
      cron,
      packageIds,
      now,
    }
  }

  buildUpdate(id: string): RR.UpdateBackupJobReq {
    const { name, targetId, cron, packageIds } = this

    return {
      id,
      name,
      targetId,
      cron,
      packageIds,
    }
  }
}
