import { Effects, PluginHostnameInfo } from '../types'

export type SetExportedUrls = (opts: { effects: Effects }) => Promise<void>
export type UpdateExportedUrls = (effects: Effects) => Promise<null>
export type SetupExportedUrls = (fn: SetExportedUrls) => UpdateExportedUrls

export const setupExportedUrls: SetupExportedUrls = (fn: SetExportedUrls) => {
  return (async (effects: Effects) => {
    const urls: PluginHostnameInfo[] = []
    await fn({
      effects: {
        ...effects,
        plugin: {
          ...effects.plugin,
          url: {
            ...effects.plugin.url,
            exportUrl: (params) => {
              urls.push(params.hostnameInfo)
              return effects.plugin.url.exportUrl(params)
            },
          },
        },
      },
    })
    await effects.plugin.url.clearUrls({ except: urls })
    return null
  }) as UpdateExportedUrls
}
