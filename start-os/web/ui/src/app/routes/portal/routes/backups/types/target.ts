import { ISB } from '@start9labs/start-sdk'

export const dropboxSpec = ISB.InputSpec.of({
  name: ISB.Value.text({
    name: 'Name',
    description: 'A friendly name for this Dropbox target',
    placeholder: 'My Dropbox',
    required: true,
    default: null,
  }),
  token: ISB.Value.text({
    name: 'Access Token',
    description: 'The secret access token for your custom Dropbox app',
    required: true,
    default: null,
    masked: true,
  }),
  path: ISB.Value.text({
    name: 'Path',
    description: 'The fully qualified path to the backup directory',
    placeholder: 'e.g. /Desktop/my-folder',
    required: true,
    default: null,
  }),
})

export const googleDriveSpec = ISB.InputSpec.of({
  name: ISB.Value.text({
    name: 'Name',
    description: 'A friendly name for this Google Drive target',
    placeholder: 'My Google Drive',
    required: true,
    default: null,
  }),
  path: ISB.Value.text({
    name: 'Path',
    description: 'The fully qualified path to the backup directory',
    placeholder: 'e.g. /Desktop/my-folder',
    required: true,
    default: null,
  }),
  // @TODO Matt do we just drop file specs?
  // key: ISB.Value.file({
  //   name: 'Private Key File',
  //   description:
  //     'Your Google Drive service account private key file (.json file)',
  //   required: { default: null },
  //   extensions: ['json'],
  // }),
})

export const cifsSpec = ISB.InputSpec.of({
  name: ISB.Value.text({
    name: 'Name',
    description: 'A friendly name for this Network Folder',
    placeholder: 'My Network Folder',
    required: true,
    default: null,
  }),
  hostname: ISB.Value.text({
    name: 'Hostname',
    description:
      'The hostname of your target device on the Local Area Network.',
    warning: null,
    placeholder: `e.g. 'My Computer' OR 'my-computer.local'`,
    required: true,
    default: null,
    patterns: [],
  }),
  path: ISB.Value.text({
    name: 'Path',
    description: `On Windows, this is the fully qualified path to the shared folder, (e.g. /Desktop/my-folder).\n\n On Linux and Mac, this is the literal name of the shared folder (e.g. my-shared-folder).`,
    placeholder: 'e.g. my-shared-folder or /Desktop/my-folder',
    required: true,
    default: null,
  }),
  username: ISB.Value.text({
    name: 'Username',
    description: `On Linux, this is the samba username you created when sharing the folder.\n\n On Mac and Windows, this is the username of the user who is sharing the folder.`,
    required: true,
    default: null,
    placeholder: 'My Network Folder',
  }),
  password: ISB.Value.text({
    name: 'Password',
    description: `On Linux, this is the samba password you created when sharing the folder.\n\n On Mac and Windows, this is the password of the user who is sharing the folder.`,
    required: false,
    masked: true,
    default: null,
    placeholder: 'My Network Folder',
  }),
})

export const remoteBackupTargetSpec = ISB.InputSpec.of({
  type: ISB.Value.union(
    {
      name: 'Target Type',
      default: 'dropbox',
    },
    ISB.Variants.of({
      dropbox: {
        name: 'Dropbox',
        spec: dropboxSpec,
      },
      'google-drive': {
        name: 'Google Drive',
        spec: googleDriveSpec,
      },
      cifs: {
        name: 'Network Folder',
        spec: cifsSpec,
      },
    }),
  ),
})

export const diskBackupTargetSpec = ISB.InputSpec.of({
  name: ISB.Value.text({
    name: 'Name',
    description: 'A friendly name for this physical target',
    placeholder: 'My Physical Target',
    required: true,
    default: null,
  }),
  path: ISB.Value.text({
    name: 'Path',
    description: 'The fully qualified path to the backup directory',
    placeholder: 'e.g. /Backups/my-folder',
    required: true,
    default: null,
  }),
})
