import { CB } from '@start9labs/start-sdk'

export const dropboxSpec = CB.Config.of({
  name: CB.Value.text({
    name: 'Name',
    description: 'A friendly name for this Dropbox target',
    placeholder: 'My Dropbox',
    required: { default: null },
  }),
  token: CB.Value.text({
    name: 'Access Token',
    description: 'The secret access token for your custom Dropbox app',
    required: { default: null },
    masked: true,
  }),
  path: CB.Value.text({
    name: 'Path',
    description: 'The fully qualified path to the backup directory',
    placeholder: 'e.g. /Desktop/my-folder',
    required: { default: null },
  }),
})

export const googleDriveSpec = CB.Config.of({
  name: CB.Value.text({
    name: 'Name',
    description: 'A friendly name for this Google Drive target',
    placeholder: 'My Google Drive',
    required: { default: null },
  }),
  path: CB.Value.text({
    name: 'Path',
    description: 'The fully qualified path to the backup directory',
    placeholder: 'e.g. /Desktop/my-folder',
    required: { default: null },
  }),
  key: CB.Value.file({
    name: 'Private Key File',
    description:
      'Your Google Drive service account private key file (.json file)',
    required: { default: null },
    extensions: ['json'],
  }),
})

export const cifsSpec = CB.Config.of({
  name: CB.Value.text({
    name: 'Name',
    description: 'A friendly name for this Network Folder',
    placeholder: 'My Network Folder',
    required: { default: null },
  }),
  hostname: CB.Value.text({
    name: 'Hostname',
    description:
      'The hostname of your target device on the Local Area Network.',
    warning: null,
    placeholder: `e.g. 'My Computer' OR 'my-computer.local'`,
    required: { default: null },
    patterns: [],
  }),
  path: CB.Value.text({
    name: 'Path',
    description: `On Windows, this is the fully qualified path to the shared folder, (e.g. /Desktop/my-folder).\n\n On Linux and Mac, this is the literal name of the shared folder (e.g. my-shared-folder).`,
    placeholder: 'e.g. my-shared-folder or /Desktop/my-folder',
    required: { default: null },
  }),
  username: CB.Value.text({
    name: 'Username',
    description: `On Linux, this is the samba username you created when sharing the folder.\n\n On Mac and Windows, this is the username of the user who is sharing the folder.`,
    required: { default: null },
    placeholder: 'My Network Folder',
  }),
  password: CB.Value.text({
    name: 'Password',
    description: `On Linux, this is the samba password you created when sharing the folder.\n\n On Mac and Windows, this is the password of the user who is sharing the folder.`,
    required: false,
    masked: true,
    placeholder: 'My Network Folder',
  }),
})

export const remoteBackupTargetSpec = CB.Config.of({
  type: CB.Value.union(
    {
      name: 'Target Type',
      required: { default: 'dropbox' },
    },
    CB.Variants.of({
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

export const diskBackupTargetSpec = CB.Config.of({
  name: CB.Value.text({
    name: 'Name',
    description: 'A friendly name for this physical target',
    placeholder: 'My Physical Target',
    required: { default: null },
  }),
  path: CB.Value.text({
    name: 'Path',
    description: 'The fully qualified path to the backup directory',
    placeholder: 'e.g. /Backups/my-folder',
    required: { default: null },
  }),
})
