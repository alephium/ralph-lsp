import { Configuration } from '@alephium/cli'

export type Settings = {}

const defaultSettings: Settings = {}

const configuration: Configuration = {
  networks: {
    devnet: {
      nodeUrl: 'http://127.0.0.1:22973',
      // default devnet private key (group 0)
      privateKeys: ['a642942e67258589cd2b1822c631506632db5a12aabcf413604e785300d762a5'],
      settings: defaultSettings,
    },
    testnet: {
      nodeUrl: process.env.NODE_URL as string,
      privateKeys: process.env.PRIVATE_KEYS === undefined ? [] : process.env.PRIVATE_KEYS.split(','),
      settings: defaultSettings,
    },
    mainnet: {
      nodeUrl: process.env.NODE_URL as string,
      privateKeys: process.env.PRIVATE_KEYS === undefined ? [] : process.env.PRIVATE_KEYS.split(','),
      settings: defaultSettings,
    },
  },
}

export default configuration
