import { Configuration } from '@alephium/cli'

export type Settings = {}

const defaultSettings: Settings = {}

const configuration: Configuration<Settings> = {
  networks: {
    devnet: {
      nodeUrl: 'http://127.0.0.1:22973',
      privateKeys: [
        'a642942e67258589cd2b1822c631506632db5a12aabcf413604e785300d762a5' // group 0
      ],
      settings: defaultSettings
    },

    testnet: {
      nodeUrl: (process.env.NODE_URL as string) ?? 'https://wallet-v20.testnet.alephium.org',
      privateKeys: process.env.TEST_NET_PRIVATE_KEYS === undefined ? [] : process.env.TEST_NET_PRIVATE_KEYS.split(','),
      settings: defaultSettings
    },

    mainnet: {
      nodeUrl: (process.env.NODE_URL as string) ?? 'https://wallet-v20.mainnet.alephium.org',
      privateKeys: process.env.MAIN_NET_PRIVATE_KEYS === undefined ? [] : process.env.MAIN_NET_PRIVATE_KEYS.split(','),
      settings: defaultSettings
    }
  }
}

export default configuration
