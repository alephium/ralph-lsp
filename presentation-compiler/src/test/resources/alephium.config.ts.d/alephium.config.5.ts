import { Configuration } from '@alephium/cli'
import { Address } from '@alephium/web3'
import { testAddress } from '@alephium/web3-test'
import 'dotenv/config'

export type Settings = {
  registrarOwner: Address
  primaryGroup: number
}

const configuration: Configuration<Settings> = {
  networks: {
    devnet: {
      nodeUrl: 'http://localhost:22973',
      privateKeys: [
        'a642942e67258589cd2b1822c631506632db5a12aabcf413604e785300d762a5',
        'ec8c4e863e4027d5217c382bfc67bd2638f21d6f956653505229f1d242123a9a',
        'bd7dd0c4abd3cf8ba2d169c8320a2cc8bc8ab583b0db9a32d4352d6f5b15d037',
        '93ae1392f36a592aca154ea14e51b791c248beaea1b63117c57cc46d56e5f482'
      ],
      settings: {
        registrarOwner: testAddress,
        primaryGroup: 0
      }
    },

    testnet: {
      nodeUrl: (process.env.NODE_URL as string) || 'https://node.testnet.alephium.org',
      privateKeys: process.env.PRIVATE_KEYS === undefined ? [] : process.env.PRIVATE_KEYS.split(','),
      settings: {
        registrarOwner: '',
        primaryGroup: 0
      }
    },

    mainnet: {
      nodeUrl: (process.env.NODE_URL as string) || 'https://node.mainnet.alephium.org',
      privateKeys: process.env.PRIVATE_KEYS === undefined ? [] : process.env.PRIVATE_KEYS.split(','),
      settings: {
        registrarOwner: '',
        primaryGroup: 0
      }
    }
  },

  compilerOptions: {
    errorOnWarnings: false,
    ignoreUnusedConstantsWarnings: true
  }
}

export default configuration
