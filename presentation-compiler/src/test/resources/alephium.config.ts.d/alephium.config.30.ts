import { Configuration } from '@alephium/cli'
import { Address } from '@alephium/web3'

export type Settings = {
  oracleAddress: Address
}

const configuration: Configuration<Settings> = {
  networks: {
    devnet: {
      nodeUrl: 'http://localhost:22973',
      privateKeys: ['a642942e67258589cd2b1822c631506632db5a12aabcf413604e785300d762a5'],
      settings: undefined as any
    },

    testnet: {
      nodeUrl: process.env.NODE_URL as string ?? 'https://node.testnet.alephium.org',
      privateKeys: process.env.PRIVATE_KEYS === undefined ? [] : process.env.PRIVATE_KEYS.split(','),
      settings: { oracleAddress: '216wgM3Xi5uBFYwwiw2T7iZoCy9vozPJ4XjToW74nQjbV' } // DIA oracle address on testnet
    },

    mainnet: {
      nodeUrl: process.env.NODE_URL as string ?? 'https://node.mainnet.alephium.org',
      privateKeys: process.env.PRIVATE_KEYS === undefined ? [] : process.env.PRIVATE_KEYS.split(','),
      settings: undefined as any
    }
  }
}

export default configuration
