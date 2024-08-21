import { Configuration } from '@alephium/cli'
import { Number256 } from '@alephium/web3'

export type Settings = {
  fee: Number256
  admin: string
}

export const loadSettings = (network: 'devnet' | 'testnet' | 'mainnet'): Settings => {
  if (network === 'devnet') {
    return {
      fee: 70n,
      admin: '1EJCtZP3HZP5rDX5v2o32woqLTxp6GS4GoLQGpzVPQm6E'
    }
  } else if (network === 'testnet' || network === 'mainnet') {
    return {
      fee: process.env.FEE as Number256,
      admin: process.env.ADMIN_ADDRESS as string
    }
  } else {
    throw new Error('Invalid network')
  }
}

const configuration: Configuration<Settings> = {
  networks: {
    devnet: {
      nodeUrl: 'http://127.0.0.1:22973',
      privateKeys: ['a642942e67258589cd2b1822c631506632db5a12aabcf413604e785300d762a5'],
      settings: loadSettings('devnet')
    },

    testnet: {
      nodeUrl: process.env.NODE_URL as string,
      privateKeys:
        process.env.PRIVATE_KEYS === undefined
          ? ['42eeb5ee9a0921048354067a262ecd9cf4817d3d50a8a6cced1cd6e3e9d487db']
          : process.env.PRIVATE_KEYS.split(','),
      settings: loadSettings('testnet')
    },

    mainnet: {
      nodeUrl: process.env.NODE_URL as string,
      privateKeys: process.env.PRIVATE_KEYS === undefined ? [] : process.env.PRIVATE_KEYS.split(','),
      settings: loadSettings('mainnet')
    }
  }
}

export default configuration
