import { Configuration } from '@alephium/cli'
import { Number256 } from '@alephium/web3'

// Settings are usually for configuring
export type Settings = {
  issueTokenAmount: Number256
  openaiAPIKey?: string
  ipfs?: {
    infura?: {
      projectId: string
      projectSecret: string
    }
  }
}

const defaultSettings: Settings = {
  issueTokenAmount: 100n,
  openaiAPIKey: process.env.OPENAI_API_KEY || '',
  ipfs: {
    infura: {
      projectId: process.env.IPFS_INFURA_PROJECT_ID || '',
      projectSecret: process.env.IPFS_INFURA_PROJECT_SECRET || ''
    }
  }
}

const configuration: Configuration<Settings> = {
  networks: {
    devnet: {
      nodeUrl: 'http://127.0.0.1:22973',
      // here we could configure which address groups to deploy the contract
      privateKeys: [],
      settings: defaultSettings
    },

    testnet: {
      nodeUrl: 'https://wallet-v20.testnet.alephium.org',
      privateKeys: [],
      settings: defaultSettings
    },

    mainnet: {
      nodeUrl: 'https://wallet-v20.mainnet.alephium.org',
      privateKeys: process.env.PRIVATE_KEYS === undefined ? [] : process.env.PRIVATE_KEYS.split(','),
      settings: defaultSettings
    }
  }
}

export default configuration
