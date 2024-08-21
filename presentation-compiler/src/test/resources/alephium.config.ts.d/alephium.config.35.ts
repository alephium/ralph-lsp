import { Configuration } from '@alephium/cli'
import { PrivateKeyWallet } from '@alephium/web3-wallet'
import * as dotenv from 'dotenv'

export type Settings = {}

dotenv.config()
const configuration: Configuration<Settings> = {
  networks: {
    testnet: {
      nodeUrl: process.env.NODE_URL ?? 'https://wallet.testnet.alephium.org',
      settings: {
      },
      privateKeys: process.env.PRIVKEY_TESTNET.split(',')  //to pass the test uncomment
    },
    mainnet:{
	   nodeUrl: process.env.NODE_URL ?? 'https://lb.notrustverify.ch',
	   settings: {
	   },
	   privateKeys: process.env.PRIVKEY_MAINNET.split(',')
	   
    }, 
	   
    devnet: {//Make sure the two values match what's in your devnet configuration
    nodeUrl: 'http://127.0.0.1:22973',
    settings: {
      privateKeys: []
    },
    privateKeys: process.env.npm_config_testing === 'true' ?  process.env.PRIVKEY_DEVNET_TEST.split(',') : process.env.PRIVKEY_DEVNET.split(',')  //to pass the test uncomment
    
  }
  }
}

export default configuration
