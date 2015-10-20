---
layout     : post
title      :  "EasyBitcoin example: Creating a command-line wallet.(2/2)"
date       : 2015-10-21 14:42:46
categories : [haskell, easy-bitcoin]
author     : "Alejandro Durán Pallarés"
---


Hi! On the [last post](/haskell/easy-bitcoin/2015/10/20/easy-bitcoin-example-1-of-2.html) we spoke about doing a command-line 
bitcoin wallet using Haskell and the `EasyBitcoin` library; we have already introduced the protocol, so now it's time to get our 
hand dirty and start coding!






### Creating our own (simplified) wallet:

Our wallet will be rather simple, we'll rely on a 3º party provider to deal with the  p2p network communication, so we won't 
need to install any additional software :) . Most of the time, applications communicate with the bitcoin network through a 
bitcoin-client hosted by themselves, this way they don't need to rely anyone. In any case, from the code 
point of view, there's no much different between calling our own client or a 3º party: it is just about adapting to their 
exposed API.

So let's start importing the libraries, types and functions will need!

{% highlight haskell  %}


{-# LANGUAGE DataKinds, OverloadedStrings, MultiWayIf #-}


import Network.EasyBitcoin
        ( Address      -- ^ a bitcoin address.
        , address      -- ^ this function derivates addresses from Keys
        , BTC          -- ^ represent an amount of bitcoin
        
        , btc          -- ^ takes a Double and interprets it as btc 
                       --   (notice bitcoin are often expressed on subunits such mBtc or satoshis
                       --   , so it important to express which units are we using)
        
        , TestNet      -- ^ Instead of using normal bitcoin, we'll use
                       --   testnet bitcoin, which are similar but free, so we
                       --   can play with them without fearing losing actual money.

        , Key          -- ^ Represents cryptographic keys, it has 2 phantom types
                       --   to codify whether it is public or private, and in which
                       --   network (such testnet) they are supposed to be used

                       -- | We'll only use private keys. 
        , Visibility(Private)
        
        , Tx           -- ^ A bitcoin transaction.
        , Txid         -- ^ A bitcoin transaction identifier.
        , txid         -- ^ Computes the Txid from a Tx

        , Outpoint(..) -- ^ A Txid plus an index representing a reference to 
                        --   a transaction output.
        
        , transaction  -- ^ Given a list of reference to transaction outputs, the 
                       --   private keys to sign them and a list of addresses 
                       --   and amount where to send the btc's, it creates a
                       --   signed, ready to use, transaction. 
        )

{% endhighlight %}

Of course that would not be enough, we'll also need to read from command line, communicate with the 3º party using 
HTTP request and we'll need to parse and compose those requests.


{% highlight haskell  %}

import System.Environment  -- ^ To read from command line   
        ( getArgs
        )                        

import Network.Wreq        -- ^ To send HTTP requests.
        ( get,post
        , responseBody
        )      

import Data.Aeson          -- ^ To compose JSON values
        ( (.=),object
        )                       

import Data.Aeson.Lens     -- ^ To parse JSON values.
        ( key,values,_JSON
        )
import Control.Lens        -- ^ To make it easier parsing
        ( toListOf,_Just   --   JSON values 
        , (^?),to
        )         

{% endhighlight %}

And some common helpers:

{% highlight haskell  %}

import Safe(readMay)
import Control.Applicative((<$>))
import Data.List(sort)
import Data.Maybe(listToMaybe,fromMaybe)

{% endhighlight %}

We'll use a constant secret private key from which our receiving address will be derivate, and that we use to sign our outgoing
transaction.

{% highlight haskell  %}
secret :: Key Private TestNet
secret =  read "tprv8ZgxMBicQKsPcsbCVeqqF1KVdH7gwDJbxbzpCxDUsoXHdb6SnTPYxdwSAKYr9mrdtPfo3MkHsmViXxedm6MJD59TMuhj9vprD9UpGKnStwq"  

our_address = address secret 
{% endhighlight %}

Nowadays, most wallets generate a new address for each incoming transaction, this way we can track who has paid us and also it helps
enhancing the user's financial privacy. Usually it has a random secret root, and from this root they derivate new 
key pairs for each new address, following some [deterministic derivation](https://en.bitcoin.it/wiki/Deterministic_wallet) schema 
like the [BIP0032](https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki).

Though `EasyBitcoin` has special support for BIP0032, we'll keep things simple and use only one key and address.


#### Implementing main procedures:



So what should a wallet do? At least it should be able receive funds, check its current balance, and send funds to any valid
address; Let's write down these usage cases:

{% highlight haskell %}


main :: IO ()
main = do args <- getArgs
          case args of
           ["print_address"]                -> print_address
           ["check_balance"]                -> check_balance
           ["send_bitcoins", to_, amount_]
            | Just to     <- readMay to_
            , Just amount <- readMay amount_
            , amount > 0                    -> send_bitcoins (btc amount) to 
 
            | otherwise                     -> putStrLn "Address and/or amount to send could not be parsed."
           _                                -> putStrLn help
                       
  where
   help = unlines
         [ "Undefined action, correct syntax is:"
        , "   wallet check_balance : to check current balance."
        , "   wallet print_address : to print the address where to receive funds"
        , "   wallet send_bitcoins <address> <amount> :"++ 
               " to send the specific amount (measured in btc) to the specific address"
        ]

{% endhighlight %}

So now, we only need to implement `print_address`, `check_balance` and `send_bitcoins` and we'll be done. The first
of the 3 procedure looks rather difficult


{% highlight haskell %}

print_address = print our_address

{% endhighlight %}

Yes, it was even worse than expected!!

To compute our balance (`check_balance`), we'll ask the 3º party about the 
[UTXO](https://bitcoin.org/en/glossary/unspent-transaction-output) pointing to our address, and we'll sum them. We'll distinguish
those confirmed (with 1 or more confirmations) and those funds yet to be confirmed.

{% highlight haskell %}

check_balance:: IO ()
check_balance = do result <- readBlockExplorer
                   
                   let confirmed = sum [ btc | (True,_,btc) <- result ]
                       all_funds = sum [ btc | (_   ,_,btc) <- result ]
                   
                   putStrLn $ show all_funds ++ " BTCs ( " ++ show confirmed ++ " confirmed )"

{% endhighlight %}


Now, in order to implement `send_bitcoins`, we need to compose a transaction, and send it to the 3º party so it can broadcast it into 
the p2p bitcoin network. For a transaction to be valid, it needs to reference the same or more bitcoin from UTXO, than the one it is sending 
(otherwise we could send more btc than what we have). But before we should check:

1. We actually have enough bitcoins to send.

2. The amount to send is not to small... transactions with tiny outputs are considered spam and rejected by the network (If doesn't matter if
   we are sending 1€ million  and paying $1 million on commission, currently, it is just have a single output too small, it will be considered
   spam).

3. The difference between the btc's from the UTXO's and the one sending will be lost as a fee, if this amount is too big, we can solve this 
   problem adding another output back to ourself sending some of the remaining btc's.

4. But if the difference is not big enough, doing that would lead us to the problem we were trying to solve on point 2.


{% highlight haskell %}

send_bitcoins :: BTC TestNet -> Address TestNet -> IO ()
send_bitcoins to_send addr = 
    do  utxos           <- readBlockExplorer
        
        let (utxo',sending) = selectOutputs (to_send+fee) utxos
            remaining       = sending - to_send - fee

        if | remaining <  0         -> putStrLn "Not enough funds."

           | to_send   <  threshold -> putStrLn "Amount to send too small, considered dust."

           | remaining >= threshold -> send $ transaction [ (out,secret) | out <- utxo' ] (addr,to_send)
                                                          [(our_address,remaining)]

           | remaining <  threshold -> send $ transaction [ (out,secret) | out <- utxo' ] (addr,to_send)
                                                          []

{% endhighlight %}

We have forget to define some important parameters, ¿How much fee to pay?, ¿Where should be that threshold?
¿How do we select the UTXOs to use among all the available ones? For the first and second one we can check 
and hardcore the recommended values (they change as the network's load and bitcoin price evolves); But the 
second one is a bit more complicated: if we want to select the best combination of UTXOs such it minimize the
remaining returned funds (that will need around 10 extra minutes to get back confirmed), we'll face 
[the Knapsack problem](https://en.wikipedia.org/wiki/Knapsack_problem) which is NP-complete in the general 
case. 

So as simple criteria to select a UTXO, we'll use:

  1. We'll prefer confirmed over no confirmed.

  2. If equally confirmed, we'll arbitrarily prefer those with biggest Txid. (As Txid are derived from 
     hashes, this would be close to pick one randomly).


{% highlight haskell %}

threshold   = btc 0.0002
fee         = btc 0.0001


selectOutputs:: BTC net -> [(Bool,Outpoint,BTC net)] -> ([Outpoint],BTC net)
selectOutputs total = fromMaybe ([],0) 
                    . listToMaybe   
                    . dropWhile ((< total).snd)         
                    . scanl step ([],0)                
                    . reverse 
                    . sort    
      where
        step (outputs,total) (_,output,amount) = (output:outputs,amount+total) 

{% endhighlight %}



#### Implementing the requests:

As we mentioned, we used 3º party, in our case, a block explorer, particularly [blockr.io](blockr.io), but any one would work,
as they are most of them quiet similar. Actually, it would only take a few changes to adapt it so it can use our own bitcoind
client.


{% highlight haskell %}

server      = "https://tbtc.blockr.io/api/v1/" 


send :: Tx net -> IO () 
send tx = do post  resource (object [ "hex" .= show tx ])
             putStrLn $ "Transaction broadcasted, txid = " ++ show (txid tx) 
   where
    resource = server++"tx/push"



readBlockExplorer :: IO [(Bool,Outpoint,BTC TestNet)]
readBlockExplorer = toListOf extractUtxo <$> get resource

 where 
   extractUtxo       = responseBody.key "data".key "unspent".values.to parseOutpoint._Just


   resource          = server ++ "address/unspent/"++ show our_address ++ "?unconfirmed=1"

   parseOutpoint val = do txid      <- val ^? key "tx"            . _JSON . to readMay . _Just
                          n         <- val ^? key "n"             . _JSON
                          
                          -- | -- WARNING: amount is shown as String by Blockr.io
                          amount    <- val ^? key "amount"        . _JSON . to readMay . _Just 
                          
                          confirmed <- val ^? key "confirmations" . _JSON . to (>(0::Int))

                          return (confirmed,Outpoint txid n,btc amount)


{% endhighlight %}



#### Time to play:


So we have finally finished and our implementation is ready to be used! I already sent some tBTC so you can already start
making some transactions (unless someone has done them before you).

If you want to create your own version, don't forget to use a different `secret` (as It is posted on a blog I guess 
is not really a "secret"). The `EasyBitcoin` library accept both WIF and BIP0032 as format for its `Read` instance.

If you want to use the official bitcoin'd' client, just adapt procedure `send` and `readBlockExplorer` to use 
[its API](https://en.bitcoin.it/wiki/Original_Bitcoin_client/API_calls_list).

You can find the `EasyBitcoin` documentation on [hackage](www.example.com); or just fire me a comment and I'll try my best 
to help :) 




