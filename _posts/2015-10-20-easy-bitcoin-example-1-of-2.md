---
layout     : post
title      :  "EasyBitcoin example: Creating a command-line wallet.(1/2)"
date       : 2015-10-20 14:42:46

categories : 
          - haskell
          - easy-bitcoin

author     : "Alejandro Durán Pallarés"
comments   : true

---




Hi! In this post I'll introduce you `EasyBitcoin`, a simple bitcoin haskell library I wrote as a fork from `Haskoin`; Which is a 
great library, but after using it for a while I thought it would be fun adapting its API to something I would feel more comfortable 
with. The library provide types and instances related with transactions, keys, addresses and escrows, and pure functions to handle 
them as well, but it does not provide support for the protocol communication; that is, you still would need to connect to a bitcoin 
client ( or a 3º party service) if you want to interact with other peers. 
   

In order to present `EasyBitcoin`, I'll show how to write a simple command-line wallet using this library and a 3º party provider:

  1. First, I'll make a quick overview of the bitcoin protocol for those who might not be that familiar with it; just to
     understand the application, so if you know a bit about the protocol, you can just skip this section. 

  2. On the second [part](/haskell/easy-bitcoin/2015/10/21/easy-bitcoin-example-2-of-2.html), I'll introduce the actual code 
     for the application, the interesting stuff.


The library's documentation can be found [on hackage](www.example.com), and its source code [on github](www.example.com). If you find 
some trouble with it or have some suggestions I'd really apreciate your input; This is the first library I wrote, and to be honest, 
I still feel there's so much I need to learn from the haskell community.  


### A Brief introduction to Bitcoin:

Bitcoin is a P2P protocol where a set of peers communicate with each other to keep and modify a ledger of balances; these balances are 
measured using a unit called bitcoin (just like the protocol's name but without using capital letter), this unit can be divided up to 
8 digits, this way you can also find mili-bitcoin, micro-bitcoin...etc. The aim of Bitcoin is to provide a foundation for a better 
financial system: from an user point of view, it is sometimes hard to appreciate what are the current problems and limitations associated 
with the current payment/bank system, and that's why lot of people wonder why bitcoin are even needed. But I'm pretty sure those who have
 worked on the technical side of the financial sector would have quiet a different opinion, whether bitcoin is the best thing ever or 
 just fade, there's no doubt, the financial sector is investing each day more on technology looking for fresh innovation. (Actually the
sector has been many time compared with dinosaurs due to its size and its inability to find technical innovation to its problems).


#### Bitcoin transactions:

In order to keep this balance ledger, peers share a global distributed database of transactions, and modify it by pushing new transactions 
to it; though peers can only push and never directly remove(*), it is possible for some recently added transactions to get lost as 
peer synchronize their view of the global state (as you probably know, it is impossible for a distributed system to [be simultaneously 
consistent, available and tolerant to partitions](https://en.wikipedia.org/wiki/CAP_theorem) ). Some malicious peer can try to used
 this situation to temporally [fake payments](https://en.bitcoin.it/wiki/Double-spending#Race_attack). This global distributed database 
of transactions is the so called [blockchain](https://en.wikipedia.org/wiki/Block_chain_%28database%29).


Though conceptually seen similar to ordinary payment transactions, where an amount of money is withdrawn from an account A into an account 
B, the Bitcoin protocol lacks the concept of accounts; instead, each transaction is defined as a set of transaction-inputs representing 
payments received and transactions-outputs representing payments sent: Each transaction-output is defined as an amount to send and an 
"ownership challenge", each transaction-input is defined as an "ownership challenge solution" and a reference to a former transaction-output 
**not referenced before by another transaction-input from any other transaction**. To be a valid transaction, the sum of the inputs 
referenced sent amount shall be the same as the sum of the outputs sent amount (plus an small fee implicitly defined as the difference between 
the input minus the outputs).


The transaction issuer decide the implicit fee it has to pay, and could be any non-negative amount including zero. The higher the amount, 
the higher the chances the network will collaborate to include its transaction into the distributed database of transactions.

When a reference to a transaction-output has not been referenced yet by some transaction input, it is called **UTXO**(unspent 
transaction output). Within the *EasyBitcoin* library, references to transaction-outputs (whether they've been referenced or not) are 
called **Outpoint**.


Hypothetical bank account balance:

    +  43 € received from transaction '3169eb88-5b6c-465c-bbb9-d322a4626457' at 2012-04-23
    + 132 € received from transaction 'c4d51e23-1c96-40e7-9b10-8e6d278fbc5b' at 2013-07-13
    -  80 € sent       on transaction '74d31f58-7dad-470b-a03f-28f70ffce82d' at 2014-01-01
    -   1 € fee
    --------------------------------------------------------------------------------------
       94 € current balance hold as today  2015-10-15
  


An actual [bitcoin transaction](https://www.blocktrail.com/BTC/tx/ffe58cb9762dca10f085dcfc2549635a0dacf5653d71d3e9491e47780a05771c)
(id: **bd37285a0d42c0811b63b5942a3fd7b0d8ba292a135a2350b2bad460a16f17cf** ):

     input #0 (from transaction e1018f7fd13f3f410e5efac7cb26173f32494d8ceb3fce93ee995bd34fbf86fd 
                      output #0  10.55196835 BTC
              )
     input #1 (from transaction c34f64222fb4bf6630d90c15d82c94881bea944632c29116b74644b084caeb8d 
                      output #0 130.51303223 BTC
              )
     input #2 (from transaction edf1c527f933dd7d7e50afcc09951b68035bbaf4e29ecda1051ad1a6b7e404e8 
                      output #0  15.62078170 BTC
              )
     input #3 (from transaction 1c921420abd9243143a59f99d2c2e04f4d9e00178f2e1f6281df18ccf8a6d553 
                      output #2   1.59475184 BTC
              ) 
    ---------------------------------------------------------------------------------------------
     157.35400000 BTC output #0 
       0.88671799 BTC output #1
       0.03970000 BTC output #2
    ---------------------------------------------------------------------------------------------
       0.00011613 BTC implicit fee



#### Bitcoin ownership:

So, what does it means to own a certain amount of bitcoins? We say we own an specific amount of bitcoins if we can create a 
bitcoin transaction having an output with that specific amount as amount to send.

If somebody creates a transaction having an output with an "ownership challenge" we can solve, that means will be able to 
reference that output from our hypothetical transaction, and therefore increasing the amount of bitcoin we own.
Later, if we use an "ownership challenge solution" to create a another transaction referencing that output, as outputs can only be 
referenced at most once, it would mean we will no longer be able to use it, reducing the bitcoin amount we own.

And what are these "ownership challenge"s and "ownership challenge solution"s ? Both are pieces of programs using a custom language 
called [Script](https://en.bitcoin.it/wiki/Script), such when appended together yield **true**. The "ownership challenge" part is 
usually called **scriptSig** and the "ownership challenge solution" called **scriptPubKey**. Though this language is limited (not 
turing-compatible), is able to codify an important range of conditions, from the one anyone is able to solved, to one nobody is.


#### Bitcoin Addresses:

When I first started reading about bitcoin (the documentation back then was much sparse and poor) I thought I got it till I reached the 
**scriptSig**/**scriptPubKey** part, it was quiet a surprise, ¿what is an embedded language doing stuck there? I mean, it was not 
fair, it was not mention on [the original paper ](https://bitcoin.org/bitcoin.pdf) !!! Also, back then, I had not read anything that 
could make me think there was that degree of liberty to define for each output an specific behavior.

The reason is simple, the typical transaction, like the ones described on the paper, are just one of the many usage it is possible to 
configure using the Script; and though there are a lot of other possibilities, most of transactions work that way, following an specific 
template for the **scriptSig** and **scriptPubKey** where transaction outputs are linked to the hash digest of some 
[public key](https://en.wikipedia.org/wiki/Public-key_cryptography), in order to use them as input for a new transaction, the issuer will 
have to prove its ownership signing the transaction he wants to create with the private key related with those public keys (as the proof is 
part of the transaction itself, it is ignored when signed, otherwise it would lead to a recursive signature! you can see more details about 
how it is actually signed [here](http://bitcoin.stackexchange.com/questions/3374/how-to-redeem-a-basic-tx) )(**)

These particular types of transactions are called P2PKH (pay-to-public-key-hash), an example of how these transactions are used could be:

  1. A vendor generate a couple of public/private keys.

  2. The customer interested on a product/service ask the vendor for its public key.

  3. The vendor gives the customer the **hash** of his public key, and the amount he has to pay him.

  4. The customer create a transaction with an outpoint with that specific amount and an P2KH **scriptSig**  which can only be solved by 
     someone knowing the private key linked the public key from the received hash; as the only one knowing this is the vendor, this enforce 
     that only the vendor will be able to spent that transaction output.

  5. The customer spread this transaction through the network of peers participating in the bitcoin protocol; after sometime, eventually 
     this transaction gets included on the blockchain.

  6. The vendor detects the transaction has been included on the blockchain, that it contains the required amount and consider
     the payment done.


Bitcoin addresses are just an special plain text format (so they can be posted anywhere or used on QR codes) to represent these hashes, 
using redundancy and avoiding ambiguous characters (like 'l' and '1' or '0'  and 'O'). You can check how they are actually 
derivate [here](https://en.bitcoin.it/wiki/Technical_background_of_version_1_Bitcoin_addresses)(**).

Thanks to its redundancy, it is almost imposible to send a transaction to the wrong guy just because some typos while writing down the 
address (the errors will be detected and the transaction won't ever make through). That said, it is still quiet possible, 
to accidentally just copy&paste a whole different address, and in this case redundancy won't help you not to sending it to the wrong guy, 
so better check twice before sending a significant amount!.  



(*) That said, there other types of transactions, defining different  behaviors with interesting properties; these 
unconventional transactions, are sometimes combined into a protocol defining the so called 
[smart-contracts](https://en.wikipedia.org/wiki/Smart_contract). This is a incredible interesting topic, but there will be time 
to speak about it onn another post!! 

(**) Well, again, here we have simplified a bit, addresses can sometimes been used for different kind of transactions, those addresses 
would have a slightly different format.


#### Wallet:

Bitcoin wallets are either the program used by the final users to manage their bitcoins (that is, to issue and detects transactions) 
or the persistent data used by those programs. On the [next post](/haskell/easy-bitcoin/2015/10/20/easy-bitcoin-example-2-of-2.html),
I'll show you how we can implement a minimalistic command-line wallet using Haskell, you'll see, it is actually quiet easy!!

